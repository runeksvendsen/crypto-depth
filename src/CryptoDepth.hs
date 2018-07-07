{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module CryptoDepth where

import Prelude (lines, unlines)
import CryptoDepth.Internal.DPrelude
import CryptoDepth.Internal.Util
import qualified CryptoDepth.Exchange as Exchange
import qualified CryptoDepth.Paths as Paths
import OrderBook.Types              (AnyBook(..))
import CryptoVenues.Types.Market
import CryptoVenues.Fetch.MarketBook
import CryptoVenues.Types.AppM
import qualified CryptoVenues.Fetch.Throttle as Throttle
import qualified CryptoVenues.Fetch.EnumMarkets as EnumMarkets
import qualified CryptoVenues.Venues as Venues
import qualified Data.Graph.Inductive.Graph as G
import qualified Control.Monad.Parallel   as Par
import qualified Data.HashMap.Strict as Map
import qualified Money


-- How many orderbooks to fetch from each venue
numObLimit :: Int
numObLimit = 15

nonCryptos :: [Paths.Sym]
nonCryptos = ["USD", "EUR", "GBP", "JPY"]

main :: AppM IO ()
main = do
    books <- allBooks
    putStr initialLine
    let symVolume = getSymVolume books numeraire slippagePercent
    forM_ symVolume (putStrLn . prettyPrint)
    putStrLn delimiter
  where
    numeraire = "USD"
    slippagePercent = 5 % 1
    -- Output
    prettyPrint :: (Paths.Sym, Money.SomeDense, Money.SomeDense) -> String
    prettyPrint (nodeSym, buySum, sellSum) = printf formatStr
        (toS nodeSym :: String)
        (showSomeDense buySum)
        (showSomeDense sellSum)
    formatStr = "%s\t%17s\t%17s"
    delimiter = "-------------------------------------------------"
    initialLine = unlines
                  [ delimiter
                  , printf formatStr
                        ("sym" :: String)
                        ("buy_volume" :: String)
                        ("sell_volume" :: String)
                  , delimiter
                  ]

getSymVolume
    :: [Paths.ABook]
    -> Paths.Sym
    -> Rational
    -> [(Paths.Sym, Money.SomeDense, Money.SomeDense)]
getSymVolume books numeraire slipPct =
    reverse $ sortOn sellVolume $ (map buySellSlips nodeSyms)
  where
    sellVolume (_,_,s) = s
    rateMap = Paths.buildRateMap books
    (depthGraph, nodeMap) = Paths.buildDepthGraph rateMap books
    nodeSyms = filter (not . (`elem` nonCryptos)) $ map fst (Map.toList nodeMap)
    throwErr = either (error . unlines) id
    buySellSlips nodeSym = case someSymbolVal (toS numeraire) of
        SomeSymbol (_ :: Proxy dst) ->
            let (buyVol, sellVol) = throwErr $ pathBuySellVol
                    rateMap nodeMap depthGraph slipPct numeraire nodeSym
            in (nodeSym, buyVol, sellVol)

pathBuySellVol
    :: Paths.USDRateMap
    -> Paths.NodeMap
    -> Paths.DepthGraph
    -> Rational                                             -- ^ Slippage (in percent)
    -> Paths.Sym                                            -- ^ Numeraire
    -> Paths.Sym                                            -- ^ Cryptocurrency symbol
    -> Either [String] (Money.SomeDense, Money.SomeDense)   -- ^ (buy,sell) volume
pathBuySellVol rateMap nodeMap depthGraph slipPct numeraire sym =
    case someSymbolVal (toS numeraire) of
        SomeSymbol (_ :: Proxy dst) -> do
            sellVol <- mkResult (pathVolumes sellFrom sellTo :: [Either String (Money.Dense dst)])
            buyVol <- mkResult (pathVolumes buyFrom buyTo :: [Either String (Money.Dense dst)])
            return (buyVol, sellVol)
  where
    (sellTo, sellFrom) = (lookupSymFail numeraire nodeMap, lookupSymFail sym nodeMap)
    (buyTo, buyFrom) = (lookupSymFail sym nodeMap, lookupSymFail numeraire nodeMap)
    -- Util
    pathVolumes :: KnownSymbol dst => G.Node -> G.Node -> [Either String (Money.Dense dst)]
    pathVolumes from to = map (Exchange.slippageExchangeMulti slipPct) $
        Paths.liquidPaths rateMap nodeMap depthGraph from to
    mkResult ps =
        if not . null . lefts $ ps
            then Left  . lefts $ ps
            else Right . Money.toSomeDense . sum . rights $ ps


showSomeDense :: Money.SomeDense -> String
showSomeDense sd = Money.withSomeDense sd showDense

showDense :: forall a. KnownSymbol a => Money.Dense a -> String
showDense = (++ " " ++ symbolVal (Proxy :: Proxy a))
                . toS . fromMaybe (error "denseToDecimal")
                . Money.denseToDecimal Money.Round False (Just ',') '.' 0 (1 % 1)

allBooks :: AppM IO [Paths.ABook]
allBooks =
   concat <$> Par.forM Venues.allVenues fetchVenueBooks

fetchVenueBooks
   :: AnyVenue
   -> AppM IO [Paths.ABook]
fetchVenueBooks (AnyVenue p) = do
    mktList :: [Market venue] <- EnumMarkets.marketList p
    let marketName = symbolVal (Proxy :: Proxy venue)
    putStrLn (printf "%s: %d markets" marketName (length mktList) :: String)
    let btcUsdL = filter (\mkt -> miBase mkt == "BTC" && miQuote mkt == "USD") mktList
        markets = take numObLimit mktList
        marketList = case btcUsdL of
            []       -> markets
            [btcUsd] -> btcUsd : filter (/= btcUsd) markets
            _ -> error $ marketName ++ ": multiple BTC/USD markets"
        toABook (AnyBook ob) = Paths.ABook ob
    map toABook <$> Throttle.fetchRateLimited marketList