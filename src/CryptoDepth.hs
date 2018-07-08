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
import Data.List ((\\))


-- | In which currency do we want to measure liquidity?
type Numeraire = "USD"

-- | Measure how much can be bought/sold while, at most, moving the price by this percentage
slippagePercent :: Rational
slippagePercent = 5 % 1

-- | Ignore the liquidity of these non-cryptos
nonCryptos :: [Paths.Sym]
nonCryptos = ["USD", "EUR", "GBP", "JPY"]

-- DEBUG: How many orderbooks to fetch from each venue
--  (not used in production)
numObLimit :: Int
numObLimit = 15

main :: AppM IO ()
main = do
    books <- allBooks
    let symVolume = getSymVolumes books slippagePercent
        symVolume :: [(Paths.Sym, Money.Dense Numeraire, Money.Dense Numeraire)]
        !outLines = map prettyPrint symVolume
    putStr initialLine
    putStr (unlines outLines)
    putStrLn delimiter
  where
    -- Output
    prettyPrint :: KnownSymbol numeraire
                => (Paths.Sym, Money.Dense numeraire, Money.Dense numeraire)
                -> String
    prettyPrint (nodeSym, buySum, sellSum) = printf formatStr
        (toS nodeSym :: String)
        (showDense buySum)
        (showDense sellSum)
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

-- | Get buy/sell volume, at the given slippage, for all cryptos in the 'ABook' list.
--  Ignores symbols in 'nonCryptos'.
getSymVolumes
    :: forall numeraire.
       KnownSymbol numeraire
    => [Paths.ABook]
    -> Rational
    -> [(Paths.Sym, Money.Dense numeraire, Money.Dense numeraire)]
getSymVolumes books slipPct =
    sortBy descSellVolume $ (map buySellSlips nodeSyms)
  where
    -- Sort by descending sell volume
    descSellVolume (_,_,s1) (_,_,s2) = s2 `compare` s1
    rateMap = Paths.buildRateMap books
    (depthGraph, nodeMap) = Paths.buildDepthGraph rateMap books
    nodeSyms = filter (not . (`elem` nonCryptos)) $ map fst (Map.toList nodeMap)
    throwErr = either (error . unlines) id
    buySellSlips nodeSym =
        let (buyVol, sellVol) = throwErr $ pathBuySellVol
                rateMap nodeMap depthGraph slipPct nodeSym
        in (nodeSym, buyVol, sellVol)

-- | Get buy/sell volume, at the given slippage, for the specified crypto
pathBuySellVol
    :: forall numeraire.
       KnownSymbol numeraire
    => Paths.USDRateMap
    -> Paths.NodeMap
    -> Paths.DepthGraph
    -> Rational                                                         -- ^ Slippage (in percent)
    -> Paths.Sym                                                        -- ^ Cryptocurrency symbol
    -> Either [String] (Money.Dense numeraire, Money.Dense numeraire)   -- ^ (buy,sell) volume
pathBuySellVol rateMap nodeMap depthGraph slipPct sym = do
    sellVol <- mkResult (pathVolumes sellFrom sellTo)
    buyVol <- mkResult (pathVolumes buyFrom buyTo)
    return (buyVol, sellVol)
  where
    (sellTo, sellFrom) = (lookupSymFail numeraire nodeMap, lookupSymFail sym nodeMap)
    (buyTo, buyFrom) = (lookupSymFail sym nodeMap, lookupSymFail numeraire nodeMap)
    numeraire = toS $ symbolVal (Proxy :: Proxy numeraire)
    -- Util
    pathVolumes :: KnownSymbol numeraire
                => G.Node
                -> G.Node
                -> [Either String (Money.Dense numeraire)]
    pathVolumes from to = map (Exchange.slippageExchangeMulti slipPct) $
        Paths.liquidPaths rateMap nodeMap depthGraph from to
    mkResult ps =
        if not . null . lefts $ ps
            then Left  . lefts $ ps
            else Right . sum . rights $ ps

showSomeDense :: Money.SomeDense -> String
showSomeDense = (`Money.withSomeDense` showDense)

showDense :: forall a. KnownSymbol a => Money.Dense a -> String
showDense = (++ " " ++ symbolVal (Proxy :: Proxy a))
                . toS . fromMaybe (error "denseToDecimal")
                . Money.denseToDecimal Money.Round False (Just ',') '.' 0 (1 % 1)

-- | Fetch books, in parallel, from all venues
allBooks :: AppM IO [Paths.ABook]
allBooks =
   concat <$> Par.forM Venues.allVenues fetchVenueBooks

-- | Fetch books from a single venue
--  DEBUG: limit number of fetched books to 'numObLimit'
fetchVenueBooks
   :: AnyVenue
   -> AppM IO [Paths.ABook]
fetchVenueBooks (AnyVenue p) = do
    allMarkets :: [Market venue] <- EnumMarkets.marketList p
    let marketName = symbolVal (Proxy :: Proxy venue)
        toABook (AnyBook ob) = Paths.ABook ob
    putStrLn (printf "%s: %d markets" marketName (length allMarkets) :: String)
    -- Begin DEBUG stuff
    let numeraire = toS $ symbolVal (Proxy :: Proxy Numeraire)
        numeraireLst = filter (\mkt -> miBase mkt == numeraire || miQuote mkt == numeraire) allMarkets
        markets = take (numObLimit - length numeraireLst) (allMarkets \\ numeraireLst)
        marketList = numeraireLst ++ markets
    -- End DEBUG stuff
    map toABook <$> Throttle.fetchRateLimited marketList
