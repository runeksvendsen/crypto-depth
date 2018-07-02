{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module CryptoDepth where

import CryptoDepth.Internal.DPrelude
import CryptoDepth.Internal.Util
import qualified CryptoDepth.Paths as Paths
import CryptoVenues.Types.Market
import CryptoVenues.Fetch.MarketBook
import CryptoVenues.Types.AppM
import qualified CryptoVenues.Fetch.Throttle as Throttle
import qualified CryptoVenues.Fetch.EnumMarkets as EnumMarkets
import qualified CryptoVenues.Venues as Venues
import qualified Control.Monad.Parallel   as Par
import qualified Data.HashMap.Strict as Map


main :: AppM IO ()
main = do
    books <- allBooks
    let rateMap = Paths.buildRateMap books
        (depthGraph, nodeMap) = Paths.buildDepthGraph rateMap books
        (btcNode, usdNode) = (lookupSymFail "BTC" nodeMap, lookupSymFail "USD" nodeMap)
    print $ sortOn fst $ map swap (Map.toList nodeMap)
    putStrLn ("###### liquidPaths ######" :: String)
    mapM_ print $ take 25 $ 
        Paths.liquidPaths rateMap nodeMap depthGraph btcNode usdNode


allBooks :: AppM IO [Paths.ABook]
allBooks =
   concat <$> Par.forM Venues.allVenues fetchVenueBooks

fetchVenueBooks
   :: AnyVenue
   -> AppM IO [Paths.ABook]
fetchVenueBooks (AnyVenue p) = do
    mktList :: [Market venue] <- EnumMarkets.marketList p
    let btcUsdL = filter (\mkt -> miBase mkt == "BTC" && miQuote mkt == "USD") mktList
        marketName = symbolVal (Proxy :: Proxy venue)
        markets = take 30 mktList
        marketList = case btcUsdL of
            []       -> markets
            [btcUsd] -> btcUsd : filter (/= btcUsd) markets
            _ -> error $ marketName ++ ": multiple BTC/USD markets"
    when (null btcUsdL) $
        putStrLn $ marketName ++ ": no BTCUSD market"
    map Paths.ABook <$> Throttle.fetchRateLimited marketList
