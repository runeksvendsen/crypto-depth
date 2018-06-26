module Main where

import CryptoDepth.Internal.DPrelude
import qualified CryptoVenues.Venues
import qualified CryptoVenues.Fetch.Throttle as Throttle
import CryptoVenues.Types.Market
import CryptoVenues.Fetch
import qualified Network.HTTP.Client   as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified CryptoVenues.Internal.Log
import Control.Monad
import qualified Control.Monad.Parallel   as Par
import qualified Control.Logging as Log

import qualified CryptoDepth
import qualified CryptoVenues.Types.AppM as AppM

logLevel = Log.LevelDebug
numMarkets = 30

logRequest :: HTTP.Request -> IO HTTP.Request
logRequest req = do
    putStrLn $ "### " ++ toS logStr
    return req
  where
    logStr = HTTP.host req <> HTTP.path req <> HTTP.queryString req

main = Log.withStderrLogging $ do
   Log.setLogLevel logLevel
   Log.setLogTimeFormat "%T:%3q"
   man <- HTTP.newManager HTTPS.tlsManagerSettings -- { HTTP.managerModifyRequest = logRequest }
   either (error . show) return =<< AppM.runAppM man CryptoDepth.main


-- forVenues :: HTTP.Manager
--           -> (AnyVenue -> IO a)
--           -> IO ()
-- forVenues man = Par.forM_ Venues.allVenues

-- testVenue :: HTTP.Manager -> AnyVenue -> IO ()
-- testVenue man av@(AnyVenue venue) =
--    withMarketList man venue
--       (void . failOnErr <=< runAppM man . Throttle.fetchRateLimited . take numMarkets)

-- withMarketList
--    :: forall venue a. EnumMarkets venue
--    => HTTP.Manager
--    -> Proxy venue
--    -> ([Market venue] -> IO a) -> IO a
-- withMarketList man venue f = do
--    markets <- failOnErr =<< runAppM man (marketList venue)
--    f markets

-- failOnErr :: (Monad m, Show e) => Either e a -> m a
-- failOnErr = either (error . show) return