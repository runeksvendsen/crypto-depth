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
maxRetries = 10

main :: IO ()
main = Log.withStderrLogging $ do
   Log.setLogLevel logLevel
   Log.setLogTimeFormat "%T.%3q"
   man <- HTTP.newManager HTTPS.tlsManagerSettings -- { HTTP.managerModifyRequest = logRequest }
   either (error . show) return =<< AppM.runAppM man maxRetries CryptoDepth.main

logRequest :: HTTP.Request -> IO HTTP.Request
logRequest req = do
    putStrLn $ "### " ++ toS logStr
    return req
  where
    logStr = HTTP.host req <> HTTP.path req <> HTTP.queryString req
