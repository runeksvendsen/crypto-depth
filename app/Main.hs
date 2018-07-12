module Main where

import CryptoDepth.Internal.DPrelude
import qualified CryptoDepth
import qualified CryptoDepth.Paths as Paths
import qualified CryptoDepth.Output.CLI as CLI
import qualified CryptoDepth.Output.HTML as HTML

import OrderBook.Types              (AnyBook(..))
import CryptoVenues.Types.Market
import CryptoVenues.Fetch.MarketBook
import qualified CryptoVenues.Fetch.Throttle as Throttle
import qualified CryptoVenues.Fetch.EnumMarkets as EnumMarkets
import qualified CryptoVenues.Venues as Venues
import qualified CryptoVenues.Types.AppM as AppM

import qualified Network.HTTP.Client   as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Control.Logging as Log
import qualified Control.Monad.Parallel   as Par
import qualified Money
import Data.List ((\\))


-- | In which currency do we want to measure liquidity?
type Numeraire = "USD"
type NumrDense = Money.Dense Numeraire

-- DEBUG: How many orderbooks to fetch from each venue
--  (not used in production)
numObLimit :: Int
numObLimit = 15

slippagePercent :: Rational
slippagePercent = 5 % 1


logLevel = Log.LevelDebug
maxRetries = 10

main :: IO ()
main = withLogging $ do
    man <- HTTP.newManager HTTPS.tlsManagerSettings -- { HTTP.managerModifyRequest = logRequest }
    let throwErr ioA = ioA >>= either (error . show) return
    res <- throwErr $ AppM.runAppM man maxRetries $
        CryptoDepth.getSymVolumes slippagePercent <$> allBooks
    HTML.htmlOut (res :: [(Paths.Sym, NumrDense, NumrDense)])

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T.%3q"
    ioa

-- | Fetch books, in parallel, from all venues
allBooks :: AppM.AppM IO [Paths.ABook]
allBooks =
   concat <$> Par.forM Venues.allVenues fetchVenueBooks

-- | Fetch books from a single venue
--  DEBUG: limit number of fetched books to 'numObLimit'
fetchVenueBooks
   :: AnyVenue
   -> AppM.AppM IO [Paths.ABook]
fetchVenueBooks (AnyVenue p) = do
    allMarkets :: [Market venue] <- EnumMarkets.marketList p
    let marketName = symbolVal (Proxy :: Proxy venue)
        toABook (AnyBook ob) = Paths.ABook ob
    lift . Log.log' $ toS (printf "%s: %d markets" marketName (length allMarkets) :: String)
    -- Begin DEBUG stuff
    let btcEth = ["BTC", "ETH"]
        numeraire = toS $ symbolVal (Proxy :: Proxy Numeraire)
        numeraireLst = filter (\mkt -> miBase mkt `elem` btcEth && miQuote mkt == numeraire) allMarkets
        markets = take (numObLimit - length numeraireLst) (allMarkets \\ numeraireLst)
        marketList = numeraireLst ++ markets
    -- End DEBUG stuff
    map toABook <$> Throttle.fetchRateLimited marketList
