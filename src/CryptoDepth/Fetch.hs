module CryptoDepth.Fetch where


import Prelude
import Data.Proxy (Proxy(..))
import Data.Ratio   ((%))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Printf (printf)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T

import qualified CryptoDepth
import qualified CryptoDepth.Output.CLI as CLI
import qualified CryptoDepth.Output.HTML as HTML

import OrderBook.Types              (AnyBook(..))
import CryptoVenues.Types.Market
import CryptoVenues.Fetch.MarketBook
import qualified CryptoVenues.Fetch.EnumMarkets as EnumMarkets
import qualified CryptoVenues.Venues as Venues
import qualified CryptoVenues.Types.AppM as AppM

import qualified Network.HTTP.Client   as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Control.Logging as Log
import qualified Control.Monad.Parallel   as Par
import Data.List ((\\))


-- | Fetch books, in parallel, from all venues
allBooks
    :: KnownSymbol numeraire
    => Proxy numeraire
    -> Word
    -> AppM.AppM IO [CryptoDepth.ABook]
allBooks p numObLimit =
   concat <$> Par.forM Venues.allVenues (fetchVenueBooks p numObLimit)

-- | Fetch books from a single venue
--  DEBUG: limit number of fetched books to 'numObLimit'
fetchVenueBooks
   :: forall numeraire.
      (KnownSymbol numeraire)
   => Proxy numeraire
   -> Word
   -> AnyVenue
   -> AppM.AppM IO [CryptoDepth.ABook]
fetchVenueBooks _ numObLimit (AnyVenue p) = do
    allMarkets :: [Market venue] <- EnumMarkets.marketList p
    let marketName = symbolVal (Proxy :: Proxy venue)
    lift . Log.log' $ T.pack (printf "%s: %d markets" marketName (length allMarkets) :: String)
    -- Begin DEBUG stuff
    let btcEth = ["BTC", "ETH"]
        numeraire = T.pack $ symbolVal (Proxy :: Proxy numeraire)
        numeraireLst = filter (\mkt -> miBase mkt `elem` btcEth && miQuote mkt == numeraire) allMarkets
        markets = take (fromIntegral numObLimit - length numeraireLst) (allMarkets \\ numeraireLst)
        marketList = numeraireLst ++ markets
    -- End DEBUG stuff
    map CryptoDepth.toABook <$> mapM fetchMarketBook marketList

