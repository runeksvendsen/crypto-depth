module CryptoDepth.Fetch where


import           Prelude
import qualified CryptoDepth
import           CryptoVenues.Types.Market
import qualified CryptoVenues.Fetch.EnumMarkets    as EnumMarkets
import           CryptoVenues.Fetch.MarketBook     (fetchMarketBook)
import qualified CryptoVenues.Venues               as Venues
import qualified CryptoVenues.Types.AppM           as AppM

import           Data.Proxy                        (Proxy(..))
import           GHC.TypeLits                      (KnownSymbol, symbolVal)
import           Data.List                         ((\\))
import qualified Data.Text                         as T
import qualified Control.Monad.Parallel            as Par


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
   -> Venues.AnyVenue
   -> AppM.AppM IO [CryptoDepth.ABook]
fetchVenueBooks _ numObLimit (Venues.AnyVenue p) = do
    allMarkets :: [Market venue] <- EnumMarkets.marketList p
    -- Begin DEBUG stuff
    let btcEth = ["BTC", "ETH"]
        numeraire = T.pack $ symbolVal (Proxy :: Proxy numeraire)
        numeraireLst = filter (\mkt -> miBase mkt `elem` btcEth && miQuote mkt == numeraire) allMarkets
        markets = take (fromIntegral numObLimit - length numeraireLst) (allMarkets \\ numeraireLst)
        marketList = numeraireLst ++ markets
    -- End DEBUG stuff
    map CryptoDepth.toABook <$> mapM fetchMarketBook marketList

