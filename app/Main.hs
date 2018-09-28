module Main where

import Prelude
import Data.Proxy (Proxy(..))
import Data.Ratio   ((%))
import GHC.TypeLits (symbolVal)
import Text.Printf (printf)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T

import qualified CryptoDepth
import qualified CryptoDepth.Fetch as Fetch
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


-- | In which currency do we want to measure liquidity?
type Numeraire = "USD"
-- | A slippage of one twentieth is 5%
type Slippage = CryptoDepth.OneDiv 20
-- | NB: We use the same slippage for edge weights and pathInfos, but we don't have to
type Graph = CryptoDepth.DepthGraph Numeraire Slippage
type PathInfoNumr = CryptoDepth.PathInfo Numeraire Slippage

-- DEBUG: How many orderbooks to fetch from each venue
--  (not used in production)
numObLimit :: Word
numObLimit = 30

logLevel = Log.LevelDebug
maxRetries = 10

main :: IO ()
main = withLogging $ do
    man <- HTTP.newManager HTTPS.tlsManagerSettings -- { HTTP.managerModifyRequest = logRequest }
    let throwErrM ioA = ioA >>= either (error . show) return
    resMap <- throwErrM $ AppM.runAppM man maxRetries $ do
        books <- Fetch.allBooks (Proxy :: Proxy Numeraire) numObLimit
        let (graph, rateMap, nodeMap) = CryptoDepth.buildDepthGraph books
        return $ CryptoDepth.symLiquidPaths rateMap nodeMap (graph :: Graph)
    let res :: CryptoDepth.Map CryptoDepth.Sym ([PathInfoNumr], [PathInfoNumr]) =
            CryptoDepth.allPathsInfos resMap
    HTML.htmlOut res

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T.%3q"
    ioa
