{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module CryptoDepth
( -- * Graph building
  Paths.DepthGraph
, buildDepthGraph
  -- * Graph queries (buy/sell paths for some cryptocurrency)
, symLiquidPaths
, LiquidPaths(..)
  -- * Path queries (liquidity of buy/sell paths)
, allPathsInfos
, Exchange.PathInfo(..)
  -- * Helper functions
, totals
, groupVenues
  -- * Related types/classes
, ABook(ABook), toABook
, Sym
, Map
, ExchangePath(..)
, module Amount
, SymPathInfo
, OneDiv
, KnownFraction(..)
, fracValPercent
  -- * EdgePath-related
, Paths.EdgePath
, Paths.srcSym
, Paths.dstSym
, Paths.pathDescr
, Exchange.pathQty
  -- * Re-exports
, Exchange.Tagged(..)
)
where

import CryptoDepth.Internal.DPrelude    hiding ((<>))
import CryptoDepth.Internal.Types
import CryptoDepth.Internal.Util
import CryptoDepth.Internal.Types.Amount as Amount
import           CryptoDepth.Exchange (PathInfo(..))
import qualified CryptoDepth.Exchange as Exchange
import qualified CryptoDepth.Paths as Paths
import qualified CryptoDepth.RateMap as Rate
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.HashMap.Strict as Map
import qualified Money
-- For Semigroup/Monoid instance
import           Data.Semigroup         (Semigroup(..))
import           Data.Monoid            (Monoid)


type SymPathInfo numeraire slippage = Map Sym ([PathInfo numeraire slippage], [PathInfo numeraire slippage])

-- | Ignore the liquidity of these non-cryptos
-- TODO: Is there a better way to enumerate all non-cryptos?
nonCryptos :: [Sym]
nonCryptos = ["USD", "EUR", "GBP", "JPY"]

totals
    :: KnownSymbol numeraire
    => Map Sym ([PathInfo numeraire slippage], [PathInfo numeraire slippage])
    -> [(Sym, Exchange.Amount numeraire, Exchange.Amount numeraire)]
totals =
    sortBy descSellVolume . map total . Map.toList
  where
    -- Sort by descending sell volume
    descSellVolume (_,_,s1) (_,_,s2) = s2 `compare` s1
    total (sym, (buy, sell)) =
        (sym, Exchange.piTotalQty buy, Exchange.piTotalQty sell)

allPathsInfos
    :: (KnownSymbol numeraire, KnownFraction slippageEdgeWeight, KnownFraction slippage)
    => Map Sym (LiquidPaths numeraire slippageEdgeWeight)
    -> Map Sym ([PathInfo numeraire slippage], [PathInfo numeraire slippage])
allPathsInfos lpMap = throwBug $
    Map.fromList <$> traverse pathInfos (Map.toList lpMap)

pathInfos
    :: (KnownSymbol numeraire, KnownFraction slippageEdgeWeight, KnownFraction slippage)
    => (Sym, LiquidPaths numeraire slippageEdgeWeight)
    -> Either String (Sym, ([PathInfo numeraire slippage], [PathInfo numeraire slippage]))
pathInfos (mapSym, LiquidPaths buyPaths sellPaths) = do
    buyVol  <- traverse (exchange mapSym) buyPaths
    sellVol <- traverse (exchange mapSym) sellPaths
    return (mapSym, (buyVol, sellVol))
  where
    exchange targetSym path = do
        (pathSym, pathVol) <- Exchange.slippageExchangeMulti path
        if pathSym /= targetSym
            then Left $ printf "pathSym (%s) /= targetSym (%s): %s" pathSym targetSym (show path)
            else Right pathVol

buildDepthGraph
    :: (KnownSymbol numeraire, KnownFraction slippageEdgeWeight)
    => [Paths.ABook]
    -> (Paths.DepthGraph numeraire slippageEdgeWeight, Rate.RateMap numeraire, Paths.NodeMap)
buildDepthGraph books =
    (graph, rateMap, nodeMap)
  where
    (graph, nodeMap) = Paths.buildDepthGraph rateMap books
    !rateMap = Rate.buildRateMap books

-- | Get most liquid paths for all cryptos in the 'ABook' list.
--  Ignores symbols in 'nonCryptos'.
symLiquidPaths
    :: forall numeraire slippageEdgeWeight.
       (KnownSymbol numeraire, KnownFraction slippageEdgeWeight)
    => Rate.RateMap numeraire
    -> Paths.NodeMap
    -> Paths.DepthGraph numeraire slippageEdgeWeight
    -> Map Sym (LiquidPaths numeraire slippageEdgeWeight)  -- ^ For each crypto, all the paths that the given crypto can be bought/sold through
symLiquidPaths rateMap nodeMap depthGraph =
    foldr (uncurry Map.insert) Map.empty $ map buySellSlips nodeSyms
  where
    nodeSyms = filter (not . (`elem` nonCryptos)) $ map fst (Map.toList nodeMap)
    buySellSlips nodeSym =
        let pathRes = buySellPath rateMap nodeMap depthGraph nodeSym
        in (nodeSym, pathRes)

-- | The most liquid paths from some cryptocurrency to "numeraire",
--    in both buy and sell direction.
--   The paths are ordered in descending order of
--    liquidity, as measured by the volume that can be bought/sold
--    at the specified "slippage".
data LiquidPaths (numeraire :: Symbol) slippage =
    LiquidPaths
    { lpBuy  :: [Paths.EdgePath numeraire]
    , lpSell :: [Paths.EdgePath numeraire]
    }

instance Monoid (LiquidPaths numeraire slippage) where
    mempty = LiquidPaths [] []
    mappend = (<>)

instance Semigroup (LiquidPaths numeraire slippage) where
    LiquidPaths buys1 sells1 <> LiquidPaths buys2 sells2 =
        LiquidPaths (buys1 <> buys2) (sells1 <> sells2)

-- | Get buy and sell paths -- in descending order of liquidity at the given slippage --
--    for the specified cryptocurrency
buySellPath
    :: forall numeraire slippageEdgeWeight.
       (KnownSymbol numeraire, KnownFraction slippageEdgeWeight)
    => Rate.RateMap numeraire
    -> Paths.NodeMap
    -> Paths.DepthGraph numeraire slippageEdgeWeight
    -> Sym                                                              -- ^ Cryptocurrency symbol
    -> LiquidPaths numeraire slippageEdgeWeight
buySellPath rateMap nodeMap depthGraph sym =
    LiquidPaths buyPaths sellPaths
  where
    sellPaths = pathVolumes sellFrom sellTo
    buyPaths = pathVolumes buyFrom buyTo
    (sellTo, sellFrom) = (lookupSymFail numeraire nodeMap, lookupSymFail sym nodeMap)
    (buyTo, buyFrom) = (lookupSymFail sym nodeMap, lookupSymFail numeraire nodeMap)
    numeraire = toS $ symbolVal (Proxy :: Proxy numeraire)
    -- Util
    pathVolumes :: G.Node
                -> G.Node
                -> [Paths.EdgePath numeraire]
    pathVolumes from to =
        Paths.liquidPaths rateMap nodeMap depthGraph from to
