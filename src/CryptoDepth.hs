{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module CryptoDepth
( symLiquidPaths
, allPathsInfos
, totals
, LiquidPaths(..)
, Exchange.PathInfo(..)
, groupVenues
, Sym
)
where

import CryptoDepth.Internal.DPrelude
import CryptoDepth.Internal.Types
import CryptoDepth.Internal.Util
import           CryptoDepth.Exchange (PathInfo(..))
import qualified CryptoDepth.Exchange as Exchange
import qualified CryptoDepth.Paths as Paths
import qualified CryptoDepth.RateMap as Rate
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.HashMap.Strict as Map
import qualified Money


-- | Ignore the liquidity of these non-cryptos
nonCryptos :: [Sym]
nonCryptos = ["USD", "EUR", "GBP", "JPY"]

totals
    :: KnownSymbol numeraire
    => Map Sym ([PathInfo numeraire], [PathInfo numeraire])
    -> [(Sym, Money.Dense numeraire, Money.Dense numeraire)]
totals =
    sortBy descSellVolume . map total . Map.toList
  where
    -- Sort by descending sell volume
    descSellVolume (_,_,s1) (_,_,s2) = s2 `compare` s1
    total (sym, (buy, sell)) =
        (sym, Exchange.piTotalQty buy, Exchange.piTotalQty sell)

allPathsInfos
    :: KnownSymbol numeraire
    => Rational
    -> Map Sym (LiquidPaths numeraire)
    -> Either String (Map Sym ([PathInfo numeraire], [PathInfo numeraire]))
allPathsInfos slipPct lpMap =
    Map.fromList <$> traverse (pathInfos slipPct) (Map.toList lpMap)

pathInfos
    :: KnownSymbol numeraire
    => Rational
    -> (Sym, LiquidPaths numeraire)
    -> Either String (Sym, ([PathInfo numeraire], [PathInfo numeraire]))
pathInfos slipPct (mapSym, LiquidPaths buyPaths sellPaths) = do
    buyVol  <- traverse (exchange mapSym) buyPaths
    sellVol <- traverse (exchange mapSym) sellPaths
    return (mapSym, (buyVol, sellVol))
  where
    exchange targetSym path = do
        (pathSym, pathVol) <- Exchange.slippageExchangeMulti slipPct path
        if pathSym /= targetSym
            then Left $ printf "pathSym (%s) /= targetSym (%s): %s" pathSym targetSym (show path)
            else Right pathVol

-- | Get most liquid paths for all cryptos in the 'ABook' list.
--  Ignores symbols in 'nonCryptos'.
symLiquidPaths
    :: forall numeraire.
       KnownSymbol numeraire
    => Rational                             -- ^ Measure how much can be bought/sold while, at most, moving the price by this percentage
    -> [Paths.ABook]
    -> Map Sym (LiquidPaths numeraire)  -- ^ For each crypto, all the paths that the given crypto can be bought/sold through
symLiquidPaths slipPct books =
    foldr (uncurry Map.insert) Map.empty $ map buySellSlips nodeSyms
  where
    !rateMap = Rate.buildRateMap books
    (depthGraph, nodeMap) = Paths.buildDepthGraph slipPct rateMap books
    nodeSyms = filter (not . (`elem` nonCryptos)) $ map fst (Map.toList nodeMap)
    buySellSlips nodeSym =
        let pathRes = buySellPath rateMap nodeMap depthGraph slipPct nodeSym
        in (nodeSym, pathRes)

-- | The most liquid paths from some symbol to "numeraire",
--    in both buy and sell direction (in descending order of
--    liquidity)
data LiquidPaths (numeraire :: Symbol) =
    LiquidPaths
    { lpBuy  :: [[SomeEdgeVenue]]
    , lpSell :: [[SomeEdgeVenue]]
    }

-- | Get buy and sell paths, at the given slippage, for the specified crypto
buySellPath
    :: forall numeraire.
       KnownSymbol numeraire
    => Rate.RateMap numeraire
    -> Paths.NodeMap
    -> Paths.DepthGraph
    -> Rational                                                         -- ^ Slippage (in percent)
    -> Sym                                                              -- ^ Cryptocurrency symbol
    -> LiquidPaths numeraire
buySellPath rateMap nodeMap depthGraph slipPct sym =
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
                -> [[SomeEdgeVenue]]
    pathVolumes from to =
        Paths.liquidPaths slipPct rateMap nodeMap depthGraph from to


mkResult ps =
    if not . null . lefts $ ps
        then Left  . lefts $ ps
        else Right . sum . rights $ ps
