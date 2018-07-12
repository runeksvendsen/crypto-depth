{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module CryptoDepth
( getSymVolumes
)
where

import Prelude (unlines)
import CryptoDepth.Internal.DPrelude
import CryptoDepth.Internal.Util
import qualified CryptoDepth.Exchange as Exchange
import qualified CryptoDepth.Paths as Paths
import qualified CryptoDepth.RateMap as Rate
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.HashMap.Strict as Map
import qualified Money


-- | Ignore the liquidity of these non-cryptos
nonCryptos :: [Paths.Sym]
nonCryptos = ["USD", "EUR", "GBP", "JPY"]

-- | Get buy/sell volume, at the given slippage, for all cryptos in the 'ABook' list.
--  Ignores symbols in 'nonCryptos'.
getSymVolumes
    :: forall numeraire.
       KnownSymbol numeraire
    => Rational     -- ^ Measure how much can be bought/sold while, at most, moving the price by this percentage
    -> [Paths.ABook]
    -> [(Paths.Sym, Money.Dense numeraire, Money.Dense numeraire)]
getSymVolumes slipPct books =
    sortBy descSellVolume $ (map buySellSlips nodeSyms)
  where
    -- Sort by descending sell volume
    descSellVolume (_,_,s1) (_,_,s2) = s2 `compare` s1
    !rateMap = Rate.buildRateMap books
    (depthGraph, nodeMap) = Paths.buildDepthGraph slipPct rateMap books
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
    => Rate.RateMap numeraire
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
        Paths.liquidPaths slipPct rateMap nodeMap depthGraph from to
    mkResult ps =
        if not . null . lefts $ ps
            then Left  . lefts $ ps
            else Right . sum . rights $ ps
