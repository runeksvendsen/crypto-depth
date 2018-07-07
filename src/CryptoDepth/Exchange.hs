{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module CryptoDepth.Exchange
( slippageExchangeMulti
)
where

import CryptoDepth.Internal.DPrelude
import CryptoDepth.Types
import OrderBook.Types              (BuyOrders, SellOrders, BuySide, SellSide)
import qualified OrderBook.Matching as Match
import qualified Money


-- | Exchange by slippage through multiple orderbook sides
slippageExchangeMulti
    :: forall dst.
       KnownSymbol dst
    => Rational                         -- ^ Slippage, in percent
    -> [SomeSide]                       -- ^ Orderbook sides to go through
    -> Either String (Money.Dense dst)  -- ^ Resulting amount in target currency
slippageExchangeMulti slip sides = do
    SomeSide sideE <- composeSS sides
    either fromBuySide fromSellSide sideE
  where
    conversionErr :: String -> String
    conversionErr sideStr =
        printf "'%s' target incompatible with %s"
               (symbolVal (Proxy :: Proxy dst))
               sideStr
    fromBuySide
        :: forall venue base quote.
        (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
        => BuySide venue base quote
        -> Either String (Money.Dense dst)
    fromBuySide bs =
        case sameSymbol (Proxy :: Proxy quote) (Proxy :: Proxy dst) of
            Just Refl -> Right . Match.resQuoteQty $ Match.slippageSell bs slip
            Nothing   ->
                case sameSymbol (Proxy :: Proxy base) (Proxy :: Proxy dst) of
                    Just Refl -> Right . Match.resBaseQty $ Match.slippageSell bs slip
                    Nothing   -> Left  $ conversionErr (showBuySide bs)
    fromSellSide
        :: forall venue base quote.
        (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
        => SellSide venue base quote
        -> Either String (Money.Dense dst)
    fromSellSide ss =
        case sameSymbol (Proxy :: Proxy base) (Proxy :: Proxy dst) of
            Just Refl -> Right . Match.resBaseQty $ Match.slippageBuy ss slip
            Nothing   ->
                case sameSymbol (Proxy :: Proxy quote) (Proxy :: Proxy dst) of
                    Just Refl -> Right . Match.resQuoteQty $ Match.slippageBuy ss slip
                    Nothing   -> Left  $ conversionErr (showSellSide ss)

