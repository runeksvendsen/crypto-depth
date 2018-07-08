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
import OrderBook.Types              (BuySide, SellSide)
import qualified OrderBook.Matching as Match
import qualified Money


-- | Exchange by slippage through multiple orderbook sides
slippageExchangeMulti
    :: forall numeraire.
       KnownSymbol numeraire
    => Rational                                 -- ^ Slippage, in percent
    -> [SomeSide]                               -- ^ Orderbook sides to go through
    -> Either String (Money.Dense numeraire)    -- ^ Resulting amount in target currency
slippageExchangeMulti slip sides = do
    SomeSide sideE <- composeSS sides
    either fromBuySide fromSellSide sideE
  where
    conversionErr :: String -> String
    conversionErr sideStr =
        printf "'%s' target incompatible with %s"
               (symbolVal (Proxy :: Proxy numeraire))
               sideStr
    fromBuySide
        :: forall venue base quote.
        (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
        => BuySide venue base quote
        -> Either String (Money.Dense numeraire)
    fromBuySide bs =
        case sameSymbol (Proxy :: Proxy quote) (Proxy :: Proxy numeraire) of
            Just Refl -> Right . Match.resQuoteQty $ Match.slippageSell bs slip
            Nothing   ->
                case sameSymbol (Proxy :: Proxy base) (Proxy :: Proxy numeraire) of
                    Just Refl -> Right . Match.resBaseQty $ Match.slippageSell bs slip
                    Nothing   -> Left  $ conversionErr (showBuySide bs)
    fromSellSide
        :: forall venue base quote.
        (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
        => SellSide venue base quote
        -> Either String (Money.Dense numeraire)
    fromSellSide ss =
        case sameSymbol (Proxy :: Proxy base) (Proxy :: Proxy numeraire) of
            Just Refl -> Right . Match.resBaseQty $ Match.slippageBuy ss slip
            Nothing   ->
                case sameSymbol (Proxy :: Proxy quote) (Proxy :: Proxy numeraire) of
                    Just Refl -> Right . Match.resQuoteQty $ Match.slippageBuy ss slip
                    Nothing   -> Left  $ conversionErr (showSellSide ss)

