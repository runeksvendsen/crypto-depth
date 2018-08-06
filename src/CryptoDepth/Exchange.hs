{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module CryptoDepth.Exchange
( slippageExchangeMulti
, PathInfo(..)
, piTotalQty
)
where

import CryptoDepth.Internal.DPrelude
import CryptoDepth.Internal.Types
import OrderBook.Types              (BuySide, SellSide)
import qualified OrderBook.Matching as Match
import qualified Money
import qualified Data.Text as T


data PathInfo numeraire =
    PathInfo
    { piQty     :: Money.Dense numeraire
    , piPath    :: NonEmpty SymVenue
    }

-- | Exchange by slippage through multiple orderbook sides
slippageExchangeMulti
    :: forall numeraire.
       KnownSymbol numeraire
    => Rational                                 -- ^ Slippage, in percent
    -> NonEmpty SomeEdgeVenue                   -- ^ Orderbook sides to go through
    -> Either String (Sym, PathInfo numeraire)  -- ^ (Source symbol, amount in target currency)
slippageExchangeMulti slip sides = do
    (SomeEdge (Edge edge), symVenues) <- composeSS sides
    (sym, qty) <- exchBuySide edge
    return $ (sym, PathInfo qty symVenues)
  where
    conversionErr :: String -> String
    conversionErr sideStr =
        printf "'%s' target incompatible with %s"
               (symbolVal (Proxy :: Proxy numeraire))
               sideStr
    exchBuySide
        :: forall base quote.
        (KnownSymbol base, KnownSymbol quote)
        => BuySide base quote
        -> Either String (Sym, Money.Dense numeraire)
    exchBuySide bs =
        case sameSymbol (Proxy :: Proxy quote) (Proxy :: Proxy numeraire) of
            Just Refl -> Right $ ( toS $ symbolVal (Proxy :: Proxy base)
                                 , Match.resQuoteQty $ Match.slippageSell bs slip)
            Nothing   ->
                case sameSymbol (Proxy :: Proxy base) (Proxy :: Proxy numeraire) of
                    Just Refl -> Right $ ( toS $ symbolVal (Proxy :: Proxy quote),
                                           Match.resBaseQty $ Match.slippageSell bs slip)
                    Nothing   -> Left  $ conversionErr (show $ Edge bs)

piTotalQty :: [PathInfo numeraire] -> Money.Dense numeraire
piTotalQty = sum . map piQty
