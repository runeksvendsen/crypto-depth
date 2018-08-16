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


-- | A quantity (measured in the 'numeraire' currency)
--    that can be bought/sold through a given path
--    (sequence of markets) while, at most, moving
--    the price by 'slippage'
data PathInfo numeraire slippage =
    PathInfo
    { piQty     :: Money.Dense numeraire    -- ^ Quantity at given slippage
    , piPath    :: NonEmpty SymVenue        -- ^ Path (markets moved through)
    } deriving (Show, Eq)

-- | Exchange by slippage through multiple orderbook sides
slippageExchangeMulti
    :: forall numeraire slippage.
       (KnownSymbol numeraire, KnownFraction slippage)
    -- | Orderbook sides to go through
    => NonEmpty SomeEdgeVenue
    -- | (Source symbol, amount in target currency)
    --   NB: A 'Left' value indicates a bug in the producer of the 'SomeEdgeVenue's
    -> Either String (Sym, PathInfo numeraire slippage)
slippageExchangeMulti sides = do
    (SomeEdge (Edge edge), symVenues) <- composeSS sides
    (sym, qty) <- exchBuySide edge
    return $ (sym, PathInfo qty symVenues)
  where
    slip = fracValPercent (Proxy :: Proxy slippage)
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

piTotalQty :: [PathInfo numeraire slippage] -> Money.Dense numeraire
piTotalQty = sum . map piQty
