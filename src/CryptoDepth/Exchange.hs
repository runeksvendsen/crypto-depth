{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module CryptoDepth.Exchange
( slippageExchangeMulti
, PathInfo(..)
, piTotalQty
, Tagged(..)
, pathQty
, module Amount
)
where

import CryptoDepth.Internal.DPrelude
import CryptoDepth.Internal.Types
import CryptoDepth.Internal.Types.EdgePath
import CryptoDepth.Internal.Types.Amount as Amount
import OrderBook.Types              (BuySide, SellSide)
import qualified OrderBook.Matching as Match
import qualified Money
import qualified Data.Text as T
import Data.Tagged                  (Tagged(..))


-- | A quantity (measured in the 'numeraire' currency)
--    that can be bought/sold through a given path
--    (sequence of markets) while, at most, moving
--    the price by 'slippage'
data PathInfo numeraire slippage =
    PathInfo
    { piQty     :: Tagged slippage (Amount numeraire)   -- ^ Quantity at given slippage
    , piPath    :: NonEmpty SymVenue                    -- ^ Path (markets moved through)
    } deriving (Show, Eq, Ord)

-- | Exchange by slippage through multiple orderbook sides
slippageExchangeMulti
    :: forall numeraire slippage.
       (KnownSymbol numeraire, KnownFraction slippage)
    -- | Orderbook sides to go through
    => EdgePath numeraire
    -- | (Source symbol, amount in target currency)
    --   NB: A 'Left' value indicates a bug in the producer of the 'SomeEdgeVenue's
    -> Either String (Sym, PathInfo numeraire slippage)
slippageExchangeMulti (EdgePath sides) = do
    (SomeEdge (Edge edge), symVenues) <- composeSS sides
    (sym, qty) <- exchBuySide edge
    return $ (sym, PathInfo (Tagged . fromDense $ qty) symVenues)
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

piTotalQty :: [PathInfo numeraire slippage] -> Amount numeraire
piTotalQty = sum . map (unTagged . piQty)

pathQty :: (KnownSymbol numeraire, KnownFraction slippage)
        => EdgePath numeraire
        -> Tagged slippage (Amount numeraire)
pathQty = piQty . snd . throwBug . slippageExchangeMulti
