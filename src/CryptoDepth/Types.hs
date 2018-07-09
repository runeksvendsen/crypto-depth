{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GADTs #-}
module CryptoDepth.Types where

import CryptoDepth.Internal.DPrelude
import CryptoVenues.Types.Market
import CryptoVenues.Fetch.MarketBook
import OrderBook.Types
import qualified OrderBook.Types    as OB
import qualified Data.HashMap.Strict as Map


type NodeMap = Map.HashMap Sym Int      -- ^ Map a currency symbol to a graph node ID
type Sym = Text                         -- ^ A currency symbol, e.g. "USD", "EUR", "BTC", "ETH" etc.

data Pair v t = Pair { pFst :: v, pSnd :: t }
    deriving Eq

instance Functor (Pair (Maybe v)) where
    fmap f (Pair v t) = Pair v (f t)

instance (Eq v, Real t) => Real (Pair (Maybe v) t) where
    toRational = toRational . pSnd

instance Num t => Num (Pair (Maybe v) t) where
    Pair v1 t1 + Pair _ t2 = Pair v1 (t1+t2)
    Pair v1 t1 * Pair _ t2 = Pair v1 (t1*t2)
    abs = fmap abs
    signum = fmap signum
    fromInteger i = Pair Nothing (fromInteger i)
    negate = fmap negate

instance (Eq v, Ord t) => Ord (Pair (Maybe v) t) where
    (Pair _ t1) <= (Pair _ t2) = t1 <= t2

data AnyMarketList =
    forall venue.
    MarketBook venue
        => AnyMarketList [Market venue]

data SomeSide =
    forall venue base quote.
    (KnownSymbol venue, KnownSymbol base, KnownSymbol quote) =>
       SomeSide { someSide :: (Either (BuySide venue base quote)
                                      (SellSide venue base quote))
                }

-- | Compose multiple 'SomeSide's into a single one, given they are compatible.
composeSS :: [SomeSide] -> Either String SomeSide
composeSS []                           = Left "Empty SomeSide list"
composeSS [ss]                         = Right ss
composeSS (SomeSide s1:SomeSide s2:sL) =
    go s1 s2 >>= composeSS
  where
    mkErr = printf "incompatible sides: %s %s"
    go :: forall venue1 base1 quote1 venue2 base2 quote2.
          ( KnownSymbol venue1, KnownSymbol base1, KnownSymbol quote1
          , KnownSymbol venue2, KnownSymbol base2, KnownSymbol quote2 )
       => Either (BuySide venue1 base1 quote1) (SellSide venue1 base1 quote1)
       -> Either (BuySide venue2 base2 quote2) (SellSide venue2 base2 quote2)
       -> Either String [SomeSide]
    go s1E s2E = case s1E of
        Left bs1 -> case s2E of
            Left bs2 ->
                case sameSymbol (Proxy :: Proxy quote1) (Proxy :: Proxy base2) of
                    Just Refl -> Right $ (SomeSide . Left $ bs2 `OB.compose` bs1) : sL
                    Nothing   -> Left $ mkErr (showBuySide bs1) (showBuySide bs2)
            Right ss2 ->
                Right $ SomeSide s1E : (SomeSide $ Left (OB.invert ss2 :: BuySide venue2 quote2 base2)) : sL
        Right ss1 -> case s2E of
            Right ss2 ->
                case sameSymbol (Proxy :: Proxy base1) (Proxy :: Proxy quote2) of
                    Just Refl -> Right $ (SomeSide . Right $ ss1 `OB.compose` ss2) : sL
                    Nothing   -> Left $ mkErr (showSellSide ss1) (showSellSide ss2)
            Left bs2 ->
                Right $ SomeSide s1E : (SomeSide $ Right (OB.invert bs2 :: SellSide venue2 quote2 base2)) : sL

instance Eq SomeSide where
    (SomeSide (Left _))  == (SomeSide (Right _)) = False
    (SomeSide (Right _)) == (SomeSide (Left _))  = False
    (SomeSide (Left bs1))  == (SomeSide (Left bs2))  = sideEq bs1 bs2
    (SomeSide (Right ss1)) == (SomeSide (Right ss2)) = sideEq ss1 ss2

sideEq
    :: forall a t11 t12 t13 t21 t22 t23.
       ( KnownSymbol t11, KnownSymbol t12, KnownSymbol t13
       , KnownSymbol t21, KnownSymbol t22, KnownSymbol t23
       )
    => a (t11 :: Symbol) (t12 :: Symbol) (t13 :: Symbol)
    -> a (t21 :: Symbol) (t22 :: Symbol) (t23 :: Symbol)
    -> Bool
sideEq a1 a2 =
    case a1 of
        (_ :: a venue1 base1 quote1) ->
            case a2 of
                (_ :: a venue2 base2 quote2) ->
                    case sameSymbol (Proxy :: Proxy venue1) (Proxy :: Proxy venue2) of
                        Nothing -> False
                        Just _  ->
                            case sameSymbol (Proxy :: Proxy base1) (Proxy :: Proxy base2) of
                                Nothing -> False
                                Just _  ->
                                    case sameSymbol (Proxy :: Proxy quote1) (Proxy :: Proxy quote2) of
                                        Nothing -> False
                                        Just _  -> True


instance Show (Pair (Maybe SomeSide) Rational) where
    show (Pair sideM rat) =
        printf "{%s %f}" (showSide sideM) (realToFrac rat :: Double)
      where
        showSide (Just side) = show side
        showSide Nothing = "-"

instance Show SomeSide where
    show (SomeSide ssE) = either showBuySide showSellSide ssE

showSellSide :: (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
             => SellSide venue base quote
             -> String
showSellSide ss = printf "<SellSide (%s): %s -> %s>"
        (t1 ss) (t2 ss) (t3 ss)

showBuySide :: (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
            => BuySide venue base quote
            -> String
showBuySide ss = printf "<BuySide (%s): %s <- %s>"
        (t1 ss) (t2 ss) (t3 ss)

-- | Just some order book
data ABook =
    forall venue base quote.
    ( KnownSymbol venue, KnownSymbol base, KnownSymbol quote
    , MarketBook venue)
       => ABook (OrderBook venue base quote)

instance Show ABook where
    show (ABook ob) =
        toS $ abBase ob <> "/" <> abQuote ob <> " (" <> abVenue ob <> ")"

instance Eq ABook where
    (ABook ob1) == (ABook ob2) =
        case ob1 of
            (ob1 :: OrderBook venue1 base1 quote1) ->
                case ob2 of
                    (ob2 :: OrderBook venue2 base2 quote2) ->
                        case sameSymbol (Proxy :: Proxy venue1) (Proxy :: Proxy venue2) of
                            Nothing -> False
                            Just _  ->
                                case sameSymbol (Proxy :: Proxy base1) (Proxy :: Proxy base2) of
                                    Nothing -> False
                                    Just _  ->
                                        case sameSymbol (Proxy :: Proxy quote1) (Proxy :: Proxy quote2) of
                                            Nothing -> False
                                            Just _  -> True

instance Show (Pair (Maybe ABook) Rational) where
    show (Pair bookM rat) =
        printf "<%s %f>" (showBook bookM) (realToFrac rat :: Double)
        where
        showBook (Just book) = show book
        showBook Nothing = "-"

abBuy :: ABook -> SomeSide
abBuy (ABook ob) =
    SomeSide . Left . obBids $ ob

abSell :: ABook -> SomeSide
abSell (ABook ob) =
    SomeSide . Right . obAsks $ ob

abBase
    :: forall base a b c. KnownSymbol base
    => a (b :: Symbol) (base :: Symbol) (c :: Symbol)
    -> Text
abBase _ = toS $ symbolVal (Proxy :: Proxy base) :: Text

abQuote
    :: forall quote a b c. KnownSymbol quote
    => a (b :: Symbol) (c :: Symbol) (quote :: Symbol)
    -> Text
abQuote _ = toS $ symbolVal (Proxy :: Proxy quote) :: Text

abVenue
    :: forall venue a b c. KnownSymbol venue
    => a (venue :: Symbol) (b :: Symbol) (c :: Symbol)
    -> Text
abVenue _ =
    toS $ symbolVal (Proxy :: Proxy venue) :: Text

t1 :: forall a t1 t2 t3.
      KnownSymbol t1
   => a (t1 :: Symbol) (t2 :: Symbol) (t3 :: Symbol) -> Text
t1 _ = toS $ symbolVal (Proxy :: Proxy t1) :: Text

t2 :: forall a t1 t2 t3.
      KnownSymbol t2
   => a (t1 :: Symbol) (t2 :: Symbol) (t3 :: Symbol) -> Text
t2 _ = toS $ symbolVal (Proxy :: Proxy t2) :: Text

t3 :: forall a t1 t2 t3.
      KnownSymbol t3
   => a (t1 :: Symbol) (t2 :: Symbol) (t3 :: Symbol) -> Text
t3 _ = toS $ symbolVal (Proxy :: Proxy t3) :: Text
