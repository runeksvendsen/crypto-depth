{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module CryptoDepth.Internal.Types
( module CryptoDepth.Internal.Types
, module CryptoDepth.Internal.Types.OneDiv
, module Amount
, module NE
, Map.HashMap
)
where

import CryptoDepth.Internal.DPrelude
import CryptoDepth.Internal.Types.OneDiv
import CryptoDepth.Internal.Types.Amount as Amount
import CryptoVenues.Types.Market
import CryptoVenues.Fetch.MarketBook
import OrderBook.Types
import qualified OrderBook.Types    as OB
import qualified Control.Category   as Cat
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty  as NE

type NodeMap = Map.HashMap Sym Int      -- ^ Map a currency symbol to a graph node ID
type Sym = Text                         -- ^ A currency symbol, e.g. "USD", "EUR", "BTC", "ETH" etc.
type Venue = Text

-- | An edge weight that is the inverse of the quantity (with a unit of 'numeraire')
--    that can be bought/sold at the specified 'slippage'
--    ('OneDiv' is used to specify the slippage as a type)
newtype Weight (numeraire :: Symbol) slippage = Weight Rational
    deriving (Enum, Eq, Fractional, Num, Ord, Read, Real, RealFrac, Show)

fracValPercent :: KnownFraction frac => Proxy frac -> Rational
fracValPercent = (100 *) . fracVal

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

newtype src $-> dst = Edge (BuySide src dst)

srcDst
    :: forall src dst.
    (KnownSymbol src, KnownSymbol dst)
    => src $-> dst
    -> (Sym, Sym)
srcDst _ = (toS $ symbolVal (Proxy :: Proxy src), toS $ symbolVal (Proxy :: Proxy dst))

instance Eq (src $-> dst) where
    (Edge bs1) == (Edge bs2) = bs1 == bs2

instance Cat.Category ($->) where
    id = Edge Cat.id
    (Edge e1) . (Edge e2) = Edge (e1 Cat.. e2)

instance (KnownSymbol src, KnownSymbol dst) => Show (src $-> dst) where
    show _ = printf "<%s -> %s>"
        (symbolVal (Proxy :: Proxy src)) (symbolVal (Proxy :: Proxy dst))

data SomeEdge =
    forall src dst.
    (KnownSymbol src, KnownSymbol dst) =>
       SomeEdge (src $-> dst)

fromBuySide :: (KnownSymbol src, KnownSymbol dst) => BuySide dst src -> SomeEdge
fromBuySide = SomeEdge . Edge

fromSellSide :: (KnownSymbol src, KnownSymbol dst) => SellSide dst src -> SomeEdge
fromSellSide = SomeEdge . Edge . OB.invert

instance Show SomeEdge where
    show (SomeEdge edge) = show edge

newtype SomeEdgeVenue = SomeEdgeVenue {someEdgeVenue :: (SomeEdge,Venue)}

toSomeEdge :: SomeEdgeVenue -> SomeEdge
toSomeEdge (SomeEdgeVenue tuple) = fst tuple

newtype BuySideVenue  base quote = BuySideVenue  (BuySide base quote , Venue)
newtype SellSideVenue base quote = SellSideVenue (SellSide base quote, Venue)

type SymVenue = (Sym,Venue,Sym)

toSymVenue :: SomeEdgeVenue -> SymVenue
toSymVenue (SomeEdgeVenue (SomeEdge edge, venue)) =
    let (src,dst) = srcDst edge
    in (src,venue,dst)

-- | Path through one or more cryptocurrency markets for a specific venue.
-- E.g. BTC -> ETH -> IOS -> EUR on "bitfinex"
--  is: ExchangePath "bitfinex" "BTC" "EUR" ["ETH", "IOS"]
data ExchangePath
    = ExchangePath
    { epVenue   :: Venue    -- ^ Venue of the given symbols
    , epSrc     :: Sym      -- ^ Start symbol
    , epDst     :: Sym      -- ^ End symbol
    , epBetween :: [Sym]    -- ^ Ordered list of symbols between "start" and "end" symbol (might be empty)
    } deriving (Show, Eq)

-- | See "test/Spec.hs" for specification
groupVenues :: NonEmpty SymVenue -> NE.NonEmpty ExchangePath
groupVenues symVenues =
    foldr groupVenue (NE.last epLst :| []) (NE.init epLst)
  where
    epLst = map toEP symVenues
    toEP (src, venue, dst) = ExchangePath venue src dst []
    -- venueL+venueR and srcL+srcR are equal: join into one ExchangePath
    joinEPs (ExchangePath venueL srcL dstL betweenL)
            (ExchangePath _      _    dstR betweenR) =
                ExchangePath venueL srcL dstR (dstL : betweenL ++ betweenR)
    groupVenue epL@(ExchangePath venueL _ _ _) lst@(epR@(ExchangePath venueR _ _ _) :| xs') =
        if venueL == venueR
            then joinEPs epL epR :| xs'
            else epL <| lst

-- | Compose multiple 'SomeEdge's into a single one, given they are compatible.
composeSS :: NonEmpty SomeEdgeVenue -> Either String (SomeEdge, NonEmpty SymVenue)
composeSS sev = do
    someSide <- composeSSR $ map toSomeEdge sev
    return (someSide, map toSymVenue sev)

-- | Compose multiple 'SomeEdge's into a single one, given they are compatible.
-- | Compose multiple 'SomeEdge's into a single one, given they are compatible.
composeSSR :: NonEmpty SomeEdge -> Either String SomeEdge
composeSSR (ss :| [])                   = Right ss
composeSSR (SomeEdge s1 :| (SomeEdge s2:sL)) =
    go s1 s2 >>= composeSSR
  where
    mkErr = printf "incompatible edges: %s %s"
    go :: forall src1 dst1 src2 dst2.
          ( KnownSymbol src1, KnownSymbol dst1
          , KnownSymbol src2, KnownSymbol dst2 )
       => src1 $-> dst1
       -> src2 $-> dst2
       -> Either String (NonEmpty SomeEdge)
    go edge1 edge2 =
        case sameSymbol (Proxy :: Proxy dst1) (Proxy :: Proxy src2) of
            Nothing   -> Left $ mkErr (show edge1) (show edge2)
            Just Refl -> Right $ (SomeEdge $ edge2 Cat.. edge1) :| sL

instance Eq SomeEdge where
    (SomeEdge edge1) == (SomeEdge edge2) =
        case sideEq edge1 edge2 of
            Nothing           -> False
            Just (Refl, Refl) -> edge1 == edge2

instance Eq SomeEdgeVenue where
    (SomeEdgeVenue (side1, venue1)) == (SomeEdgeVenue (side2, venue2)) =
        side1 == side2 && venue1 == venue2

sideEq
    :: forall a t11 t12 t21 t22.
       ( KnownSymbol t11, KnownSymbol t12
       , KnownSymbol t21, KnownSymbol t22
       )
    => a (t11 :: Symbol) (t12 :: Symbol)
    -> a (t21 :: Symbol) (t22 :: Symbol)
    -> Maybe (t11 :~: t21, t12 :~: t22)
sideEq a1 a2 =
    case a1 of
        (_ :: a base1 quote1) ->
            case a2 of
                (_ :: a base2 quote2) ->
                    case sameSymbol (Proxy :: Proxy base1) (Proxy :: Proxy base2) of
                        Nothing -> Nothing
                        Just reflA ->
                            case sameSymbol (Proxy :: Proxy quote1) (Proxy :: Proxy quote2) of
                                Nothing -> Nothing
                                Just reflB -> Just (reflA, reflB)


instance Show (Pair (Maybe SomeEdgeVenue) (Weight numeraire slippage)) where
    show (Pair sideM rat) =
        printf "%s" (showSide sideM)
      where
        showSide (Just side) = show side
        showSide Nothing = "-"

instance Show SomeEdgeVenue where
    show (SomeEdgeVenue (SomeEdge se, v)) = showEdgeVenue v se

showEdgeVenue
    :: forall src dst.
       (KnownSymbol src, KnownSymbol dst)
    => Venue
    -> src $-> dst
    -> String
showEdgeVenue v _ = printf "<%s: %s -> %s>"
        (toS v :: String) (symbolVal (Proxy :: Proxy src)) (symbolVal (Proxy :: Proxy dst))


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

obBuy :: forall venue base quote. (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
       => OrderBook venue base quote -> (Venue,BuySide base quote)
obBuy ob =
    (toS $ symbolVal (Proxy :: Proxy venue), obBids $ ob)

obSell :: forall venue base quote. (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
       => OrderBook venue base quote -> (Venue,SellSide base quote)
obSell ob =
    (toS $ symbolVal (Proxy :: Proxy venue), obAsks $ ob)

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

sideBase
    :: forall a base b. KnownSymbol base
    => a (base :: Symbol) (b :: Symbol)
    -> Text
sideBase _ = toS $ symbolVal (Proxy :: Proxy base) :: Text

sideQuote
    :: forall a b quote. KnownSymbol quote
    => a (b :: Symbol) (quote :: Symbol)
    -> Text
sideQuote _ = toS $ symbolVal (Proxy :: Proxy quote) :: Text


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
