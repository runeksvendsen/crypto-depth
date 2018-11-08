{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoDepth.Internal.Types.Amount
( Amount
, mkAmount
, fromDense
, toDense
)
where

import CryptoDepth.Internal.DPrelude
import qualified Money


-- | Represents monetary amounts as integers.
--   Used for amounts that will not be used in further
--    calculations (since it's relatively imprecise).
--   Range: 0 - 2^64-1.
newtype Amount (currency :: Symbol) = Amount Word64
    deriving (Eq, Integral, Bounded, Enum, Num, Ord, Real, Generic)

instance KnownSymbol currency => Show (Amount currency) where
    show (Amount qty) =
        thousandGroup (show qty)
        ++ " "
        ++ symbolVal (Proxy :: Proxy currency)
        where chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)
              -- Source: https://stackoverflow.com/a/12882583/700597
              thousandGroup = reverse . intercalate "," . chunks 3 . reverse

-- | Create an 'Amount' (bounded by 2^64-1)
mkAmount :: Integral amount => amount -> Amount currency
mkAmount = Amount . fromIntegral

fromDense :: Money.Dense currency -> Amount currency
fromDense = round . toRational

toDense :: Amount currency -> Money.Dense currency
toDense = Money.dense' . toRational
