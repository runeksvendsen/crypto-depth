{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module CryptoDepth.Internal.Types.OneDiv
( -- * Type
  OneDiv
  -- * Linking type and value level
, KnownFraction(fracVal)
  -- * Re-exports
, Nat
, KnownNat
, Rational
)
where

import Prelude              (Show(show), const, (.), ($))
import GHC.TypeLits         (Nat, KnownNat, CmpNat, natVal)
import Data.Ratio           (Rational, (%))
import Data.Proxy           (Proxy(Proxy))
import Data.Ord             (Ordering(LT))


-- | A type-level fraction whose numerator is 1 --
--    e.g. one percent (1/100) has the type "OneDiv 100".
--   Used for representing fractions less than or equal to 1.
data OneDiv (denominator :: Nat)

-- | Convert a typ-level fraction into its value-level counterpart
class KnownFraction frac where
    fracVal :: Proxy frac -> Rational

type KnownNonZeroNat denominator =
    (KnownNat denominator, CmpNat 0 denominator ~ 'LT)

instance KnownNonZeroNat denominator => KnownFraction (OneDiv denominator) where
    fracVal = const $ 1 % natVal (Proxy :: Proxy denominator)

instance KnownNonZeroNat denominator => Show (OneDiv denominator) where
    show = const . show $ fracVal (Proxy :: Proxy (OneDiv denominator))
