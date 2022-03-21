-- | Provides the 'MMonoid' typeclass.
--
-- @since 0.1.0.0
module Numeric.Algebra.Multiplicative.MMonoid
  ( MMonoid (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Data.Fraction (Fraction (..))
import Numeric.Data.ModN (ModN (..))
import Numeric.Data.ModP (ModP (..))
import Numeric.Data.NonNegative (NonNegative (..), reallyUnsafeNonNegative)
import Numeric.Data.NonZero (NonZero (..), reallyUnsafeNonZero)
import Numeric.Data.Positive (Positive (..), reallyUnsafePositive)

-- | Defines a monoid over a multiplicative semigroup.
--
-- @since 0.1.0.0
class MSemigroup m => MMonoid m where
  -- | @since 0.1.0.0
  one :: m

-- | @since 0.1.0.0
instance MMonoid Double where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Float where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Int where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Int8 where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Int16 where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Int32 where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Int64 where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Integer where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Word where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Word8 where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Word16 where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Word32 where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Word64 where
  one = 1

-- | @since 0.1.0.0
instance MMonoid Natural where
  one = 1

-- | @since 0.1.0.0
instance MMonoid (Ratio Integer) where
  one = 1

-- | @since 0.1.0.0
instance MMonoid (Ratio Natural) where
  one = 1

-- | @since 0.1.0.0
instance MMonoid (Fraction Integer) where
  one = 1 :%: 1

-- | @since 0.1.0.0
instance MMonoid (Fraction Natural) where
  one = 1 :%: 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Integer) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Natural) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Integer) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Natural) where
  one = MkModP 1

-- | @since 0.1.0.0
instance (Eq a, Num a, Ord a, Show a) => MMonoid (NonNegative a) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance (Eq a, Num a) => MMonoid (NonZero a) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance (Eq a, Num a, Ord a, Show a) => MMonoid (Positive a) where
  one = reallyUnsafePositive 1
