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
instance KnownNat n => MMonoid (ModN n Int) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Int8) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Int16) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Int32) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Int64) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Integer) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Word) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Word8) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Word16) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Word32) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Word64) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Natural) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Int) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Int8) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Int16) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Int32) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Int64) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Integer) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Word) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Word8) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Word16) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Word32) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Word64) where
  one = MkModP 1

-- | @since 0.1.0.0
instance KnownNat p => MMonoid (ModP p Natural) where
  one = MkModP 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Float) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Double) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Int) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Int8) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Int16) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Int32) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Int64) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Integer) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Word) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Word8) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Word16) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Word32) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Word64) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonNegative Natural) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Float) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Double) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Int) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Int8) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Int16) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Int32) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Int64) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Integer) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Word) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Word8) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Word16) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Word32) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Word64) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (NonZero Natural) where
  one = reallyUnsafeNonZero 1

-- | @since 0.1.0.0
instance MMonoid (Positive Float) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Double) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Int) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Int8) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Int16) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Int32) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Int64) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Integer) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Word) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Word8) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Word16) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Word32) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Word64) where
  one = reallyUnsafePositive 1

-- | @since 0.1.0.0
instance MMonoid (Positive Natural) where
  one = reallyUnsafePositive 1
