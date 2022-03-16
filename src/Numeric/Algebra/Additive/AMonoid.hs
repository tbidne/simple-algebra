-- | Provides the 'AMonoid' typeclass.
--
-- @since 0.1.0.0
module Numeric.Algebra.Additive.AMonoid
  ( AMonoid (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.TypeLits (KnownNat)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Data.Fraction (Fraction (..))
import Numeric.Data.ModN (ModN (..))
import Numeric.Data.NonNegative (NonNegative (..), reallyUnsafeNonNegative)
import Numeric.Data.NonPositive (NonPositive (..), reallyUnsafeNonPositive)

-- | Defines a monoid over an additive semigroup.
--
-- @since 0.1.0.0
class ASemigroup m => AMonoid m where
  -- | @since 0.1.0.0
  zero :: m

-- | @since 0.1.0.0
instance AMonoid Double where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Float where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Int where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Int8 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Int16 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Int32 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Int64 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Integer where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Word where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Word8 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Word16 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Word32 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Word64 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Natural where
  zero = 0

instance AMonoid (Ratio Integer) where
  zero = 0

instance AMonoid (Ratio Natural) where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a) where
  zero = (zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a) where
  zero = (zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a) where
  zero = (zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid (Fraction Integer) where
  zero = 0 :%: 1

-- | @since 0.1.0.0
instance AMonoid (Fraction Natural) where
  zero = 0 :%: 1

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Int) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Int8) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Int16) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Int32) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Int64) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Integer) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Word) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Word8) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Word16) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Word32) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Word64) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Natural) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Float) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Double) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Int) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Int8) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Int16) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Int32) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Int64) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Integer) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Word) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Word8) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Word16) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Word32) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Word64) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonNegative Natural) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Float) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Double) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Int) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Int8) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Int16) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Int32) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Int64) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Integer) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Word) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Word8) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Word16) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Word32) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Word64) where
  zero = reallyUnsafeNonPositive 0

-- | @since 0.1.0.0
instance AMonoid (NonPositive Natural) where
  zero = reallyUnsafeNonPositive 0
