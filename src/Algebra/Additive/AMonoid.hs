-- | Provides the 'AMonoid' typeclass.
--
-- @since 0.1.0.0
module Algebra.Additive.AMonoid
  ( AMonoid (..),
  )
where

import Algebra.Additive.ASemigroup (ASemigroup (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Refined (Even, NonNegative, NonPositive, Refined)
import Refined.Unsafe qualified as R

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
instance AMonoid Natural where
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

instance Integral a => AMonoid (Ratio a) where
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
instance (ASemigroup a, Num a, Ord a) => AMonoid (Refined NonNegative a) where
  zero = R.unsafeRefine 0

-- | @since 0.1.0.0
instance (ASemigroup a, Num a, Ord a) => AMonoid (Refined NonPositive a) where
  zero = R.unsafeRefine 0

-- | @since 0.1.0.0
instance (ASemigroup a, Integral a) => AMonoid (Refined Even a) where
  zero = R.unsafeRefine 0
