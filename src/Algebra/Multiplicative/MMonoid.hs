-- | Provides the 'MMonoid' typeclass.
--
-- @since 0.1.0.0
module Algebra.Multiplicative.MMonoid
  ( MMonoid (..),
  )
where

import Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Refined (NonNegative, NonZero, Odd, Positive, Refined)
import Refined.Unsafe qualified as R

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
instance MMonoid Natural where
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
instance MMonoid (Ratio Integer) where
  one = 1

-- | @since 0.1.0.0
instance MMonoid (Ratio Natural) where
  one = 1

-- | @since 0.1.0.0
instance (MSemigroup a, Num a, Ord a) => MMonoid (Refined NonNegative a) where
  one = R.unsafeRefine 1

-- | @since 0.1.0.0
instance (MSemigroup a, Num a, Ord a) => MMonoid (Refined Positive a) where
  one = R.unsafeRefine 1

-- | @since 0.1.0.0
instance (MSemigroup a, Num a) => MMonoid (Refined NonZero a) where
  one = R.unsafeRefine 1

-- | @since 0.1.0.0
instance (Integral a, MSemigroup a) => MMonoid (Refined Odd a) where
  one = R.unsafeRefine 1
