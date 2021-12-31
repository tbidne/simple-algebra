-- | Provides the 'MultiplicativeMonoid' typeclass.
--
-- @since 0.1.0.0
module Algebra.MultiplicativeMonoid
  ( MultiplicativeMonoid (..),
  )
where

import Algebra.Multiplicative (Multiplicative (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Refined (NonNegative, NonZero, Odd, Positive, Refined)
import Refined.Unsafe qualified as R

-- | Defines a monoid over a \"multiplicative\" semigroup.
--
-- @since 0.1.0.0
class Multiplicative g => MultiplicativeMonoid g where
  -- | @since 0.1.0.0
  one :: g

-- | @since 0.1.0.0
instance MultiplicativeMonoid Double where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Float where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Int where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Int8 where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Int16 where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Int32 where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Int64 where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Integer where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Natural where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Word where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Word8 where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Word16 where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Word32 where
  one = 1

-- | @since 0.1.0.0
instance MultiplicativeMonoid Word64 where
  one = 1

-- | @since 0.1.0.0
instance Integral a => MultiplicativeMonoid (Ratio a) where
  one = 1

-- | @since 0.1.0.0
instance (Num a, Ord a) => MultiplicativeMonoid (Refined NonNegative a) where
  one = R.unsafeRefine 1

-- | @since 0.1.0.0
instance forall a. (Num a, Ord a) => MultiplicativeMonoid (Refined Positive a) where
  one = R.unsafeRefine 1

-- | @since 0.1.0.0
instance forall a. (Num a, Ord a) => MultiplicativeMonoid (Refined NonZero a) where
  one = R.unsafeRefine 1

-- | @since 0.1.0.0
instance forall a. (Integral a) => MultiplicativeMonoid (Refined Odd a) where
  one = R.unsafeRefine 1
