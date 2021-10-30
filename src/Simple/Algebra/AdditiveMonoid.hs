-- | Provides the 'AdditiveMonoid' typeclass.
module Simple.Algebra.AdditiveMonoid
  ( AdditiveMonoid (..),
    NonZero (MkNonZero, unNonZero),
    mkMonoidNonZero,
    unsafeMonoidNonZero,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Simple.Algebra.Additive (Additive (..))
import Smart.Data.Math.NonNegative (NonNegative (..), unsafeNonNegative)
import Smart.Data.Math.NonZero (NonZero (..))
import Unsafe.Coerce (unsafeCoerce)

-- | Defines an algebraic monoid over an \"additive\" semigroup.
class Additive g => AdditiveMonoid g where
  zero :: g

instance AdditiveMonoid Double where
  zero = 0

instance AdditiveMonoid Float where
  zero = 0

instance AdditiveMonoid Int where
  zero = 0

instance AdditiveMonoid Int8 where
  zero = 0

instance AdditiveMonoid Int16 where
  zero = 0

instance AdditiveMonoid Int32 where
  zero = 0

instance AdditiveMonoid Int64 where
  zero = 0

instance AdditiveMonoid Integer where
  zero = 0

instance AdditiveMonoid Natural where
  zero = 0

instance AdditiveMonoid Word where
  zero = 0

instance AdditiveMonoid Word8 where
  zero = 0

instance AdditiveMonoid Word16 where
  zero = 0

instance AdditiveMonoid Word32 where
  zero = 0

instance AdditiveMonoid Word64 where
  zero = 0

instance Integral a => AdditiveMonoid (Ratio a) where
  zero = 0

instance (Num a, Ord a) => AdditiveMonoid (NonNegative a) where
  zero = unsafeNonNegative 0

-- | Smart constructor for 'NonZero', based on its 'zero'.
mkMonoidNonZero :: AdditiveMonoid a => a -> Maybe (NonZero a)
mkMonoidNonZero x
  | x == zero = Nothing
  | otherwise = Just (unsafeCoerce x)

-- | Unsafe constructor for 'NonZero', based on its 'zero'. Intended to be used
-- with known constants. Exercise restraint!
unsafeMonoidNonZero :: AdditiveMonoid a => a -> NonZero a
unsafeMonoidNonZero x
  | x == zero = error "Passed identity to unsafeMonoidNonZero!"
  | otherwise = unsafeCoerce x
