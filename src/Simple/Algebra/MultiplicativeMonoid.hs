-- | Provides the 'MultiplicativeMonoid' typeclass.
module Simple.Algebra.MultiplicativeMonoid
  ( MultiplicativeMonoid (..),
    NonOne (MkNonOne, unNonOne),
    mkMonoidNonOne,
    unsafeMonoidNonOne,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Simple.Algebra.Multiplicative (Multiplicative (..))
import Smart.Data.Math.NonNegative (NonNegative (..), unsafeNonNegative)
import Smart.Data.Math.NonOne (NonOne (..))
import Smart.Data.Math.Positive (Positive (..), unsafePositive)
import Unsafe.Coerce (unsafeCoerce)

-- | Defines an algebraic monoid over a \"multiplicative\" semigroup.
class Multiplicative g => MultiplicativeMonoid g where
  one :: g

instance MultiplicativeMonoid Double where
  one = 1

instance MultiplicativeMonoid Float where
  one = 1

instance MultiplicativeMonoid Int where
  one = 1

instance MultiplicativeMonoid Int8 where
  one = 1

instance MultiplicativeMonoid Int16 where
  one = 1

instance MultiplicativeMonoid Int32 where
  one = 1

instance MultiplicativeMonoid Int64 where
  one = 1

instance MultiplicativeMonoid Integer where
  one = 1

instance MultiplicativeMonoid Natural where
  one = 1

instance MultiplicativeMonoid Word where
  one = 1

instance MultiplicativeMonoid Word8 where
  one = 1

instance MultiplicativeMonoid Word16 where
  one = 1

instance MultiplicativeMonoid Word32 where
  one = 1

instance MultiplicativeMonoid Word64 where
  one = 1

instance Integral a => MultiplicativeMonoid (Ratio a) where
  one = 1

instance (Num a, Ord a) => MultiplicativeMonoid (NonNegative a) where
  one = unsafeNonNegative 1

instance (Num a, Ord a) => MultiplicativeMonoid (Positive a) where
  one = unsafePositive 1

-- | Smart constructor for 'NonOne', based on its 'one'.
mkMonoidNonOne :: MultiplicativeMonoid a => a -> Maybe (NonOne a)
mkMonoidNonOne x
  | x == one = Nothing
  | otherwise = Just (unsafeCoerce x)

-- | Unsafe constructor for 'NonOne', based on its 'one'. Intended to be used
-- with known constants. Exercise restraint!
unsafeMonoidNonOne :: MultiplicativeMonoid a => a -> NonOne a
unsafeMonoidNonOne x
  | x == one = error "Passed identity to unsafeMonoidNonOne!"
  | otherwise = unsafeCoerce x
