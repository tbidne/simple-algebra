-- | Provides the 'MultiplicativeMonoid' typeclass.
module Simple.Algebra.MultiplicativeMonoid
  ( MultiplicativeMonoid (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Simple.Algebra.Multiplicative (Multiplicative (..))
import Simple.NonNat (NonZero, unsafeNonZero)

-- | Defines a monoid over a \"multiplicative\" semigroup.
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

instance (Num a, Ord a) => MultiplicativeMonoid (NonZero a) where
  one = unsafeNonZero 1