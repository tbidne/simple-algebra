-- | Provides the 'Multiplicative' typeclass.
module Simple.Algebra.Multiplicative
  ( Multiplicative (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Simple.NonNat (NonZero, unsafeNonZero, pattern MkNonZero)

-- | Defines a multiplicative semigroup.
class Eq g => Multiplicative g where
  (.*.) :: g -> g -> g

infixl 7 .*.

instance Multiplicative Double where
  (.*.) = (*)

instance Multiplicative Float where
  (.*.) = (*)

instance Multiplicative Int where
  (.*.) = (*)

instance Multiplicative Int8 where
  (.*.) = (*)

instance Multiplicative Int16 where
  (.*.) = (*)

instance Multiplicative Int32 where
  (.*.) = (*)

instance Multiplicative Int64 where
  (.*.) = (*)

instance Multiplicative Integer where
  (.*.) = (*)

instance Multiplicative Natural where
  (.*.) = (*)

instance Multiplicative Word where
  (.*.) = (*)

instance Multiplicative Word8 where
  (.*.) = (*)

instance Multiplicative Word16 where
  (.*.) = (*)

instance Multiplicative Word32 where
  (.*.) = (*)

instance Multiplicative Word64 where
  (.*.) = (*)

instance Integral a => Multiplicative (Ratio a) where
  (.*.) = (*)

instance (Num a, Ord a) => Multiplicative (NonZero a) where
  MkNonZero x .*. MkNonZero y = unsafeNonZero $ x * y