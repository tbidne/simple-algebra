-- | Provides the 'Additive' typeclass.
module Simple.Algebra.Additive
  ( Additive (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Simple.NonNat (NonZero, unsafeNonZero, pattern MkNonZero)

-- | Defines an additive semigroup.
class Eq g => Additive g where
  (.+.) :: g -> g -> g

infixl 6 .+.

instance Additive Double where
  (.+.) = (+)

instance Additive Float where
  (.+.) = (+)

instance Additive Int where
  (.+.) = (+)

instance Additive Int8 where
  (.+.) = (+)

instance Additive Int16 where
  (.+.) = (+)

instance Additive Int32 where
  (.+.) = (+)

instance Additive Int64 where
  (.+.) = (+)

instance Additive Integer where
  (.+.) = (+)

instance Additive Natural where
  (.+.) = (+)

instance Additive Word where
  (.+.) = (+)

instance Additive Word8 where
  (.+.) = (+)

instance Additive Word16 where
  (.+.) = (+)

instance Additive Word32 where
  (.+.) = (+)

instance Additive Word64 where
  (.+.) = (+)

instance Integral a => Additive (Ratio a) where
  (.+.) = (+)

instance Additive (NonZero Natural) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

instance Additive (NonZero Word) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

instance Additive (NonZero Word8) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

instance Additive (NonZero Word16) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

instance Additive (NonZero Word32) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

instance Additive (NonZero Word64) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y
