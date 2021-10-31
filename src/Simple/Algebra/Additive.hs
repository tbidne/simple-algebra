-- | Provides the 'Additive' typeclass.
module Simple.Algebra.Additive
  ( Additive (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Smart.Data.Math.Negative (Negative (..), unsafeNegative)
import Smart.Data.Math.NonNegative (NonNegative (..), unsafeNonNegative)
import Smart.Data.Math.NonOne (NonOne (..), unsafeNonOne)
import Smart.Data.Math.NonPositive (NonPositive (..), unsafeNonPositive)
import Smart.Data.Math.NonZero (NonZero (..), unsafeNonZero)
import Smart.Data.Math.Positive (Positive (..), unsafePositive)

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

instance (Num a, Ord a) => Additive (Negative a) where
  MkNegative x .+. MkNegative y = unsafeNegative $ x + y

instance (Num a, Ord a) => Additive (NonNegative a) where
  MkNonNegative x .+. MkNonNegative y = unsafeNonNegative $ x + y

instance (Num a, Ord a) => Additive (NonPositive a) where
  MkNonPositive x .+. MkNonPositive y = unsafeNonPositive $ x + y

instance (Num a, Ord a) => Additive (Positive a) where
  MkPositive x .+. MkPositive y = unsafePositive $ x + y

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

instance Additive (NonOne Natural) where
  MkNonOne x .+. MkNonOne y = unsafeNonOne $ x + y

instance Additive (NonOne Word) where
  MkNonOne x .+. MkNonOne y = unsafeNonOne $ x + y

instance Additive (NonOne Word8) where
  MkNonOne x .+. MkNonOne y = unsafeNonOne $ x + y

instance Additive (NonOne Word16) where
  MkNonOne x .+. MkNonOne y = unsafeNonOne $ x + y

instance Additive (NonOne Word32) where
  MkNonOne x .+. MkNonOne y = unsafeNonOne $ x + y

instance Additive (NonOne Word64) where
  MkNonOne x .+. MkNonOne y = unsafeNonOne $ x + y
