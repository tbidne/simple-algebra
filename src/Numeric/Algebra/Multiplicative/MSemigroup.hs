-- | Provides the 'MSemigroup' typeclass.
--
-- @since 0.1.0.0
module Numeric.Algebra.Multiplicative.MSemigroup
  ( MSemigroup (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Data.Fraction (Fraction)
import Numeric.Data.NonNegative (NonNegative (..), reallyUnsafeNonNegative)
import Numeric.Data.NonZero (NonZero (..), reallyUnsafeNonZero)
import Numeric.Data.Positive (Positive (..), reallyUnsafePositive)

-- | Defines a multiplicative semigroup.
--
-- @since 0.1.0.0
class Eq s => MSemigroup s where
  -- | Possible constraint on the second argument to '(.*.)' e.g. for
  -- preventing overflow.
  --
  -- @since 0.1.0.0
  type MultConstraint s

  -- | @since 0.1.0.0
  (.*.) :: s -> MultConstraint s -> s

infixl 7 .*.

-- | @since 0.1.0.0
instance MSemigroup Double where
  type MultConstraint Double = Double
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Float where
  type MultConstraint Float = Float
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Int where
  type MultConstraint Int = Int
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Int8 where
  type MultConstraint Int8 = Int8
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Int16 where
  type MultConstraint Int16 = Int16
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Int32 where
  type MultConstraint Int32 = Int32
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Int64 where
  type MultConstraint Int64 = Int64
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Integer where
  type MultConstraint Integer = Integer
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Word where
  type MultConstraint Word = Word
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Word8 where
  type MultConstraint Word8 = Word8
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Word16 where
  type MultConstraint Word16 = Word16
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Word32 where
  type MultConstraint Word32 = Word32
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Word64 where
  type MultConstraint Word64 = Word64
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Natural where
  type MultConstraint Natural = Natural
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup (Ratio Integer) where
  type MultConstraint (Ratio Integer) = Ratio Integer
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup (Ratio Natural) where
  type MultConstraint (Ratio Natural) = Ratio Natural
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup (Fraction Integer) where
  type MultConstraint (Fraction Integer) = Fraction Integer
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup (Fraction Natural) where
  type MultConstraint (Fraction Natural) = Fraction Natural
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Float) where
  type MultConstraint (NonNegative Float) = NonNegative Float
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Double) where
  type MultConstraint (NonNegative Double) = NonNegative Double
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Int) where
  type MultConstraint (NonNegative Int) = NonNegative Int
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Int8) where
  type MultConstraint (NonNegative Int8) = NonNegative Int8
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Int16) where
  type MultConstraint (NonNegative Int16) = NonNegative Int16
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Int32) where
  type MultConstraint (NonNegative Int32) = NonNegative Int32
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Int64) where
  type MultConstraint (NonNegative Int64) = NonNegative Int64
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Integer) where
  type MultConstraint (NonNegative Integer) = NonNegative Integer
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Word) where
  type MultConstraint (NonNegative Word) = NonNegative Word
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Word8) where
  type MultConstraint (NonNegative Word8) = NonNegative Word8
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Word16) where
  type MultConstraint (NonNegative Word16) = NonNegative Word16
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Word32) where
  type MultConstraint (NonNegative Word32) = NonNegative Word32
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Word64) where
  type MultConstraint (NonNegative Word64) = NonNegative Word64
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonNegative Natural) where
  type MultConstraint (NonNegative Natural) = NonNegative Natural
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Float) where
  type MultConstraint (NonZero Float) = NonZero Float
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Double) where
  type MultConstraint (NonZero Double) = NonZero Double
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Int) where
  type MultConstraint (NonZero Int) = NonZero Int
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Int8) where
  type MultConstraint (NonZero Int8) = NonZero Int8
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Int16) where
  type MultConstraint (NonZero Int16) = NonZero Int16
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Int32) where
  type MultConstraint (NonZero Int32) = NonZero Int32
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Int64) where
  type MultConstraint (NonZero Int64) = NonZero Int64
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Integer) where
  type MultConstraint (NonZero Integer) = NonZero Integer
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Word) where
  type MultConstraint (NonZero Word) = NonZero Word
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Word8) where
  type MultConstraint (NonZero Word8) = NonZero Word8
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Word16) where
  type MultConstraint (NonZero Word16) = NonZero Word16
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Word32) where
  type MultConstraint (NonZero Word32) = NonZero Word32
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Word64) where
  type MultConstraint (NonZero Word64) = NonZero Word64
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (NonZero Natural) where
  type MultConstraint (NonZero Natural) = NonZero Natural
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Float) where
  type MultConstraint (Positive Float) = Positive Float
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Double) where
  type MultConstraint (Positive Double) = Positive Double
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Int) where
  type MultConstraint (Positive Int) = Positive Int
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Int8) where
  type MultConstraint (Positive Int8) = Positive Int8
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Int16) where
  type MultConstraint (Positive Int16) = Positive Int16
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Int32) where
  type MultConstraint (Positive Int32) = Positive Int32
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Int64) where
  type MultConstraint (Positive Int64) = Positive Int64
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Integer) where
  type MultConstraint (Positive Integer) = Positive Integer
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Word) where
  type MultConstraint (Positive Word) = Positive Word
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Word8) where
  type MultConstraint (Positive Word8) = Positive Word8
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Word16) where
  type MultConstraint (Positive Word16) = Positive Word16
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Word32) where
  type MultConstraint (Positive Word32) = Positive Word32
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Word64) where
  type MultConstraint (Positive Word64) = Positive Word64
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y

-- | @since 0.1.0.0
instance MSemigroup (Positive Natural) where
  type MultConstraint (Positive Natural) = Positive Natural
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y
