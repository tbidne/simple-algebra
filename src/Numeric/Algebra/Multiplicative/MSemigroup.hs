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
import GHC.TypeNats (KnownNat)
import Numeric.Data.Fraction (Fraction)
import Numeric.Data.ModN (ModN (..))
import Numeric.Data.ModP (ModP (..))
import Numeric.Data.ModP qualified as ModP
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
instance KnownNat n => MSemigroup (ModN n Integer) where
  type MultConstraint (ModN n Integer) = ModN n Integer
  MkModN x .*. MkModN y = MkModN (x * y)

-- | @since 0.1.0.0
instance KnownNat n => MSemigroup (ModN n Natural) where
  type MultConstraint (ModN n Natural) = ModN n Natural
  MkModN x .*. MkModN y = MkModN (x * y)

-- | @since 0.1.0.0
instance KnownNat p => MSemigroup (ModP p Integer) where
  type MultConstraint (ModP p Integer) = ModP p Integer
  MkModP x .*. MkModP y = ModP.reallyUnsafeModP (x * y)

-- | @since 0.1.0.0
instance KnownNat p => MSemigroup (ModP p Natural) where
  type MultConstraint (ModP p Natural) = ModP p Natural
  MkModP x .*. MkModP y = ModP.reallyUnsafeModP (x * y)

-- | @since 0.1.0.0
instance (Eq a, Num a) => MSemigroup (NonNegative a) where
  type MultConstraint (NonNegative a) = NonNegative a
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance (Eq a, Num a) => MSemigroup (NonZero a) where
  type MultConstraint (NonZero a) = NonZero a
  MkNonZero x .*. MkNonZero y = reallyUnsafeNonZero $ x * y

-- | @since 0.1.0.0
instance (Eq a, Num a) => MSemigroup (Positive a) where
  type MultConstraint (Positive a) = Positive a
  MkPositive x .*. MkPositive y = reallyUnsafePositive $ x * y
