-- | Provides the 'ASemigroup' typeclass.
--
-- @since 0.1.0.0
module Numeric.Algebra.Additive.ASemigroup
  ( ASemigroup (..),
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
import Numeric.Data.Negative (Negative (..), reallyUnsafeNegative)
import Numeric.Data.NonNegative (NonNegative (..), reallyUnsafeNonNegative)
import Numeric.Data.NonPositive (NonPositive (..), reallyUnsafeNonPositive)
import Numeric.Data.NonZero (NonZero (..), reallyUnsafeNonZero)
import Numeric.Data.Positive (Positive (..), reallyUnsafePositive)

-- | Defines an additive semigroup.
--
-- @since 0.1.0.0
class Eq s => ASemigroup s where
  -- | Possible constraint on the second argument to '(.+.)' e.g. for
  -- preventing overflow.
  --
  -- @since 0.1.0.0
  type AddConstraint s

  -- | @since 0.1.0.0
  (.+.) :: s -> AddConstraint s -> s

infixl 6 .+.

-- | @since 0.1.0.0
instance ASemigroup Double where
  type AddConstraint Double = Double
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Float where
  type AddConstraint Float = Float
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Int where
  type AddConstraint Int = Int
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Int8 where
  type AddConstraint Int8 = Int8
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Int16 where
  type AddConstraint Int16 = Int16
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Int32 where
  type AddConstraint Int32 = Int32
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Int64 where
  type AddConstraint Int64 = Int64
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Integer where
  type AddConstraint Integer = Integer
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Word where
  type AddConstraint Word = Word
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Word8 where
  type AddConstraint Word8 = Word8
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Word16 where
  type AddConstraint Word16 = Word16
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Word32 where
  type AddConstraint Word32 = Word32
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Word64 where
  type AddConstraint Word64 = Word64
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup Natural where
  type AddConstraint Natural = Natural
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup (Ratio Integer) where
  type AddConstraint (Ratio Integer) = Ratio Integer
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup (Ratio Natural) where
  type AddConstraint (Ratio Natural) = Ratio Natural
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup a => ASemigroup (a, a) where
  type AddConstraint (a, a) = (AddConstraint a, AddConstraint a)
  (x1, x2) .+. (y1, y2) = (x1 .+. y1, x2 .+. y2)

-- | @since 0.1.0.0
instance ASemigroup a => ASemigroup (a, a, a) where
  type
    AddConstraint (a, a, a) =
      ( AddConstraint a,
        AddConstraint a,
        AddConstraint a
      )
  (x1, x2, x3) .+. (y1, y2, y3) = (x1 .+. y1, x2 .+. y2, x3 .+. y3)

-- | @since 0.1.0.0
instance ASemigroup a => ASemigroup (a, a, a, a) where
  type
    AddConstraint (a, a, a, a) =
      ( AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a
      )
  (x1, x2, x3, x4) .+. (y1, y2, y3, y4) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4
    )

-- | @since 0.1.0.0
instance ASemigroup a => ASemigroup (a, a, a, a, a) where
  type
    AddConstraint (a, a, a, a, a) =
      ( AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a
      )
  (x1, x2, x3, x4, x5) .+. (y1, y2, y3, y4, y5) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5
    )

-- | @since 0.1.0.0
instance ASemigroup a => ASemigroup (a, a, a, a, a, a) where
  type
    AddConstraint (a, a, a, a, a, a) =
      ( AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a
      )
  (x1, x2, x3, x4, x5, x6) .+. (y1, y2, y3, y4, y5, y6) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6
    )

-- | @since 0.1.0.0
instance ASemigroup a => ASemigroup (a, a, a, a, a, a, a) where
  type
    AddConstraint (a, a, a, a, a, a, a) =
      ( AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a
      )
  (x1, x2, x3, x4, x5, x6, x7) .+. (y1, y2, y3, y4, y5, y6, y7) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6,
      x7 .+. y7
    )

-- | @since 0.1.0.0
instance ASemigroup a => ASemigroup (a, a, a, a, a, a, a, a) where
  type
    AddConstraint (a, a, a, a, a, a, a, a) =
      ( AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a
      )
  (x1, x2, x3, x4, x5, x6, x7, x8) .+. (y1, y2, y3, y4, y5, y6, y7, y8) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6,
      x7 .+. y7,
      x8 .+. y8
    )

-- | @since 0.1.0.0
instance ASemigroup a => ASemigroup (a, a, a, a, a, a, a, a, a) where
  type
    AddConstraint (a, a, a, a, a, a, a, a, a) =
      ( AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a,
        AddConstraint a
      )
  (x1, x2, x3, x4, x5, x6, x7, x8, x9) .+. (y1, y2, y3, y4, y5, y6, y7, y8, y9) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6,
      x7 .+. y7,
      x8 .+. y8,
      x9 .+. y9
    )

-- | @since 0.1.0.0
instance ASemigroup (Fraction Integer) where
  type AddConstraint (Fraction Integer) = Fraction Integer
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup (Fraction Natural) where
  type AddConstraint (Fraction Natural) = Fraction Natural
  (.+.) = (+)

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Int) where
  type AddConstraint (ModN n Int) = ModN n Int
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Int8) where
  type AddConstraint (ModN n Int8) = ModN n Int8
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Int16) where
  type AddConstraint (ModN n Int16) = ModN n Int16
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Int32) where
  type AddConstraint (ModN n Int32) = ModN n Int32
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Int64) where
  type AddConstraint (ModN n Int64) = ModN n Int64
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Integer) where
  type AddConstraint (ModN n Integer) = ModN n Integer
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Word) where
  type AddConstraint (ModN n Word) = ModN n Word
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Word8) where
  type AddConstraint (ModN n Word8) = ModN n Word8
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Word16) where
  type AddConstraint (ModN n Word16) = ModN n Word16
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Word32) where
  type AddConstraint (ModN n Word32) = ModN n Word32
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Word64) where
  type AddConstraint (ModN n Word64) = ModN n Word64
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Natural) where
  type AddConstraint (ModN n Natural) = ModN n Natural
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Int) where
  type AddConstraint (ModP p Int) = ModP p Int
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Int8) where
  type AddConstraint (ModP p Int8) = ModP p Int8
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Int16) where
  type AddConstraint (ModP p Int16) = ModP p Int16
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Int32) where
  type AddConstraint (ModP p Int32) = ModP p Int32
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Int64) where
  type AddConstraint (ModP p Int64) = ModP p Int64
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Integer) where
  type AddConstraint (ModP p Integer) = ModP p Integer
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Word) where
  type AddConstraint (ModP p Word) = ModP p Word
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Word8) where
  type AddConstraint (ModP p Word8) = ModP p Word8
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Word16) where
  type AddConstraint (ModP p Word16) = ModP p Word16
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Word32) where
  type AddConstraint (ModP p Word32) = ModP p Word32
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Word64) where
  type AddConstraint (ModP p Word64) = ModP p Word64
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance KnownNat p => ASemigroup (ModP p Natural) where
  type AddConstraint (ModP p Natural) = ModP p Natural
  MkModP x .+. MkModP y = ModP.reallyUnsafeModP $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Float) where
  type AddConstraint (Negative Float) = Negative Float
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Double) where
  type AddConstraint (Negative Double) = Negative Double
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Int) where
  type AddConstraint (Negative Int) = Negative Int
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Int8) where
  type AddConstraint (Negative Int8) = Negative Int8
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Int16) where
  type AddConstraint (Negative Int16) = Negative Int16
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Int32) where
  type AddConstraint (Negative Int32) = Negative Int32
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Int64) where
  type AddConstraint (Negative Int64) = Negative Int64
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Integer) where
  type AddConstraint (Negative Integer) = Negative Integer
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Word) where
  type AddConstraint (Negative Word) = Negative Word
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Word8) where
  type AddConstraint (Negative Word8) = Negative Word8
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Word16) where
  type AddConstraint (Negative Word16) = Negative Word16
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Word32) where
  type AddConstraint (Negative Word32) = Negative Word32
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Word64) where
  type AddConstraint (Negative Word64) = Negative Word64
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Negative Natural) where
  type AddConstraint (Negative Natural) = Negative Natural
  MkNegative x .+. MkNegative y = reallyUnsafeNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Float) where
  type AddConstraint (NonNegative Float) = NonNegative Float
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Double) where
  type AddConstraint (NonNegative Double) = NonNegative Double
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Int) where
  type AddConstraint (NonNegative Int) = NonNegative Int
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Int8) where
  type AddConstraint (NonNegative Int8) = NonNegative Int8
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Int16) where
  type AddConstraint (NonNegative Int16) = NonNegative Int16
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Int32) where
  type AddConstraint (NonNegative Int32) = NonNegative Int32
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Int64) where
  type AddConstraint (NonNegative Int64) = NonNegative Int64
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Integer) where
  type AddConstraint (NonNegative Integer) = NonNegative Integer
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Word) where
  type AddConstraint (NonNegative Word) = NonNegative Word
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Word8) where
  type AddConstraint (NonNegative Word8) = NonNegative Word8
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Word16) where
  type AddConstraint (NonNegative Word16) = NonNegative Word16
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Word32) where
  type AddConstraint (NonNegative Word32) = NonNegative Word32
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Word64) where
  type AddConstraint (NonNegative Word64) = NonNegative Word64
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonNegative Natural) where
  type AddConstraint (NonNegative Natural) = NonNegative Natural
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Float) where
  type AddConstraint (NonPositive Float) = NonPositive Float
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Double) where
  type AddConstraint (NonPositive Double) = NonPositive Double
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Int) where
  type AddConstraint (NonPositive Int) = NonPositive Int
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Int8) where
  type AddConstraint (NonPositive Int8) = NonPositive Int8
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Int16) where
  type AddConstraint (NonPositive Int16) = NonPositive Int16
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Int32) where
  type AddConstraint (NonPositive Int32) = NonPositive Int32
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Int64) where
  type AddConstraint (NonPositive Int64) = NonPositive Int64
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Integer) where
  type AddConstraint (NonPositive Integer) = NonPositive Integer
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Word) where
  type AddConstraint (NonPositive Word) = NonPositive Word
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Word8) where
  type AddConstraint (NonPositive Word8) = NonPositive Word8
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Word16) where
  type AddConstraint (NonPositive Word16) = NonPositive Word16
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Word32) where
  type AddConstraint (NonPositive Word32) = NonPositive Word32
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Word64) where
  type AddConstraint (NonPositive Word64) = NonPositive Word64
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonPositive Natural) where
  type AddConstraint (NonPositive Natural) = NonPositive Natural
  MkNonPositive x .+. MkNonPositive y = reallyUnsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonZero Word) where
  type AddConstraint (NonZero Word) = NonZero Word
  MkNonZero x .+. MkNonZero y = reallyUnsafeNonZero $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonZero Word8) where
  type AddConstraint (NonZero Word8) = NonZero Word8
  MkNonZero x .+. MkNonZero y = reallyUnsafeNonZero $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonZero Word16) where
  type AddConstraint (NonZero Word16) = NonZero Word16
  MkNonZero x .+. MkNonZero y = reallyUnsafeNonZero $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonZero Word32) where
  type AddConstraint (NonZero Word32) = NonZero Word32
  MkNonZero x .+. MkNonZero y = reallyUnsafeNonZero $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonZero Word64) where
  type AddConstraint (NonZero Word64) = NonZero Word64
  MkNonZero x .+. MkNonZero y = reallyUnsafeNonZero $ x + y

-- | @since 0.1.0.0
instance ASemigroup (NonZero Natural) where
  type AddConstraint (NonZero Natural) = NonZero Natural
  MkNonZero x .+. MkNonZero y = reallyUnsafeNonZero $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Float) where
  type AddConstraint (Positive Float) = Positive Float
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Double) where
  type AddConstraint (Positive Double) = Positive Double
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Int) where
  type AddConstraint (Positive Int) = Positive Int
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Int8) where
  type AddConstraint (Positive Int8) = Positive Int8
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Int16) where
  type AddConstraint (Positive Int16) = Positive Int16
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Int32) where
  type AddConstraint (Positive Int32) = Positive Int32
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Int64) where
  type AddConstraint (Positive Int64) = Positive Int64
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Integer) where
  type AddConstraint (Positive Integer) = Positive Integer
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Word) where
  type AddConstraint (Positive Word) = Positive Word
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Word8) where
  type AddConstraint (Positive Word8) = Positive Word8
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Word16) where
  type AddConstraint (Positive Word16) = Positive Word16
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Word32) where
  type AddConstraint (Positive Word32) = Positive Word32
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Word64) where
  type AddConstraint (Positive Word64) = Positive Word64
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y

-- | @since 0.1.0.0
instance ASemigroup (Positive Natural) where
  type AddConstraint (Positive Natural) = Positive Natural
  MkPositive x .+. MkPositive y = reallyUnsafePositive $ x + y
