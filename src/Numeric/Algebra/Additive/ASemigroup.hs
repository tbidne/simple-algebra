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
import Numeric.Data.Fraction (Fraction)
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
