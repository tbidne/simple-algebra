-- | Provides the 'AGroup' typeclass.
--
-- @since 0.1.0.0
module Numeric.Algebra.Additive.AGroup
  ( AGroup (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Num (Natural)
import GHC.Real (Ratio (..))
import GHC.TypeNats (KnownNat)
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Data.Fraction (Fraction (..))
import Numeric.Data.ModN (ModN (..))

-- | Defines an additive group.
--
-- @since 0.1.0.0
class AMonoid g => AGroup g where
  -- | Possible constraint on the second argument to '(.-.)' e.g. for
  -- preventing underflow.
  --
  -- @since 0.1.0.0
  type SubtractConstraint g

  -- | @since 0.1.0.0
  (.-.) :: g -> SubtractConstraint g -> g

  -- | Returns @|x|@. Should satisfy
  --
  -- @
  -- Non-negative: aabs x >= zero
  -- Positive-definite: aabs x = 0 <=> x = 0
  -- Triangle equality: aabs (x .+. y) <= aabs x .+. aabs y
  -- @
  --
  -- @since 0.1.0.0
  aabs :: g -> g

infixl 6 .-.

-- | @since 0.1.0.0
instance AGroup Double where
  type SubtractConstraint Double = Double
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Float where
  type SubtractConstraint Float = Float
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Int where
  type SubtractConstraint Int = Int
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Int8 where
  type SubtractConstraint Int8 = Int8
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Int16 where
  type SubtractConstraint Int16 = Int16
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Int32 where
  type SubtractConstraint Int32 = Int32
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Int64 where
  type SubtractConstraint Int64 = Int64
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Integer where
  type SubtractConstraint Integer = Integer
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Word where
  type SubtractConstraint Word = Word
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Word8 where
  type SubtractConstraint Word8 = Word8
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Word16 where
  type SubtractConstraint Word16 = Word16
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Word32 where
  type SubtractConstraint Word32 = Word32
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup Word64 where
  type SubtractConstraint Word64 = Word64
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup (Ratio Integer) where
  type SubtractConstraint (Ratio Integer) = Ratio Integer
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a) where
  type SubtractConstraint (a, a) = (SubtractConstraint a, SubtractConstraint a)
  (x1, x2) .-. (y1, y2) = (x1 .-. y1, x2 .-. y2)
  aabs (x1, x2) = (aabs x1, aabs x2)

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a) where
  type
    SubtractConstraint (a, a, a) =
      ( SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a
      )
  (x1, x2, x3) .-. (y1, y2, y3) = (x1 .-. y1, x2 .-. y2, x3 .-. y3)
  aabs (x1, x2, x3) = (aabs x1, aabs x2, aabs x3)

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a, a) where
  type
    SubtractConstraint (a, a, a, a) =
      ( SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a
      )
  (x1, x2, x3, x4) .-. (y1, y2, y3, y4) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4
    )
  aabs (x1, x2, x3, x4) = (aabs x1, aabs x2, aabs x3, aabs x4)

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a, a, a) where
  type
    SubtractConstraint (a, a, a, a, a) =
      ( SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a
      )
  (x1, x2, x3, x4, x5) .-. (y1, y2, y3, y4, y5) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5
    )
  aabs (x1, x2, x3, x4, x5) = (aabs x1, aabs x2, aabs x3, aabs x4, aabs x5)

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a, a, a, a) where
  type
    SubtractConstraint (a, a, a, a, a, a) =
      ( SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a
      )
  (x1, x2, x3, x4, x5, x6) .-. (y1, y2, y3, y4, y5, y6) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6
    )
  aabs (x1, x2, x3, x4, x5, x6) =
    ( aabs x1,
      aabs x2,
      aabs x3,
      aabs x4,
      aabs x5,
      aabs x6
    )

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a, a, a, a, a) where
  type
    SubtractConstraint (a, a, a, a, a, a, a) =
      ( SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a
      )
  (x1, x2, x3, x4, x5, x6, x7) .-. (y1, y2, y3, y4, y5, y6, y7) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6,
      x7 .-. y7
    )
  aabs (x1, x2, x3, x4, x5, x6, x7) =
    ( aabs x1,
      aabs x2,
      aabs x3,
      aabs x4,
      aabs x5,
      aabs x6,
      aabs x7
    )

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a, a, a, a, a, a) where
  type
    SubtractConstraint (a, a, a, a, a, a, a, a) =
      ( SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a
      )
  (x1, x2, x3, x4, x5, x6, x7, x8) .-. (y1, y2, y3, y4, y5, y6, y7, y8) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6,
      x7 .-. y7,
      x8 .-. y8
    )
  aabs (x1, x2, x3, x4, x5, x6, x7, x8) =
    ( aabs x1,
      aabs x2,
      aabs x3,
      aabs x4,
      aabs x5,
      aabs x6,
      aabs x7,
      aabs x8
    )

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a, a, a, a, a, a, a) where
  type
    SubtractConstraint (a, a, a, a, a, a, a, a, a) =
      ( SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a,
        SubtractConstraint a
      )
  (x1, x2, x3, x4, x5, x6, x7, x8, x9) .-. (y1, y2, y3, y4, y5, y6, y7, y8, y9) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6,
      x7 .-. y7,
      x8 .-. y8,
      x9 .-. y9
    )
  aabs (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
    ( aabs x1,
      aabs x2,
      aabs x3,
      aabs x4,
      aabs x5,
      aabs x6,
      aabs x7,
      aabs x8,
      aabs x9
    )

-- | @since 0.1.0.0
instance AGroup (Fraction Integer) where
  type SubtractConstraint (Fraction Integer) = Fraction Integer
  (.-.) = (-)
  aabs = abs

instance KnownNat n => AGroup (ModN n Int) where
  type SubtractConstraint (ModN n Int) = ModN n Int
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Int8) where
  type SubtractConstraint (ModN n Int8) = ModN n Int8
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Int16) where
  type SubtractConstraint (ModN n Int16) = ModN n Int16
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Int32) where
  type SubtractConstraint (ModN n Int32) = ModN n Int32
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Int64) where
  type SubtractConstraint (ModN n Int64) = ModN n Int64
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Integer) where
  type SubtractConstraint (ModN n Integer) = ModN n Integer
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Word) where
  type SubtractConstraint (ModN n Word) = ModN n Word
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Word8) where
  type SubtractConstraint (ModN n Word8) = ModN n Word8
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Word16) where
  type SubtractConstraint (ModN n Word16) = ModN n Word16
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Word32) where
  type SubtractConstraint (ModN n Word32) = ModN n Word32
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Word64) where
  type SubtractConstraint (ModN n Word64) = ModN n Word64
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id

instance KnownNat n => AGroup (ModN n Natural) where
  type SubtractConstraint (ModN n Natural) = ModN n Natural
  MkModN x .-. MkModN y = MkModN (x + y)
  aabs = id
