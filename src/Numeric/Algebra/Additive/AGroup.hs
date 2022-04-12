-- | Provides the 'AGroup' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Additive.AGroup
  ( AGroup (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Real (Ratio (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))

-- | Defines an additive group.
--
-- @since 0.1
class AMonoid g => AGroup g where
  -- | @since 0.1
  (.-.) :: g -> g -> g

  -- | Returns @|x|@. Should satisfy
  --
  -- @
  -- Non-negative: aabs x >= zero
  -- Positive-definite: aabs x = 0 <=> x = 0
  -- Triangle equality: aabs (x .+. y) <= aabs x .+. aabs y
  -- @
  --
  -- @since 0.1
  aabs :: g -> g

infixl 6 .-.

-- | @since 0.1
instance AGroup Double where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Float where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Int where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Int8 where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Int16 where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Int32 where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Int64 where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Integer where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Word where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Word8 where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Word16 where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Word32 where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup Word64 where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup (Ratio Integer) where
  (.-.) = (-)
  aabs = abs

-- | @since 0.1
instance AGroup a => AGroup (a, a) where
  (x1, x2) .-. (y1, y2) = (x1 .-. y1, x2 .-. y2)
  aabs (x1, x2) = (aabs x1, aabs x2)

-- | @since 0.1
instance AGroup a => AGroup (a, a, a) where
  (x1, x2, x3) .-. (y1, y2, y3) = (x1 .-. y1, x2 .-. y2, x3 .-. y3)
  aabs (x1, x2, x3) = (aabs x1, aabs x2, aabs x3)

-- | @since 0.1
instance AGroup a => AGroup (a, a, a, a) where
  (x1, x2, x3, x4) .-. (y1, y2, y3, y4) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4
    )
  aabs (x1, x2, x3, x4) = (aabs x1, aabs x2, aabs x3, aabs x4)

-- | @since 0.1
instance AGroup a => AGroup (a, a, a, a, a) where
  (x1, x2, x3, x4, x5) .-. (y1, y2, y3, y4, y5) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5
    )
  aabs (x1, x2, x3, x4, x5) = (aabs x1, aabs x2, aabs x3, aabs x4, aabs x5)

-- | @since 0.1
instance AGroup a => AGroup (a, a, a, a, a, a) where
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

-- | @since 0.1
instance AGroup a => AGroup (a, a, a, a, a, a, a) where
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

-- | @since 0.1
instance AGroup a => AGroup (a, a, a, a, a, a, a, a) where
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

-- | @since 0.1
instance AGroup a => AGroup (a, a, a, a, a, a, a, a, a) where
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
