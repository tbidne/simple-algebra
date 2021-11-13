-- | Provides the 'Additive' typeclass.
--
-- @since 0.1.0.0
module Simple.Algebra.Additive
  ( Additive (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines an additive semigroup.
--
-- @since 0.1.0.0
class Eq g => Additive g where
  -- | @since 0.1.0.0
  (.+.) :: g -> g -> g

infixl 6 .+.

-- | @since 0.1.0.0
instance Additive Double where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Float where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Int where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Int8 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Int16 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Int32 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Int64 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Integer where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Natural where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Word where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Word8 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Word16 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Word32 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Word64 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Integral a => Additive (Ratio a) where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive a => Additive (a, a) where
  (x1, x2) .+. (y1, y2) = (x1 .+. y1, x2 .+. y2)

-- | @since 0.1.0.0
instance Additive a => Additive (a, a, a) where
  (x1, x2, x3) .+. (y1, y2, y3) = (x1 .+. y1, x2 .+. y2, x3 .+. y3)

-- | @since 0.1.0.0
instance Additive a => Additive (a, a, a, a) where
  (x1, x2, x3, x4) .+. (y1, y2, y3, y4) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4
    )

-- | @since 0.1.0.0
instance Additive a => Additive (a, a, a, a, a) where
  (x1, x2, x3, x4, x5) .+. (y1, y2, y3, y4, y5) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5
    )

-- | @since 0.1.0.0
instance Additive a => Additive (a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6) .+. (y1, y2, y3, y4, y5, y6) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6
    )

-- | @since 0.1.0.0
instance Additive a => Additive (a, a, a, a, a, a, a) where
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
instance Additive a => Additive (a, a, a, a, a, a, a, a) where
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
instance Additive a => Additive (a, a, a, a, a, a, a, a, a) where
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
