-- | Provides the 'ASemigroup' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Additive.ASemigroup
  ( ASemigroup (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines an additive semigroup.
--
-- @since 0.1
class Eq s => ASemigroup s where
  -- | @since 0.1
  (.+.) :: s -> s -> s

  -- | @since 0.1
  aabs :: s -> s

infixl 6 .+.

-- | @since 0.1
instance ASemigroup Double where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Float where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Int where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Int8 where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Int16 where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Int32 where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Int64 where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Integer where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Word where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Word8 where
  aabs = abs
  (.+.) = (+)

-- | @since 0.1
instance ASemigroup Word16 where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Word32 where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Word64 where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup Natural where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup (Ratio Integer) where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup (Ratio Natural) where
  (.+.) = (+)
  aabs = abs

-- | @since 0.1
instance ASemigroup a => ASemigroup (a, a) where
  (x1, x2) .+. (y1, y2) = (x1 .+. y1, x2 .+. y2)
  aabs (x1, x2) = (aabs x1, aabs x2)

-- | @since 0.1
instance ASemigroup a => ASemigroup (a, a, a) where
  (x1, x2, x3) .+. (y1, y2, y3) = (x1 .+. y1, x2 .+. y2, x3 .+. y3)
  aabs (x1, x2, x3) = (aabs x1, aabs x2, aabs x3)

-- | @since 0.1
instance ASemigroup a => ASemigroup (a, a, a, a) where
  (x1, x2, x3, x4) .+. (y1, y2, y3, y4) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4
    )
  aabs (x1, x2, x3, x4) = (aabs x1, aabs x2, aabs x3, aabs x4)

-- | @since 0.1
instance ASemigroup a => ASemigroup (a, a, a, a, a) where
  (x1, x2, x3, x4, x5) .+. (y1, y2, y3, y4, y5) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5
    )
  aabs (x1, x2, x3, x4, x5) = (aabs x1, aabs x2, aabs x3, aabs x4, aabs x5)


-- | @since 0.1
instance ASemigroup a => ASemigroup (a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6) .+. (y1, y2, y3, y4, y5, y6) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6
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
instance ASemigroup a => ASemigroup (a, a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6, x7) .+. (y1, y2, y3, y4, y5, y6, y7) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6,
      x7 .+. y7
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
instance ASemigroup a => ASemigroup (a, a, a, a, a, a, a, a) where
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
instance ASemigroup a => ASemigroup (a, a, a, a, a, a, a, a, a) where
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
