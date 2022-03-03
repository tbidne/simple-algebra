-- | Provides the 'AGroup' typeclass.
--
-- @since 0.1.0.0
module Numeric.Algebra.Additive.AGroup
  ( AGroup (..),
    gabs,
    ginv,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Real (Ratio (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))

-- | Defines an additive group.
--
-- @since 0.1.0.0
class AMonoid g => AGroup g where
  -- | @since 0.1.0.0
  (.-.) :: g -> g -> g

infixl 6 .-.

-- | Returns @|x|@.
--
-- @since 0.1.0.0
gabs :: (Ord p, AGroup p) => p -> p
gabs x
  | x < zero = ginv x
  | otherwise = x

-- | Returns @zero - x@.
--
-- @since 0.1.0.0
ginv :: AGroup g => g -> g
ginv g = zero .-. g

-- | @since 0.1.0.0
instance AGroup Double where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Float where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Int where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Int8 where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Int16 where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Int32 where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Int64 where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Integer where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Word where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Word8 where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Word16 where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Word32 where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup Word64 where
  (.-.) = (-)

-- | NB. Differs from 'Ratio'\'s 'abs' in that we also apply 'abs' to the
-- denominator.
--
-- @since 0.1.0.0
instance AGroup (Ratio Integer) where
  (.-.) = (-)

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a) where
  (x1, x2) .-. (y1, y2) = (x1 .-. y1, x2 .-. y2)

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a) where
  (x1, x2, x3) .-. (y1, y2, y3) = (x1 .-. y1, x2 .-. y2, x3 .-. y3)

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a, a) where
  (x1, x2, x3, x4) .-. (y1, y2, y3, y4) = (x1 .-. y1, x2 .-. y2, x3 .-. y3, x4 .-. y4)

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a, a, a) where
  (x1, x2, x3, x4, x5) .-. (y1, y2, y3, y4, y5) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5
    )

-- | @since 0.1.0.0
instance AGroup a => AGroup (a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6) .-. (y1, y2, y3, y4, y5, y6) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6
    )

-- | @since 0.1.0.0
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

-- | @since 0.1.0.0
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

-- | @since 0.1.0.0
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
