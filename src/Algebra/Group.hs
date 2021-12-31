-- | Provides the 'Group' typeclass.
--
-- @since 0.1.0.0
module Algebra.Group
  ( Group (..),
  )
where

import Algebra.Additive (Additive (..))
import Algebra.AdditiveMonoid (AdditiveMonoid (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Refined (Even, Refined)
import Refined.Extras qualified as RExtras

-- | Defines a group.
--
-- @since 0.1.0.0
class AdditiveMonoid g => Group g where
  -- | @since 0.1.0.0
  (.-.) :: g -> g -> g
  g .-. h = g .+. ginv h

  -- | @since 0.1.0.0
  ginv :: g -> g
  ginv g = zero .-. g

  -- | @since 0.1.0.0
  gabs :: g -> g

  {-# MINIMAL ((.-.) | ginv), gabs #-}

infixl 6 .-.

-- | @since 0.1.0.0
instance Group Double where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Float where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Int where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Int8 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Int16 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Int32 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Int64 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Integer where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance (Integral a) => Group (Ratio a) where
  (.-.) = (-)
  ginv = negate
  gabs = abs

-- | @since 0.1.0.0
instance Group a => Group (a, a) where
  (x1, x2) .-. (y1, y2) = (x1 .-. y1, x2 .-. y2)
  ginv (x1, x2) = (ginv x1, ginv x2)
  gabs (x1, x2) = (gabs x1, gabs x2)

-- | @since 0.1.0.0
instance Group a => Group (a, a, a) where
  (x1, x2, x3) .-. (y1, y2, y3) = (x1 .-. y1, x2 .-. y2, x3 .-. y3)
  ginv (x1, x2, x3) = (ginv x1, ginv x2, ginv x3)
  gabs (x1, x2, x3) = (gabs x1, gabs x2, gabs x3)

-- | @since 0.1.0.0
instance Group a => Group (a, a, a, a) where
  (x1, x2, x3, x4) .-. (y1, y2, y3, y4) = (x1 .-. y1, x2 .-. y2, x3 .-. y3, x4 .-. y4)
  ginv (x1, x2, x3, x4) = (ginv x1, ginv x2, ginv x3, ginv x4)
  gabs (x1, x2, x3, x4) = (gabs x1, gabs x2, gabs x3, gabs x4)

-- | @since 0.1.0.0
instance Group a => Group (a, a, a, a, a) where
  (x1, x2, x3, x4, x5) .-. (y1, y2, y3, y4, y5) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5
    )
  ginv (x1, x2, x3, x4, x5) = (ginv x1, ginv x2, ginv x3, ginv x4, ginv x5)
  gabs (x1, x2, x3, x4, x5) = (gabs x1, gabs x2, gabs x3, gabs x4, gabs x5)

-- | @since 0.1.0.0
instance Group a => Group (a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6) .-. (y1, y2, y3, y4, y5, y6) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6
    )
  ginv (x1, x2, x3, x4, x5, x6) =
    ( ginv x1,
      ginv x2,
      ginv x3,
      ginv x4,
      ginv x5,
      ginv x6
    )
  gabs (x1, x2, x3, x4, x5, x6) =
    ( gabs x1,
      gabs x2,
      gabs x3,
      gabs x4,
      gabs x5,
      gabs x6
    )

-- | @since 0.1.0.0
instance Group a => Group (a, a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6, x7) .-. (y1, y2, y3, y4, y5, y6, y7) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6,
      x7 .-. y7
    )
  ginv (x1, x2, x3, x4, x5, x6, x7) =
    ( ginv x1,
      ginv x2,
      ginv x3,
      ginv x4,
      ginv x5,
      ginv x6,
      ginv x7
    )
  gabs (x1, x2, x3, x4, x5, x6, x7) =
    ( gabs x1,
      gabs x2,
      gabs x3,
      gabs x4,
      gabs x5,
      gabs x6,
      gabs x7
    )

-- | @since 0.1.0.0
instance Group a => Group (a, a, a, a, a, a, a, a) where
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
  ginv (x1, x2, x3, x4, x5, x6, x7, x8) =
    ( ginv x1,
      ginv x2,
      ginv x3,
      ginv x4,
      ginv x5,
      ginv x6,
      ginv x7,
      ginv x8
    )
  gabs (x1, x2, x3, x4, x5, x6, x7, x8) =
    ( gabs x1,
      gabs x2,
      gabs x3,
      gabs x4,
      gabs x5,
      gabs x6,
      gabs x7,
      gabs x8
    )

-- | @since 0.1.0.0
instance Group a => Group (a, a, a, a, a, a, a, a, a) where
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
  ginv (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
    ( ginv x1,
      ginv x2,
      ginv x3,
      ginv x4,
      ginv x5,
      ginv x6,
      ginv x7,
      ginv x8,
      ginv x9
    )
  gabs (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
    ( gabs x1,
      gabs x2,
      gabs x3,
      gabs x4,
      gabs x5,
      gabs x6,
      gabs x7,
      gabs x8,
      gabs x9
    )

instance Integral a => Group (Refined Even a) where
  (.-.) = RExtras.unsafeLiftR2 (-)
  gabs = RExtras.unsafeLiftR abs
  ginv = RExtras.unsafeLiftR negate
