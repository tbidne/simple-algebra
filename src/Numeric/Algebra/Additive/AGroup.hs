-- | Provides the 'AGroup' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Additive.AGroup
  ( AGroup (..),
    anegate,
  )
where

import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))

-- | Defines an additive group.
--
-- @since 0.1
type AGroup :: Type -> Constraint
class (AMonoid g) => AGroup g where
  -- | @since 0.1
  (.-.) :: g -> g -> g

infixl 6 .-.

-- | @since 0.1
anegate :: (AGroup g) => g -> g
anegate n = zero .-. n
{-# INLINE anegate #-}

-- | @since 0.1
instance AGroup Double where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Float where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Int where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Int8 where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Int16 where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Int32 where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Int64 where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Integer where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Word where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Word8 where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Word16 where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Word32 where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup Word64 where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance AGroup (Ratio Integer) where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (RealFloat a) => AGroup (Complex a) where
  (.-.) = (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a) where
  (x1, x2) .-. (y1, y2) = (x1 .-. y1, x2 .-. y2)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a) where
  (x1, x2, x3) .-. (y1, y2, y3) = (x1 .-. y1, x2 .-. y2, x3 .-. y3)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a) where
  (x1, x2, x3, x4) .-. (y1, y2, y3, y4) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4
    )
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a, a) where
  (x1, x2, x3, x4, x5) .-. (y1, y2, y3, y4, y5) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5
    )
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6) .-. (y1, y2, y3, y4, y5, y6) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6
    )
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6, x7) .-. (y1, y2, y3, y4, y5, y6, y7) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6,
      x7 .-. y7
    )
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a, a, a, a, a) where
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
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a, a, a, a, a, a) where
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
  {-# INLINE (.-.) #-}
