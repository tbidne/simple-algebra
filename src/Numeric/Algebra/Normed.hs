-- | Provides the 'Normed' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Normed
  ( Normed (..),
  )
where

import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines a \"norm\" on a given structure. We have the following laws:
--
-- * Positive definiteness: @norm x > 0 for x /= 0, norm 0 = 0@.
-- * Subadditivity: @norm (x + y) <= norm x + norm y@.
-- * Inversion : @norm x = norm (-x)@.
--
-- Notice these laws require various algebraic assumptions, from a semigroup
-- up to a group structure. It is expected that types that implement
-- 'Numeric.Algebra.Additive.ASemigroup.ASemigroup',
-- 'Numeric.Algebra.Additive.ASemigroup.AMonoid', and
-- 'Numeric.Algebra.Additive.ASemigroup.AGroup' will follow the respective laws.
--
-- We do not /require/ any of these typeclasses as constraints as it is
-- possible that we may want to abstract over "positive" types with a type
-- that does not have an additive semigroup instance (consider multiplicative
-- groups).
--
-- @since 0.1
type Normed :: Type -> Constraint
class Normed s where
  -- | @since 0.1
  norm :: s -> s

-- | @since 0.1
instance Normed Double where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Float where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Int where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Int8 where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Int16 where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Int32 where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Int64 where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Integer where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Word where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Word8 where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Word16 where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Word32 where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Word64 where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed Natural where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed (Ratio Integer) where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance Normed (Ratio Natural) where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance (RealFloat a) => Normed (Complex a) where
  norm = abs
  {-# INLINE norm #-}

-- | @since 0.1
instance (Normed a) => Normed (a, a) where
  norm (x1, x2) = (norm x1, norm x2)
  {-# INLINE norm #-}

-- | @since 0.1
instance (Normed a) => Normed (a, a, a) where
  norm (x1, x2, x3) = (norm x1, norm x2, norm x3)
  {-# INLINE norm #-}

-- | @since 0.1
instance (Normed a) => Normed (a, a, a, a) where
  norm (x1, x2, x3, x4) = (norm x1, norm x2, norm x3, norm x4)
  {-# INLINE norm #-}

-- | @since 0.1
instance (Normed a) => Normed (a, a, a, a, a) where
  norm (x1, x2, x3, x4, x5) = (norm x1, norm x2, norm x3, norm x4, norm x5)
  {-# INLINE norm #-}

-- | @since 0.1
instance (Normed a) => Normed (a, a, a, a, a, a) where
  norm (x1, x2, x3, x4, x5, x6) =
    ( norm x1,
      norm x2,
      norm x3,
      norm x4,
      norm x5,
      norm x6
    )
  {-# INLINE norm #-}

-- | @since 0.1
instance (Normed a) => Normed (a, a, a, a, a, a, a) where
  norm (x1, x2, x3, x4, x5, x6, x7) =
    ( norm x1,
      norm x2,
      norm x3,
      norm x4,
      norm x5,
      norm x6,
      norm x7
    )
  {-# INLINE norm #-}

-- | @since 0.1
instance (Normed a) => Normed (a, a, a, a, a, a, a, a) where
  norm (x1, x2, x3, x4, x5, x6, x7, x8) =
    ( norm x1,
      norm x2,
      norm x3,
      norm x4,
      norm x5,
      norm x6,
      norm x7,
      norm x8
    )
  {-# INLINE norm #-}

-- | @since 0.1
instance (Normed a) => Normed (a, a, a, a, a, a, a, a, a) where
  norm (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
    ( norm x1,
      norm x2,
      norm x3,
      norm x4,
      norm x5,
      norm x6,
      norm x7,
      norm x8,
      norm x9
    )
  {-# INLINE norm #-}
