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
-- * Sign : @sgn x = x / norm x@.
--
-- Notice these laws require various algebraic assumptions, from a semigroup
-- up to a group structure. It is expected that types that implement
-- the relevant typeclasses will follow the respective laws.
--
-- We do not /require/ any of these typeclasses as constraints as it is
-- possible that we may want to abstract over "positive" types with a type
-- that does not have an additive semigroup instance (consider multiplicative
-- groups).
--
-- @since 0.1
type Normed :: Type -> Constraint
class Normed s where
  -- | Returns the norm.
  --
  -- @since 0.1
  norm :: s -> s

  -- | Sign function. Notice that when 'norm' is 'abs', this is the familiar
  -- 'signum' function i.e.
  --
  -- @
  -- sgn x
  --   | x < 0 = -1
  --   | x == 0 = 0
  --   | x > 0 = 1
  -- @
  --
  -- @since 0.1
  sgn :: s -> s

-- | @since 0.1
instance Normed Double where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Float where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Int where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Int8 where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Int16 where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Int32 where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Int64 where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Integer where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Word where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Word8 where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Word16 where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Word32 where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Word64 where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed Natural where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed (Ratio Integer) where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance Normed (Ratio Natural) where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}

-- | @since 0.1
instance (RealFloat a) => Normed (Complex a) where
  norm = abs
  {-# INLINE norm #-}
  sgn = signum
  {-# INLINE sgn #-}
