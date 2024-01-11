-- | Provides typeclasses for division.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MGroup
  ( MGroup (..),
  )
where

import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Real (Ratio)
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))

-- | Defines a multiplicative group.
--
-- @since 0.1
type MGroup :: Type -> Constraint
class (MMonoid g) => MGroup g where
  -- | @since 0.1
  (.%.) :: g -> g -> g

infixl 7 .%.

-- | @since 0.1
instance MGroup Double where
  (.%.) = (/)
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Float where
  (.%.) = (/)
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Int where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Int8 where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Int16 where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Int32 where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Int64 where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Integer where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Word where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Word8 where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Word16 where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Word32 where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Word64 where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup Natural where
  (.%.) = div
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup (Ratio Integer) where
  x .%. d = x .*. recip d
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance MGroup (Ratio Natural) where
  x .%. d = x .*. recip d
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance (RealFloat a) => MGroup (Complex a) where
  (.%.) = (/)
  {-# INLINE (.%.) #-}
