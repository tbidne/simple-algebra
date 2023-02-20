-- | Provides the 'MSemigroup' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MSemigroup
  ( MSemigroup (..),
  )
where

import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines a multiplicative semigroup.
--
-- @since 0.1
type MSemigroup :: Type -> Constraint
class MSemigroup s where
  -- | @since 0.1
  (.*.) :: s -> s -> s

infixl 7 .*.

-- | @since 0.1
instance MSemigroup Double where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Float where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Int where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Int8 where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Int16 where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Int32 where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Int64 where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Integer where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Word where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Word8 where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Word16 where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Word32 where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Word64 where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup Natural where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup (Ratio Integer) where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance MSemigroup (Ratio Natural) where
  (.*.) = (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance (RealFloat a) => MSemigroup (Complex a) where
  (.*.) = (*)
  {-# INLINE (.*.) #-}
