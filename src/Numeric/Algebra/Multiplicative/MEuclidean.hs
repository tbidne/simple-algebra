-- | Provides typeclass for euclidean division.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MEuclidean
  ( MEuclidean (..),
    mmod,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))

-- | 'MGroup' equipped with "euclidean" division.
--
--  @since 0.1
type MEuclidean :: Type -> Constraint
class (MGroup g) => MEuclidean g where
  -- | @since 0.1
  mdivMod :: g -> g -> (g, g)
  mdivMod x d = (x .%. d, x `mmod` d)
  {-# INLINE mdivMod #-}

-- | @since 0.1
mmod :: (MEuclidean g) => g -> g -> g
mmod x d = snd $ mdivMod x d
{-# INLINE mmod #-}

-- | @since 0.1
instance MEuclidean Int where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Int8 where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Int16 where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Int32 where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Int64 where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Integer where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Word where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Word8 where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Word16 where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Word32 where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Word64 where
  mdivMod = divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Natural where
  mdivMod = divMod
  {-# INLINE mdivMod #-}
