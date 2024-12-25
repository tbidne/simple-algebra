-- | Provides typeclass for euclidean division.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MEuclidean
  ( MEuclidean (..),
    mdiv,
    mmod,
    mgcd,
    mlcm,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Multiplicative.MGroup (MGroup)
import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
import Numeric.Algebra.Normed (Normed (norm))

-- | 'MGroup' equipped with "euclidean" division.
--
--  @since 0.1
type MEuclidean :: Type -> Constraint
class (MGroup g) => MEuclidean g where
  -- | @since 0.1
  mdivMod :: g -> g -> (g, g)

-- | @since 0.1
mdiv :: (MEuclidean g) => g -> g -> g
mdiv x d = fst $ mdivMod x d
{-# INLINE mdiv #-}

-- | @since 0.1
mmod :: (MEuclidean g) => g -> g -> g
mmod x d = snd $ mdivMod x d
{-# INLINE mmod #-}

-- | @since 0.1
mgcd :: (AMonoid g, Eq g, MEuclidean g, Normed g) => g -> g -> g
mgcd x y = gcd' (norm x) (norm y)
  where
    gcd' a b =
      if b == zero
        then a
        else gcd' b (a `mmod` b)
{-# INLINE mgcd #-}

-- | @since 0.1
mlcm :: (AMonoid g, Eq g, MEuclidean g, Normed g) => g -> g -> g
mlcm x y
  | x == zero = zero
  | y == zero = zero
  | otherwise = norm (x `mdiv` mgcd x y .*. y)
{-# INLINE mlcm #-}

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
