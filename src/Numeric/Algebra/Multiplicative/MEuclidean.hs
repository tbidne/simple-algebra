-- | Provides typeclass for euclidean division.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MEuclidean
  ( MEuclidean (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Algebra.Multiplicative.MGroup
  ( MGroup ((.%.)),
    NonZero (MkNonZero),
  )

-- | 'MGroup' equipped with "euclidean" division.
--
--  @since 0.1
type MEuclidean :: Type -> Constraint
class MGroup g => MEuclidean g where
  type ModResult g

  -- | @since 0.1
  mmod :: g -> NonZero g -> ModResult g
  mmod x d = snd $ mdivMod x d
  {-# INLINE mmod #-}

  -- | @since 0.1
  mdivMod :: g -> NonZero g -> (g, ModResult g)
  mdivMod x d = (x .%. d, x `mmod` d)
  {-# INLINE mdivMod #-}

  {-# MINIMAL (mdivMod | mmod) #-}

-- | @since 0.1
instance MEuclidean Int where
  type ModResult Int = Int
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Int8 where
  type ModResult Int8 = Int8
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Int16 where
  type ModResult Int16 = Int16
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Int32 where
  type ModResult Int32 = Int32
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Int64 where
  type ModResult Int64 = Int64
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Integer where
  type ModResult Integer = Integer
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Word where
  type ModResult Word = Word
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Word8 where
  type ModResult Word8 = Word8
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Word16 where
  type ModResult Word16 = Word16
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Word32 where
  type ModResult Word32 = Word32
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Word64 where
  type ModResult Word64 = Word64
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance MEuclidean Natural where
  type ModResult Natural = Natural
  x `mdivMod` MkNonZero d = x `divMod` d
  {-# INLINE mdivMod #-}
