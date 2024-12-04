-- | Provides the 'MetricSpace' typeclass.
--
-- @since 0.1
module Numeric.Algebra.MetricSpace
  ( MetricSpace (..),
  )
where

import Data.Complex (Complex, magnitude)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (float2Double)
import GHC.Natural (Natural)

-- | Defines a metric space. A metric is a function
-- \(d : M \times M \to \mathbb{R}\) s.t. for all \(x, y, z \in M\):
--
-- * \(d(x, x) = 0\)
-- * __Positivity__: If \(x \ne y\), then \(d(x, y) > 0\)
-- * __Symmetry__: \(d(x, y) = d(y, x)\)
-- * __Triangle equality__: \(d(x, z) \le d(x, y) + d(y, z) \)
--
-- @since 0.1
type MetricSpace :: Type -> Constraint
class MetricSpace s where
  -- | @since 0.1
  diff :: s -> s -> Double

-- | @since 0.1
instance MetricSpace Double where
  diff x y = abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Float where
  diff x y = float2Double $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Int where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Int8 where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Int16 where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Int32 where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Int64 where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Integer where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Word where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Word8 where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Word16 where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Word32 where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Word64 where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace Natural where
  diff x y = fromIntegral $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace (Ratio Integer) where
  diff x y = fromRational $ abs (y - x)
  {-# INLINE diff #-}

-- | @since 0.1
instance MetricSpace (Ratio Natural) where
  diff x y
    | x <= y = realToFrac $ y - x
    | otherwise = realToFrac $ x - y
  {-# INLINE diff #-}

-- | @since 0.1
instance (RealFloat a) => MetricSpace (Complex a) where
  -- NOTE: magnitude === abs except the latter adds an (+ 0i) imaginary part,
  -- to keep the type the same. Thus this agrees with the usual complex
  -- metric in terms of its norm.
  diff x y = realToFrac $ magnitude (y - x)
  {-# INLINE diff #-}
