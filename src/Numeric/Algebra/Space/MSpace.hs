-- | Provides the 'MSpace' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.MSpace
  ( MSpace (..),
    (%.),
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..))
import Numeric.Algebra.Space.MSemiSpace (MSemiSpace)

-- | Defines a "multiplicative space" over an 'MGroup'. This generalizes
-- the notion of a 'Numeric.Algebra.Space.SemivectorSpace.Semivectorspace'
-- \(V\) over a 'Numeric.Algebra.Semifield.Semifield' \(K\) in that we
-- assume no additive structure on the space itself.
--
-- @since 0.1
type MSpace :: Type -> Type -> Constraint
class (MGroup k, MSemiSpace v k) => MSpace v k | v -> k where
  -- | @since 0.1
  (.%) :: v -> k -> v

infixl 7 .%

-- | @since 0.1
(%.) :: (MSpace v k) => k -> v -> v
(%.) = flip (.%)
{-# INLINE (%.) #-}

infixl 7 %.

-- | @since 0.1
instance (MGroup k) => MSpace (k, k) k where
  (x1, x2) .% k = (x1 .%. k, x2 .%. k)
  {-# INLINE (.%) #-}

-- | @since 0.1
instance (MGroup k) => MSpace (k, k, k) k where
  (x1, x2, x3) .% k = (x1 .%. k, x2 .%. k, x3 .%. k)
  {-# INLINE (.%) #-}

-- | @since 0.1
instance (MGroup k) => MSpace (k, k, k, k) k where
  (x1, x2, x3, x4) .% k = (x1 .%. k, x2 .%. k, x3 .%. k, x4 .%. k)
  {-# INLINE (.%) #-}

-- | @since 0.1
instance (MGroup k) => MSpace (k, k, k, k, k) k where
  (x1, x2, x3, x4, x5) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k
    )
  {-# INLINE (.%) #-}

-- | @since 0.1
instance (MGroup k) => MSpace (k, k, k, k, k, k) k where
  (x1, x2, x3, x4, x5, x6) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k,
      x6 .%. k
    )
  {-# INLINE (.%) #-}

-- | @since 0.1
instance (MGroup k) => MSpace (k, k, k, k, k, k, k) k where
  (x1, x2, x3, x4, x5, x6, x7) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k,
      x6 .%. k,
      x7 .%. k
    )
  {-# INLINE (.%) #-}

-- | @since 0.1
instance (MGroup k) => MSpace (k, k, k, k, k, k, k, k) k where
  (x1, x2, x3, x4, x5, x6, x7, x8) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k,
      x6 .%. k,
      x7 .%. k,
      x8 .%. k
    )
  {-# INLINE (.%) #-}

-- | @since 0.1
instance (MGroup k) => MSpace (k, k, k, k, k, k, k, k, k) k where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k,
      x6 .%. k,
      x7 .%. k,
      x8 .%. k,
      x9 .%. k
    )
  {-# INLINE (.%) #-}
