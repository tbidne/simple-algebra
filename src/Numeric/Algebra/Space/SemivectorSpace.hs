-- | Provides the 'SemivectorSpace' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.SemivectorSpace
  ( SemivectorSpace (..),
  )
where

import Numeric.Algebra.Multiplicative.MGroup (MGroup (..), NonZero)
import Numeric.Algebra.Semifield (Semifield)
import Numeric.Algebra.Space.Semimodule (Semimodule)

-- | Defines a semivector space over a semifield. This generalizes the notion
-- of a 'Numeric.Algebra.VectorSpace.VectorSpace' \(V\) over a
-- 'Numeric.Algebra.Field.Field' \(K\) such that:
--
-- * \(V\) is an 'Numeric.Algebra.Additive.AMonoid.AMonoid', not an
--   'Numeric.Algebra.Additive.AGroup.AGroup'.
-- * \(K\) is a 'Semifield', not a 'Numeric.Algebra.Field.Field'.
--
-- @since 0.1
class (Semifield k, Semimodule v k) => SemivectorSpace v k | v -> k where
  -- | @since 0.1
  (.%) :: v -> NonZero k -> v

infixl 7 .%

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k) k where
  (x1, x2) .% k = (x1 .%. k, x2 .%. k)

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k) k where
  (x1, x2, x3) .% k = (x1 .%. k, x2 .%. k, x3 .%. k)

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k) k where
  (x1, x2, x3, x4) .% k = (x1 .%. k, x2 .%. k, x3 .%. k, x4 .%. k)

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k, k) k where
  (x1, x2, x3, x4, x5) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k
    )

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k, k, k) k where
  (x1, x2, x3, x4, x5, x6) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k,
      x6 .%. k
    )

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k, k, k, k) k where
  (x1, x2, x3, x4, x5, x6, x7) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k,
      x6 .%. k,
      x7 .%. k
    )

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k, k, k, k, k) k where
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

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k, k, k, k, k, k) k where
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
