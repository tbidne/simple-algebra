-- | Provides the 'SemivectorSpace' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.SemivectorSpace
  ( SemivectorSpace,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Semifield (Semifield)
import Numeric.Algebra.Space.MSpace (MSpace)
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
type SemivectorSpace :: Type -> Type -> Constraint
class (MSpace v k, Semifield k, Semimodule v k) => SemivectorSpace v k | v -> k

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k) k

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k) k

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k) k

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k, k) k

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k, k, k) k

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k, k, k, k) k

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k, k, k, k, k) k

-- | @since 0.1
instance Semifield k => SemivectorSpace (k, k, k, k, k, k, k, k, k) k
