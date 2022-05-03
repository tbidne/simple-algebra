-- | Provides the 'VectorSpace' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.VectorSpace
  ( VectorSpace,
  )
where

import Numeric.Algebra.Field (Field)
import Numeric.Algebra.Space.Module (Module)
import Numeric.Algebra.Space.SemivectorSpace (SemivectorSpace)

-- | Defines a vector space over a field.
--
-- @since 0.1
class (Field k, Module v k, SemivectorSpace v k) => VectorSpace v k | v -> k

-- | @since 0.1
instance Field k => VectorSpace (k, k) k

-- | @since 0.1
instance Field k => VectorSpace (k, k, k) k

-- | @since 0.1
instance Field k => VectorSpace (k, k, k, k) k

-- | @since 0.1
instance Field k => VectorSpace (k, k, k, k, k) k

-- | @since 0.1
instance Field k => VectorSpace (k, k, k, k, k, k) k

-- | @since 0.1
instance Field k => VectorSpace (k, k, k, k, k, k, k) k

-- | @since 0.1
instance Field k => VectorSpace (k, k, k, k, k, k, k, k) k

-- | @since 0.1
instance Field k => VectorSpace (k, k, k, k, k, k, k, k, k) k
