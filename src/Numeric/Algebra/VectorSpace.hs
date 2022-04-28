-- | Provides the 'VectorSpace' typeclass.
--
-- @since 0.1
module Numeric.Algebra.VectorSpace
  ( VectorSpace,
  )
where

import Numeric.Algebra.Field (Field)
import Numeric.Algebra.Module (Module)
import Numeric.Algebra.SemivectorSpace (SemivectorSpace)

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
