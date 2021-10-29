-- | Re-exports modules used for defining algebraic structures.
-- The motivation is to provide a simple alternative to 'Num',
-- allowing us to provide finer-grain functionality.
--
-- Notice we do not provide classes for the many other algebraic structures
-- that exist. This package is not intended to be a comprehensive
-- implementation of abstract algebra. Rather. the intention is to provide
-- a simple type hierarchy that makes working with mathematical structures
-- more principled than 'Num'. If this is insufficient, perhaps the
-- \"algebra\" package on hackage will be more suitable.
module Simple.Algebra
  ( module A,
  )
where

import Simple.Algebra.AMonoid as A
import Simple.Algebra.ASemigroup as A
import Simple.Algebra.Field as A
import Simple.Algebra.Group as A
import Simple.Algebra.MMonoid as A
import Simple.Algebra.MSemigroup as A
import Simple.Algebra.Module as A
import Simple.Algebra.Ring as A
import Simple.Algebra.VectorSpace as A
