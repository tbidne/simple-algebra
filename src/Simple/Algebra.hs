-- | Re-exports modules used for defining algebraic structures.
-- The motivation is to provide a simple alternative to 'Num',
-- allowing us to provide finer-grain functionality.
--
-- Notice we do not provide Semigroup, Monoid, or indeed the many, many other
-- algebraic structures that exist. This package is not intended to be a
-- comprehensive implementation of algebraic structures. Thus clashing
-- with base\'s 'Semigroup' and 'Monoid' is undesirable, and for the former,
-- see perhaps the \"algebra\" package on hackage.
module Simple.Algebra
  ( module A,
  )
where

import Simple.Algebra.Field as A
import Simple.Algebra.Group as A
import Simple.Algebra.Module as A
import Simple.Algebra.Ring as A
import Simple.Algebra.VectorSpace as A
