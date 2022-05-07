-- | Provides the 'Semimodule' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.Semimodule
  ( Semimodule,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Additive.AMonoid (AMonoid)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Algebra.Space.MSemiSpace (MSemiSpace)

-- | Defines a semimodule over a semiring. This generalizes the notion of
-- a 'Numeric.Algebra.Module.Module' \(M\) over a 'Numeric.Algebra.Ring.Ring'
-- \(R\) such that:
--
-- * \(M\) is an 'AMonoid', not an 'Numeric.Algebra.Additive.AGroup.AGroup'.
-- * \(R\) is a 'Semiring', not a 'Numeric.Algebra.Ring.Ring'.
--
-- @since 0.1
type Semimodule :: Type -> Type -> Constraint
class (AMonoid m, MSemiSpace m r, Semiring r) => Semimodule m r | m -> r

-- | @since 0.1
instance Semiring r => Semimodule (r, r) r

-- | @since 0.1
instance Semiring r => Semimodule (r, r, r) r

-- | @since 0.1
instance Semiring r => Semimodule (r, r, r, r) r

-- | @since 0.1
instance Semiring r => Semimodule (r, r, r, r, r) r

-- | @since 0.1
instance Semiring r => Semimodule (r, r, r, r, r, r) r

-- | @since 0.1
instance Semiring r => Semimodule (r, r, r, r, r, r, r) r

-- | @since 0.1
instance Semiring r => Semimodule (r, r, r, r, r, r, r, r) r

-- | @since 0.1
instance Semiring r => Semimodule (r, r, r, r, r, r, r, r, r) r
