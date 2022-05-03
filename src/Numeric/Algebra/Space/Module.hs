-- | Provides the 'Module' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.Module
  ( Module,
  )
where

import Numeric.Algebra.Additive.AGroup (AGroup)
import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Space.Semimodule (Semimodule)

-- | Defines a module over a ring. This generalizes the notion of
-- a 'Numeric.Algebra.VectorSpace.VectorSpace' \(M\) over a
-- 'Numeric.Algebra.Field.Field' \(R\) such that:
--
-- * \(R\) is a 'Ring', not a 'Numeric.Algebra.Field.Field'.
--
-- @since 0.1
class (AGroup m, Semimodule m r, Ring r) => Module m r | m -> r

-- | @since 0.1
instance Ring r => Module (r, r) r

-- | @since 0.1
instance Ring r => Module (r, r, r) r

-- | @since 0.1
instance Ring r => Module (r, r, r, r) r

-- | @since 0.1
instance Ring r => Module (r, r, r, r, r) r

-- | @since 0.1
instance Ring r => Module (r, r, r, r, r, r) r

-- | @since 0.1
instance Ring r => Module (r, r, r, r, r, r, r) r

-- | @since 0.1
instance Ring r => Module (r, r, r, r, r, r, r, r) r

-- | @since 0.1
instance Ring r => Module (r, r, r, r, r, r, r, r, r) r
