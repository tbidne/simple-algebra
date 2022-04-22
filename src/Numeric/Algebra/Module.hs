-- | Provides the 'Module' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Module
  ( Module,
  )
where

import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Semimodule (Semimodule (..))

-- | Defines a module over a ring.
--
-- @since 0.1
class (Semimodule m r, Ring r) => Module m r | m -> r

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
