-- | Provides the 'Module' typeclass.
--
-- @since 0.1.0.0
module Numeric.Algebra.Module
  ( Module (..),
  )
where

import Numeric.Algebra.Additive.AGroup (AGroup)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Algebra.Ring (Ring)

-- | Defines a module over a ring.
--
-- @since 0.1.0.0
class (AGroup m, Ring r) => Module m r | m -> r where
  -- | @since 0.1.0.0
  (.*) :: m -> MultConstraint r -> m
  (.*) = flip (*.)

  -- | @since 0.1.0.0
  (*.) :: MultConstraint r -> m -> m
  (*.) = flip (.*)

  {-# MINIMAL ((.*) | (*.)) #-}

infixl 7 .*

infixl 7 *.

-- | @since 0.1.0.0
instance Ring r => Module (r, r) r where
  (n1, n2) .* m = (n1 .*. m, n2 .*. m)

-- | @since 0.1.0.0
instance Ring r => Module (r, r, r) r where
  (n1, n2, n3) .* m = (n1 .*. m, n2 .*. m, n3 .*. m)

-- | @since 0.1.0.0
instance Ring r => Module (r, r, r, r) r where
  (n1, n2, n3, n4) .* m = (n1 .*. m, n2 .*. m, n3 .*. m, n4 .*. m)

-- | @since 0.1.0.0
instance Ring r => Module (r, r, r, r, r) r where
  (n1, n2, n3, n4, n5) .* m =
    ( n1 .*. m,
      n2 .*. m,
      n3 .*. m,
      n4 .*. m,
      n5 .*. m
    )

-- | @since 0.1.0.0
instance Ring r => Module (r, r, r, r, r, r) r where
  (n1, n2, n3, n4, n5, n6) .* m =
    ( n1 .*. m,
      n2 .*. m,
      n3 .*. m,
      n4 .*. m,
      n5 .*. m,
      n6 .*. m
    )

-- | @since 0.1.0.0
instance Ring r => Module (r, r, r, r, r, r, r) r where
  (n1, n2, n3, n4, n5, n6, n7) .* m =
    ( n1 .*. m,
      n2 .*. m,
      n3 .*. m,
      n4 .*. m,
      n5 .*. m,
      n6 .*. m,
      n7 .*. m
    )

-- | @since 0.1.0.0
instance Ring r => Module (r, r, r, r, r, r, r, r) r where
  (n1, n2, n3, n4, n5, n6, n7, n8) .* m =
    ( n1 .*. m,
      n2 .*. m,
      n3 .*. m,
      n4 .*. m,
      n5 .*. m,
      n6 .*. m,
      n7 .*. m,
      n8 .*. m
    )

-- | @since 0.1.0.0
instance Ring r => Module (r, r, r, r, r, r, r, r, r) r where
  (n1, n2, n3, n4, n5, n6, n7, n8, n9) .* m =
    ( n1 .*. m,
      n2 .*. m,
      n3 .*. m,
      n4 .*. m,
      n5 .*. m,
      n6 .*. m,
      n7 .*. m,
      n8 .*. m,
      n9 .*. m
    )
