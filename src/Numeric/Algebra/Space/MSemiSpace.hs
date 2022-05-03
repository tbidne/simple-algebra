-- | Provides the 'MSemiSpace' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.MSemiSpace
  ( MSemiSpace (..),
  )
where

import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))

-- | Defines a "multiplicative semi space" over an 'MSemigroup'. This
-- generalizes the notion of a 'Numeric.Algebra.Space.Semimodule.Semimodule'
-- \(M\) over a 'Numeric.Algebra.Ring.Ring' \(R\) in that we assume no
-- additive structure on the space itself.
--
-- @since 0.1
class MSemigroup r => MSemiSpace m r | m -> r where
  -- | @since 0.1
  (.*) :: m -> r -> m
  (.*) = flip (*.)

  -- | @since 0.1
  (*.) :: r -> m -> m
  (*.) = flip (.*)

  {-# MINIMAL ((.*) | (*.)) #-}

infixl 7 .*

infixl 7 *.

-- | @since 0.1
instance MSemigroup r => MSemiSpace (r, r) r where
  (n1, n2) .* m = (n1 .*. m, n2 .*. m)

-- | @since 0.1
instance MSemigroup r => MSemiSpace (r, r, r) r where
  (n1, n2, n3) .* m = (n1 .*. m, n2 .*. m, n3 .*. m)

-- | @since 0.1
instance MSemigroup r => MSemiSpace (r, r, r, r) r where
  (n1, n2, n3, n4) .* m = (n1 .*. m, n2 .*. m, n3 .*. m, n4 .*. m)

-- | @since 0.1
instance MSemigroup r => MSemiSpace (r, r, r, r, r) r where
  (n1, n2, n3, n4, n5) .* m =
    ( n1 .*. m,
      n2 .*. m,
      n3 .*. m,
      n4 .*. m,
      n5 .*. m
    )

-- | @since 0.1
instance MSemigroup r => MSemiSpace (r, r, r, r, r, r) r where
  (n1, n2, n3, n4, n5, n6) .* m =
    ( n1 .*. m,
      n2 .*. m,
      n3 .*. m,
      n4 .*. m,
      n5 .*. m,
      n6 .*. m
    )

-- | @since 0.1
instance MSemigroup r => MSemiSpace (r, r, r, r, r, r, r) r where
  (n1, n2, n3, n4, n5, n6, n7) .* m =
    ( n1 .*. m,
      n2 .*. m,
      n3 .*. m,
      n4 .*. m,
      n5 .*. m,
      n6 .*. m,
      n7 .*. m
    )

-- | @since 0.1
instance MSemigroup r => MSemiSpace (r, r, r, r, r, r, r, r) r where
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

-- | @since 0.1
instance MSemigroup r => MSemiSpace (r, r, r, r, r, r, r, r, r) r where
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
