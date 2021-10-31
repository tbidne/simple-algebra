-- | Provides the 'Module' typeclass.
module Simple.Algebra.Module
  ( Module (..),
  )
where

import Simple.Algebra.Group (Group)
import Simple.Algebra.Ring (Ring)

-- | Defines a module over a ring.
class (Group m, Ring r) => Module m r | m -> r where
  (.*) :: m -> r -> m
  (.*) = flip (*.)

  (*.) :: r -> m -> m
  (*.) = flip (.*)

  {-# MINIMAL ((.*) | (*.)) #-}

infixl 7 .*

infixl 7 *.
