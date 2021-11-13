-- | Provides the 'Module' typeclass.
--
-- @since 0.1.0.0
module Simple.Algebra.Module
  ( Module (..),
  )
where

import Simple.Algebra.Group (Group)
import Simple.Algebra.Ring (Ring)

-- | Defines a module over a ring.
--
-- @since 0.1.0.0
class (Group m, Ring r) => Module m r | m -> r where
  -- | @since 0.1.0.0
  (.*) :: m -> r -> m
  (.*) = flip (*.)

  -- | @since 0.1.0.0
  (*.) :: r -> m -> m
  (*.) = flip (.*)

  {-# MINIMAL ((.*) | (*.)) #-}

infixl 7 .*

infixl 7 *.
