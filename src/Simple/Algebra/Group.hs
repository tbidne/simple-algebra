-- | Provides the 'Group' typeclass.
--
-- @since 0.1.0.0
module Simple.Algebra.Group
  ( Group (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Simple.Algebra.Additive (Additive (..))
import Simple.Algebra.AdditiveMonoid (AdditiveMonoid (..))

-- | Defines a group.
--
-- @since 0.1.0.0
class AdditiveMonoid g => Group g where
  -- | @since 0.1.0.0
  (.-.) :: g -> g -> g
  g .-. h = g .+. ginv h

  -- | @since 0.1.0.0
  ginv :: g -> g
  ginv g = zero .-. g

  -- | @since 0.1.0.0
  gabs :: g -> g

  {-# MINIMAL ((.-.) | ginv), gabs #-}

infixl 6 .-.

-- | @since 0.1.0.0
instance Group Double where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Float where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Int where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Int8 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Int16 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Int32 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Int64 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance Group Integer where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

-- | @since 0.1.0.0
instance (Integral a) => Group (Ratio a) where
  (.-.) = (-)
  ginv = negate
  gabs = abs
