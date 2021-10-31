-- | Provides the 'Group' typeclass.
module Simple.Algebra.Group
  ( Group (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Simple.Algebra.Additive (Additive (..))
import Simple.Algebra.AdditiveMonoid (AdditiveMonoid (..))

-- | Defines a group.
class AdditiveMonoid g => Group g where
  (.-.) :: g -> g -> g
  g .-. h = g .+. ginv h

  ginv :: g -> g
  ginv g = zero .-. g

  gabs :: g -> g

  {-# MINIMAL ((.-.) | ginv), gabs #-}

infixl 6 .-.

instance Group Double where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Float where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Int where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Int8 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Int16 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Int32 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Int64 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Integer where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance (Integral a) => Group (Ratio a) where
  (.-.) = (-)
  ginv = negate
  gabs = abs
