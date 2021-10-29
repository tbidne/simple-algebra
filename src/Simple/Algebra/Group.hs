-- | Provides the 'Group' typeclass.
module Simple.Algebra.Group
  ( Group (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Simple.Algebra.AMonoid (AMonoid (..))
import Simple.Algebra.ASemigroup (ASemigroup (..))

-- | Defines an algebraic group.
class AMonoid g => Group g where
  (.-.) :: g -> g -> g
  g .-. h = g .+. ginv h

  ginv :: g -> g
  ginv g = aid .-. g

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

instance Group Natural where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Word where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Word8 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Word16 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Word32 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Group Word64 where
  (.-.) = (-)
  ginv x = - x
  gabs = abs

instance Integral a => Group (Ratio a) where
  (.-.) = (-)
  ginv x = - x
  gabs = abs
