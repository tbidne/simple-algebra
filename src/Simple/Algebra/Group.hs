-- | Provides the 'Group' typeclass.
module Simple.Algebra.Group
  ( Group (..),

    -- * NonZero
    NonZero (unNonZero, MkNonZero),
    mkNonZero,
    unsafeNonZero,
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

-- | Intended to be used for operations requiring non-zero numbers
-- (e.g. division).
newtype NonZero a = MkUnsafeNonZero
  { -- | Unwraps the 'NonZero'.
    unNonZero :: a
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'NonZero'.
pattern MkNonZero :: a -> NonZero a
pattern MkNonZero x <- MkUnsafeNonZero x

{-# COMPLETE MkNonZero #-}

-- | Smart constructor for 'NonZero'.
mkNonZero :: Group a => a -> Maybe (NonZero a)
mkNonZero x
  | x /= aid = Just $ MkUnsafeNonZero x
  | otherwise = Nothing

-- | Unsafe constructor for 'NonZero', intended to be used with known
-- constants, e.g., @unsafeNonZero 7@. Exercise restraint!
unsafeNonZero :: Group a => a -> NonZero a
unsafeNonZero x
  | x /= aid = MkUnsafeNonZero x
  | otherwise = error "Passed zero to unsafeNonZero!"
