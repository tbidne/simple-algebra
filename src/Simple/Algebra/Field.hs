-- | Provides the 'Field' typeclass.
--
-- @since 0.1.0.0
module Simple.Algebra.Field
  ( -- * Typeclass
    Field (..),

    -- * NonZero
    mkFieldNonZero,
    unsafeFieldNonZero,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Simple.Algebra.AdditiveMonoid (AdditiveMonoid (..))
import Simple.Algebra.Data.NonNat (NonZero, pattern MkNonZero)
import Simple.Algebra.Ring (Ring)
import Unsafe.Coerce (unsafeCoerce)

-- | Defines a field.
--
-- @since 0.1.0.0
class Ring f => Field f where
  -- | @since 0.1.0.0
  (.%.) :: f -> NonZero f -> f

infixl 7 .%.

-- | @since 0.1.0.0
instance Field Double where x .%. MkNonZero d = x / d

-- | @since 0.1.0.0
instance Field Float where x .%. MkNonZero d = x / d

-- | @since 0.1.0.0
instance Field Int where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Field Int8 where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Field Int16 where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Field Int32 where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Field Int64 where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Field Integer where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Integral k => Field (Ratio k) where
  x .%. MkNonZero d = x / d

-- | Smart constructor for 'NonZero', based on its additive monoid instance.
--
-- >>> mkFieldNonZero @Integer 7
-- Just (MkUnsafeNonNat {unNonNat = 7})
--
-- >>> mkFieldNonZero @Integer 0
-- Nothing
--
-- @since 0.1.0.0
mkFieldNonZero :: Field a => a -> Maybe (NonZero a)
mkFieldNonZero x
  | x == zero = Nothing
  | otherwise = Just (unsafeCoerce x)

-- | Unsafe constructor for 'NonZero', based on its additive monoid instance.
-- Intended to be used with known constants. Exercise restraint!
--
-- >>> unsafeFieldNonZero @Integer 7
-- MkUnsafeNonNat {unNonNat = 7}
--
-- >>> unsafeFieldNonZero @Integer 0
-- Passed identity to unsafeFieldNonZero!
--
-- @since 0.1.0.0
unsafeFieldNonZero :: Field a => a -> NonZero a
unsafeFieldNonZero x
  | x == zero = error "Passed identity to unsafeFieldNonZero!"
  | otherwise = unsafeCoerce x
