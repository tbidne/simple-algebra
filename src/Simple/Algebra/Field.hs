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

-- | Defines a field. The second type parameter is some nonzero wrapper
-- over the ring @f@.
--
-- @since 0.1.0.0
class Ring f => Field f nz where
  -- | @since 0.1.0.0
  (.%.) :: f -> nz f -> f

infixl 7 .%.

-- | @since 0.1.0.0
instance Field Double NonZero where x .%. MkNonZero d = x / d

-- | @since 0.1.0.0
instance Field Float NonZero where x .%. MkNonZero d = x / d

-- | @since 0.1.0.0
instance Field Int NonZero where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Field Int8 NonZero where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Field Int16 NonZero where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Field Int32 NonZero where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Field Int64 NonZero where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Field Integer NonZero where x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance Integral k => Field (Ratio k) NonZero where
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
mkFieldNonZero :: Field a nz => a -> Maybe (nz a)
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
unsafeFieldNonZero :: Field a nz => a -> nz a
unsafeFieldNonZero x
  | x == zero = error "Passed identity to unsafeFieldNonZero!"
  | otherwise = unsafeCoerce x
