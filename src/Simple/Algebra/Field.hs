-- | Provides the 'Field' typeclass.
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
import Simple.Algebra.Ring (Ring)
import Simple.NonNat (NonZero, pattern MkNonZero)
import Unsafe.Coerce (unsafeCoerce)

-- | Defines a field.
class Ring f => Field f where
  (.%.) :: f -> NonZero f -> f

infixl 7 .%.

instance Field Double where x .%. MkNonZero d = x / d

instance Field Float where x .%. MkNonZero d = x / d

instance Field Int where x .%. MkNonZero d = x `div` d

instance Field Int8 where x .%. MkNonZero d = x `div` d

instance Field Int16 where x .%. MkNonZero d = x `div` d

instance Field Int32 where x .%. MkNonZero d = x `div` d

instance Field Int64 where x .%. MkNonZero d = x `div` d

instance Field Integer where x .%. MkNonZero d = x `div` d

instance Integral k => Field (Ratio k) where
  x .%. MkNonZero d = x / d

-- | Smart constructor for 'NonZero', based on its additive monoid instance.
mkFieldNonZero :: Field a => a -> Maybe (NonZero a)
mkFieldNonZero x
  | x == zero = Nothing
  | otherwise = Just (unsafeCoerce x)

-- | Unsafe constructor for 'NonZero', based on its additive monoid instance.
-- Intended to be used with known constants. Exercise restraint!
unsafeFieldNonZero :: Field a => a -> NonZero a
unsafeFieldNonZero x
  | x == zero = error "Passed identity to unsafeFieldNonZero!"
  | otherwise = unsafeCoerce x
