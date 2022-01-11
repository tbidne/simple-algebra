-- | Provides the 'Field' typeclass.
--
-- @since 0.1.0.0
module Algebra.Field
  ( -- * Typeclass
    Field (..),

    -- * NonZero
    -- $nonzero
    mkFieldNonZero,
    unsafeFieldNonZero,
  )
where

import Algebra.AdditiveMonoid (AdditiveMonoid (..))
import Algebra.Ring (Ring)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Refined (NonZero, Refined)
import Refined.Extras (Implies, pattern MkRefined)
import Unsafe.Coerce (unsafeCoerce)

-- | Defines a field.
--
-- @since 0.1.0.0
class Ring f => Field f where
  -- | @since 0.1.0.0
  (.%.) :: Implies ps NonZero => f -> Refined ps f -> f

infixl 7 .%.

-- | @since 0.1.0.0
instance Field Double where x .%. MkRefined d = x / d

-- | @since 0.1.0.0
instance Field Float where x .%. MkRefined d = x / d

-- | @since 0.1.0.0
instance Field Int where x .%. MkRefined d = x `div` d

-- | @since 0.1.0.0
instance Field Int8 where x .%. MkRefined d = x `div` d

-- | @since 0.1.0.0
instance Field Int16 where x .%. MkRefined d = x `div` d

-- | @since 0.1.0.0
instance Field Int32 where x .%. MkRefined d = x `div` d

-- | @since 0.1.0.0
instance Field Int64 where x .%. MkRefined d = x `div` d

-- | @since 0.1.0.0
instance Field Integer where x .%. MkRefined d = x `div` d

-- | @since 0.1.0.0
instance Integral k => Field (Ratio k) where
  x .%. MkRefined d = x / d

-- $nonzero
-- The nonzero types here are based on @refined@'s 'NonZero'.
--
-- N.B. These functions check the /field/ 'zero', not the literal @0@. That
-- is, __the refinement is not checked__.
--
-- This gives us maximum flexibility:
--
--     * Non-numeric types can implement 'Field' (consider representing
--       \(\mathbb{Z}/2\mathbb{Z}\) as @data Z2 = One | Two@).
--     * Numeric types can utilize @refined@'s capabilities (e.g.
--       creating non-zero terms at compile-time via 'Refined.refineTH').
--
-- Not to mention, unifying 'Field'\'s notion of 'NonZero' with its
-- 'AdditiveMonoid' is absolutely necessary for '.%.' to actually be safe.
--
-- However, this also means that you can use this to create a 'Refined'
-- ['NonZero'] with a numeric type that /is/ zero, if you define
-- @zero /= 0@ for some reason.
--
-- To sum up:
--
--     * With non-numeric types there are no problems. There are no illusions
--       of getting extra value out of @refined@, so just use
--       'mkFieldNonZero' and 'unsafeFieldNonZero' and be on your merry way.
--
--     * With 'Num' types, there are no problems __as long as @'zero' == 0@__.
--       In this case, 'zero' and 'Refined' 'NonZero' are unified, so you
--       can happily take advantage of @refined@'s functionality.
--
--     * With 'Num' types that /do/ define @zero /= 0@, the algebraic
--       consistency is itself fine, but we can no longer assume that
--       @refined@'s 'NonZero' invariant is satisfied. This could
--       enable one to "prove" false theorems in @refined@, e.g.
--       @NonZero && NonNegative => Positive@.
--
-- In short, the only case where you might run into problems is if you
-- define 'Field' for some 'Num' @a@ with @zero /= 0@, and in that case
-- you just need to be careful about not using @refined@
-- in an unjustified way.

-- | Smart constructor for 'NonZero', based on its additive monoid instance.
--
-- >>> mkFieldNonZero @Integer 7
-- Just (UnsafeRefined {unrefine = 7})
--
-- >>> mkFieldNonZero @Integer 0
-- Nothing
--
-- @since 0.1.0.0
mkFieldNonZero :: Field a => a -> Maybe (Refined NonZero a)
mkFieldNonZero x
  | x == zero = Nothing
  | otherwise = Just (unsafeCoerce x)

-- | Unsafe constructor for 'NonZero', based on its additive monoid instance.
-- Intended to be used with known constants. Exercise restraint!
--
-- >>> unsafeFieldNonZero @Integer 7
-- UnsafeRefined {unrefine = 7}
--
-- >>> unsafeFieldNonZero @Integer 0
-- Passed identity to unsafeFieldNonZero!
--
-- @since 0.1.0.0
unsafeFieldNonZero :: Field a => a -> Refined NonZero a
unsafeFieldNonZero x
  | x == zero = error "Passed identity to unsafeFieldNonZero!"
  | otherwise = unsafeCoerce x
