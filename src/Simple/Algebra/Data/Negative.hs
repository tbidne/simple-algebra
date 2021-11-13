-- | Provides the 'Negative' type for enforcing a negative invariant.
--
-- @since 0.1.0.0
module Simple.Algebra.Data.Negative
  ( -- * Type
    Negative (MkNegative, unNegative),

    -- * Creation
    mkNegative,
    readNegative,
    unsafeNegative,
  )
where

import Simple.Algebra.Additive (Additive (..))
import Simple.Algebra.Data.Utils qualified as U

-- | Newtype wrapper over /a/. The underlying /a/ is in \((-\infty, 0)\).
--
-- @since 0.1.0.0
newtype Negative a = UnsafeNegative
  { -- | Unwraps the 'Negative'
    --
    -- @since 0.1.0.0
    unNegative :: a
  }
  deriving (Eq, Ord, Show)

-- | @since 0.1.0.0
instance (Num a, Ord a) => Additive (Negative a) where
  MkNegative x .+. MkNegative y = unsafeNegative $ x + y

-- | Allows pattern matching on 'Negative'.
--
-- @since 0.1.0.0
pattern MkNegative :: a -> Negative a
pattern MkNegative n <- UnsafeNegative n

{-# COMPLETE MkNegative #-}

-- | Smart constructor for 'Negative'.
--
-- Examples:
--
-- >>> mkNegative (-2)
-- Just (UnsafeNegative {unNegative = -2})
--
-- >>> mkNegative 7
-- Nothing
--
-- @since 0.1.0.0
mkNegative :: (Num a, Ord a) => a -> Maybe (Negative a)
mkNegative = U.mkX isNegative UnsafeNegative

-- | Unsafe constructor for 'Negative', intended to be used with
-- known constants, e.g., @unsafeNegative 7@. Exercise restraint!
--
-- >>> unsafeNegative (-7)
-- UnsafeNegative {unNegative = -7}
--
-- >>> unsafeNegative 0
-- Passed non-negative to unsafeNegative!
--
-- @since 0.1.0.0
unsafeNegative :: (Num a, Ord a) => a -> Negative a
unsafeNegative = U.unsafeX msg isNegative UnsafeNegative
  where
    msg = "Passed non-negative to unsafeNegative!"

-- | Safely attempts to read a 'Negative'.
--
-- >>> readNegative "-5"
-- Just (UnsafeNegative {unNegative = -5})
--
-- >>> readNegative "cat"
-- Nothing
--
-- >>> readNegative "5"
-- Nothing
--
-- @since 0.1.0.0
readNegative :: (Num a, Ord a, Read a) => String -> Maybe (Negative a)
readNegative = U.readX isNegative UnsafeNegative

isNegative :: (Num a, Ord a) => a -> Bool
isNegative = (< 0)
