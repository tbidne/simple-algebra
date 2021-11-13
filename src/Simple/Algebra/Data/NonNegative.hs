-- | Provides the 'NonNegative' type for enforcing a non-negative invariant.
--
-- @since 0.1.0.0
module Simple.Algebra.Data.NonNegative
  ( -- * Type
    NonNegative (MkNonNegative, unNonNegative),

    -- * Creation
    mkNonNegative,
    readNonNegative,
    unsafeNonNegative,
  )
where

import Simple.Algebra.Additive (Additive (..))
import Simple.Algebra.AdditiveMonoid (AdditiveMonoid (..))
import Simple.Algebra.Data.Utils qualified as U
import Simple.Algebra.Multiplicative (Multiplicative (..))
import Simple.Algebra.MultiplicativeMonoid (MultiplicativeMonoid (..))
import Simple.Algebra.Semiring (Semiring)

-- | Newtype wrapper over /a/. The underlying /a/ is in \([0, \infty)\).
--
-- @since 0.1.0.0
newtype NonNegative a = UnsafeNonNegative
  { -- | Unwraps the 'NonNegative'
    --
    -- @since 0.1.0.0
    unNonNegative :: a
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'NonNegative'.
--
-- @since 0.1.0.0
pattern MkNonNegative :: a -> NonNegative a
pattern MkNonNegative n <- UnsafeNonNegative n

{-# COMPLETE MkNonNegative #-}

-- | @since 0.1.0.0
instance (Num a, Ord a) => Additive (NonNegative a) where
  MkNonNegative x .+. MkNonNegative y = unsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance (Num a, Ord a) => AdditiveMonoid (NonNegative a) where
  zero = unsafeNonNegative 0

-- | @since 0.1.0.0
instance (Num a, Ord a) => Multiplicative (NonNegative a) where
  MkNonNegative x .*. MkNonNegative y = unsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance (Num a, Ord a) => MultiplicativeMonoid (NonNegative a) where
  one = unsafeNonNegative 1

-- | @since 0.1.0.0
instance (Num a, Ord a) => Semiring (NonNegative a)

-- | Smart constructor for 'NonNegative'.
--
-- Examples:
--
-- >>> mkNonNegative 0
-- Just (UnsafeNonNegative {unNonNegative = 0})
--
-- >>> mkNonNegative (-2)
-- Nothing
--
-- @since 0.1.0.0
mkNonNegative :: (Num a, Ord a) => a -> Maybe (NonNegative a)
mkNonNegative = U.mkX isNonNegative UnsafeNonNegative

-- | Unsafe constructor for 'NonNegative', intended to be used with
-- known constants, e.g., @unsafeNonNegative 7@. Exercise restraint!
--
-- >>> unsafeNonNegative 0
-- UnsafeNonNegative {unNonNegative = 0}
--
-- >>> unsafeNonNegative (-7)
-- Passed negative to unsafeNonNegative!
--
-- @since 0.1.0.0
unsafeNonNegative :: (Num a, Ord a) => a -> NonNegative a
unsafeNonNegative = U.unsafeX msg isNonNegative UnsafeNonNegative
  where
    msg = "Passed negative to unsafeNonNegative!"

-- | Safely attempts to read a 'NonNegative'.
--
-- >>> readNonNegative "0"
-- Just (UnsafeNonNegative {unNonNegative = 0})
--
-- >>> readNonNegative "cat"
-- Nothing
--
-- >>> readNonNegative "-5"
-- Nothing
--
-- @since 0.1.0.0
readNonNegative :: (Num a, Ord a, Read a) => String -> Maybe (NonNegative a)
readNonNegative = U.readX isNonNegative UnsafeNonNegative

isNonNegative :: (Num a, Ord a) => a -> Bool
isNonNegative = (>= 0)
