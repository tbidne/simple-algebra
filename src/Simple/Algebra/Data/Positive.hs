-- | Provides the 'Positive' type for enforcing a positive invariant.
--
-- @since 0.1.0.0
module Simple.Algebra.Data.Positive
  ( -- * Type
    Positive (MkPositive, unPositive),

    -- * Creation
    mkPositive,
    readPositive,
    unsafePositive,
  )
where

import Simple.Algebra.Additive (Additive (..))
import Simple.Algebra.Data.Utils qualified as U
import Simple.Algebra.Multiplicative (Multiplicative (..))
import Simple.Algebra.MultiplicativeMonoid (MultiplicativeMonoid (..))

-- | Newtype wrapper over /a/. The underlying /a/ is in \((0, \infty)\).
--
-- @since 0.1.0.0
newtype Positive a = UnsafePositive
  { -- | Unwraps the 'Positive'
    --
    -- @since 0.1.0.0
    unPositive :: a
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'Positive'.
--
-- @since 0.1.0.0
pattern MkPositive :: a -> Positive a
pattern MkPositive n <- UnsafePositive n

{-# COMPLETE MkPositive #-}

-- | @since 0.1.0.0
instance (Num a, Ord a) => Additive (Positive a) where
  MkPositive x .+. MkPositive y = unsafePositive $ x + y

-- | @since 0.1.0.0
instance (Num a, Ord a) => Multiplicative (Positive a) where
  MkPositive x .*. MkPositive y = unsafePositive $ x * y

-- | @since 0.1.0.0
instance (Num a, Ord a) => MultiplicativeMonoid (Positive a) where
  one = unsafePositive 1

-- | Smart constructor for 'Positive'.
--
-- Examples:
--
-- >>> mkPositive 7
-- Just (UnsafePositive {unPositive = 7})
--
-- >>> mkPositive 0
-- Nothing
--
-- @since 0.1.0.0
mkPositive :: (Num a, Ord a) => a -> Maybe (Positive a)
mkPositive = U.mkX isPositive UnsafePositive

-- | Unsafe constructor for 'Positive', intended to be used with
-- known constants, e.g., @unsafePositive 7@. Exercise restraint!
--
-- >>> unsafePositive 7
-- UnsafePositive {unPositive = 7}
--
-- >>> unsafePositive 0
-- Passed non-positive to unsafePositive!
--
-- @since 0.1.0.0
unsafePositive :: (Num a, Ord a) => a -> Positive a
unsafePositive = U.unsafeX msg isPositive UnsafePositive
  where
    msg = "Passed non-positive to unsafePositive!"

-- | Safely attempts to read a 'Positive'.
--
-- >>> readPositive "5"
-- Just (UnsafePositive {unPositive = 5})
--
-- >>> readPositive "cat"
-- Nothing
--
-- >>> readPositive "0"
-- Nothing
--
-- @since 0.1.0.0
readPositive :: (Num a, Ord a, Read a) => String -> Maybe (Positive a)
readPositive = U.readX isPositive UnsafePositive

isPositive :: (Num a, Ord a) => a -> Bool
isPositive = (> 0)
