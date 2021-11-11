-- | Provides the 'NonPositive' type for enforcing a non-positive invariant.
module Simple.Algebra.Data.NonPositive
  ( -- * Type
    NonPositive (MkNonPositive, unNonPositive),

    -- * Creation
    mkNonPositive,
    readNonPositive,
    unsafeNonPositive,
  )
where

import Simple.Algebra (Additive (..), AdditiveMonoid (..))
import Simple.Algebra.Data.Utils qualified as U

-- | Newtype wrapper over /a/. The underlying /a/ is in \(-(\infty, 0]\).
newtype NonPositive a = UnsafeNonPositive
  { -- | Unwraps the 'NonPositive'
    unNonPositive :: a
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'NonPositive'.
pattern MkNonPositive :: a -> NonPositive a
pattern MkNonPositive n <- UnsafeNonPositive n

{-# COMPLETE MkNonPositive #-}

instance (Num a, Ord a) => Additive (NonPositive a) where
  MkNonPositive x .+. MkNonPositive y = unsafeNonPositive $ x + y

instance (Num a, Ord a) => AdditiveMonoid (NonPositive a) where
  zero = unsafeNonPositive 0

-- | Smart constructor for 'NonPositive'.
--
-- Examples:
--
-- >>> mkNonPositive (-2)
-- Just (UnsafeNonPositive {unNonPositive = -2})
--
-- >>> mkNonPositive 7
-- Nothing
mkNonPositive :: (Num a, Ord a) => a -> Maybe (NonPositive a)
mkNonPositive = U.mkX isNonPositive UnsafeNonPositive

-- | Unsafe constructor for 'NonPositive', intended to be used with
-- known constants, e.g., @unsafeNonPositive (-2)@. Exercise restraint!
unsafeNonPositive :: (Num a, Ord a) => a -> NonPositive a
unsafeNonPositive = U.unsafeX msg isNonPositive UnsafeNonPositive
  where
    msg = "Passed negative to unsafeNonPositive!"

-- | Safely attempts to read a 'NonPositive'.
--
-- >>> readNonPositive "-5"
-- Just (UnsafeNonPositive {unNonPositive = -5})
--
-- >>> readNonPositive "cat"
-- Nothing
--
-- >>> readNonPositive "5"
-- Nothing
readNonPositive :: (Num a, Ord a, Read a) => String -> Maybe (NonPositive a)
readNonPositive = U.readX isNonPositive UnsafeNonPositive

isNonPositive :: (Num a, Ord a) => a -> Bool
isNonPositive = (<= 0)
