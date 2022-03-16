{-# LANGUAGE CPP #-}

-- | Provides the 'NonNegative' type for enforcing a nonnegative invariant.
--
-- @since 0.1.0.0
module Numeric.Data.NonNegative
  ( -- * Type
    NonNegative (MkNonNegative),

    -- * Creation
    mkNonNegativeTH,
    mkNonNegative,
    unsafeNonNegative,
    reallyUnsafeNonNegative,

    -- * Elimination
    unNonNegative,
  )
where

import Data.Kind (Type)
import GHC.Stack (HasCallStack)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))

-- $setup
-- >>> :set -XTemplateHaskell

-- | Newtype wrapper that attaches a 'NonNegative' invariant to some @a@.
-- 'NonNegative' is a:
--
-- * 'Numeric.Algebra.Additive.ASemigroup.ASemigroup'
-- * 'Numeric.Algebra.Additive.AMonoid.AMonoid'
-- * 'Numeric.Algebra.Multiplicative.MSemigroup.MSemigroup'
-- * 'Numeric.Algebra.Multiplicative.MMonoid.MMonoid'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroup'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroupIntegral'
-- * 'Numeric.Algebra.Semiring.Semiring'
--
-- @since 0.1.0.0
type NonNegative :: Type -> Type
newtype NonNegative a = UnsafeNonNegative a
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )

-- | Unidirectional pattern synonym for 'NonNegative'. This allows us to pattern
-- match on a nonnegative term without exposing the unsafe internal details.
--
-- @since 0.1.0.0
pattern MkNonNegative :: a -> NonNegative a
pattern MkNonNegative x <- UnsafeNonNegative x

{-# COMPLETE MkNonNegative #-}

-- | Unwraps a 'NonNegative'.
--
-- @since 0.1.0.0
unNonNegative :: NonNegative a -> a
unNonNegative (UnsafeNonNegative x) = x

-- | Template haskell for creating a 'NonNegative' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkNonNegativeTH 1)
-- UnsafeNonNegative 1
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkNonNegativeTH :: (Integral a, Lift a, Show a) => a -> a -> Code Q (NonNegative a)
#else
mkNonNegativeTH :: (Integral a, Lift a, Show a) => a -> Q (TExp (NonNegative a))
#endif
mkNonNegativeTH x = maybe (error err) liftTyped $ mkNonNegative x
  where
    err =
      "Numeric.Data.NonNegative.mkNonNegativeTH: Passed value < 0: "
        <> show x

-- | Smart constructor for 'NonNegative'. Returns 'Nothing' if the second
-- parameter is @< 0@.
--
-- ==== __Examples__
-- >>> mkNonNegative 0
-- Just (UnsafeNonNegative 0)
--
-- >>> mkNonNegative (-2)
-- Nothing
--
-- @since 0.1.0.0
mkNonNegative :: (Num a, Ord a) => a -> Maybe (NonNegative a)
mkNonNegative x
  | x >= 0 = Just (UnsafeNonNegative x)
  | otherwise = Nothing

-- | Variant of 'mkNonNegative' that throws an error when given a value < 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeNonNegative 7
-- UnsafeNonNegative 7
--
-- @since 0.1.0.0
unsafeNonNegative :: (HasCallStack, Num a, Ord a, Show a) => a -> NonNegative a
unsafeNonNegative x
  | x >= 0 = UnsafeNonNegative x
  | otherwise =
      error $
        "Numeric.Data.NonNegative.unsafeNonNegative: Passed value < 0: " <> show x

-- | This function is an alias for the unchecked constructor @UnsafeNonNegative@
-- i.e. it allows us to construct a 'NonNegative' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeNonNegative') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1.0.0
reallyUnsafeNonNegative :: a -> NonNegative a
reallyUnsafeNonNegative = UnsafeNonNegative
