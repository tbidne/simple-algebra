{-# LANGUAGE CPP #-}

-- | Provides the 'NonPositive' type for enforcing a nonpositive invariant.
--
-- @since 0.1.0.0
module Numeric.Data.NonPositive
  ( -- * Type
    NonPositive (MkNonPositive),

    -- * Creation
    mkNonPositiveTH,
    mkNonPositive,
    unsafeNonPositive,
    reallyUnsafeNonPositive,

    -- * Elimination
    unNonPositive,
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

-- | Newtype wrapper that attaches a 'NonPositive' invariant to some @a@.
--
-- @since 0.1.0.0
type NonPositive :: Type -> Type
newtype NonPositive a = UnsafeNonPositive a
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

pattern MkNonPositive :: a -> NonPositive a
pattern MkNonPositive x <- UnsafeNonPositive x

{-# COMPLETE MkNonPositive #-}

-- | Unwraps a 'NonPositive'.
--
-- @since 0.1.0.0
unNonPositive :: NonPositive a -> a
unNonPositive (UnsafeNonPositive x) = x

-- | Template haskell for creating a 'NonPositive' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkNonPositiveTH 0)
-- UnsafeNonPositive 0
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkNonPositiveTH :: (Integral a, Lift a, Show a) => a -> a -> Code Q (NonPositive a)
#else
mkNonPositiveTH :: (Integral a, Lift a, Show a) => a -> Q (TExp (NonPositive a))
#endif
mkNonPositiveTH x = maybe (error err) liftTyped $ mkNonPositive x
  where
    err =
      "Numeric.Data.NonPositive.mkNonPositiveTH: Passed value > 0: "
        <> show x

-- | Smart constructor for 'NonPositive'. Returns 'Nothing' if the second
-- parameter is @> 0@.
--
-- ==== __Examples__
-- >>> mkNonPositive (-2)
-- Just (UnsafeNonPositive (-2))
--
-- >>> mkNonPositive 1
-- Nothing
--
-- @since 0.1.0.0
mkNonPositive :: (Num a, Ord a) => a -> Maybe (NonPositive a)
mkNonPositive x
  | x <= 0 = Just (UnsafeNonPositive x)
  | otherwise = Nothing

-- | Variant of 'mkNonPositive' that throws an error when given a value > 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeNonPositive 0
-- UnsafeNonPositive 0
--
-- @since 0.1.0.0
unsafeNonPositive :: (HasCallStack, Num a, Ord a, Show a) => a -> NonPositive a
unsafeNonPositive x
  | x <= 0 = UnsafeNonPositive x
  | otherwise =
      error $
        "Numeric.Data.NonPositive.unsafeNonPositive: Passed value > 0: " <> show x

-- | This function is an alias for the unchecked constructor @UnsafeNonPositive@
-- i.e. it allows us to construct a 'NonPositive' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeNonPositive') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1.0.0
reallyUnsafeNonPositive :: a -> NonPositive a
reallyUnsafeNonPositive = UnsafeNonPositive
