{-# LANGUAGE CPP #-}

-- | Provides the 'Negative' type for enforcing a negative invariant.
--
-- @since 0.1.0.0
module Numeric.Data.Negative
  ( -- * Type
    Negative (MkNegative),

    -- * Creation
    mkNegativeTH,
    mkNegative,
    unsafeNegative,
    reallyUnsafeNegative,

    -- * Elimination
    unNegative,
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

-- | Newtype wrapper that attaches a 'Negative' invariant to some @a@.
-- 'Negative' is a:
--
-- * 'Numeric.Algebra.Additive.ASemigroup.ASemigroup'
--
-- @since 0.1.0.0
type Negative :: Type -> Type
newtype Negative a = UnsafeNegative a
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

pattern MkNegative :: a -> Negative a
pattern MkNegative x <- UnsafeNegative x

{-# COMPLETE MkNegative #-}

-- | Unwraps a 'Negative'.
--
-- @since 0.1.0.0
unNegative :: Negative a -> a
unNegative (UnsafeNegative x) = x

-- | Template haskell for creating a 'Negative' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkNegativeTH (-1))
-- UnsafeNegative (-1)
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkNegativeTH :: (Integral a, Lift a, Show a) => a -> a -> Code Q (Negative a)
#else
mkNegativeTH :: (Integral a, Lift a, Show a) => a -> Q (TExp (Negative a))
#endif
mkNegativeTH x = maybe (error err) liftTyped $ mkNegative x
  where
    err =
      "Numeric.Data.Positive.mkPositiveTH: Passed value >= 0: "
        <> show x

-- | Smart constructor for 'Negative'. Returns 'Nothing' if the second
-- parameter is @>= 0@.
--
-- ==== __Examples__
-- >>> mkNegative (-1)
-- Just (UnsafeNegative (-1))
--
-- >>> mkNegative 0
-- Nothing
--
-- @since 0.1.0.0
mkNegative :: (Num a, Ord a) => a -> Maybe (Negative a)
mkNegative x
  | x < 0 = Just (UnsafeNegative x)
  | otherwise = Nothing

-- | Variant of 'mkNegative' that throws an error when given a value <= 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeNegative (-1)
-- UnsafeNegative (-1)
--
-- @since 0.1.0.0
unsafeNegative :: (HasCallStack, Num a, Ord a, Show a) => a -> Negative a
unsafeNegative x
  | x < 0 = UnsafeNegative x
  | otherwise =
      error $
        "Numeric.Data.Negative.unsafeNegative: Passed value >= 0: " <> show x

-- | This function is an alias for the unchecked constructor @UnsafeNegative@
-- i.e. it allows us to construct a 'Negative' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeNegative') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1.0.0
reallyUnsafeNegative :: a -> Negative a
reallyUnsafeNegative = UnsafeNegative
