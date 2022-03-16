{-# LANGUAGE CPP #-}

-- | Provides the 'Positive' type for enforcing a positive invariant.
--
-- @since 0.1.0.0
module Numeric.Data.Positive
  ( -- * Type
    Positive (MkPositive),

    -- * Creation
    mkPositiveTH,
    mkPositive,
    unsafePositive,
    reallyUnsafePositive,

    -- * Elimination
    unPositive,
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

-- | Newtype wrapper that attaches a 'Positive' invariant to some @a@.
--
-- @since 0.1.0.0
type Positive :: Type -> Type
newtype Positive a = UnsafePositive a
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

pattern MkPositive :: a -> Positive a
pattern MkPositive x <- UnsafePositive x

{-# COMPLETE MkPositive #-}

-- | Unwraps a 'Positive'.
--
-- @since 0.1.0.0
unPositive :: Positive a -> a
unPositive (UnsafePositive x) = x

-- | Template haskell for creating a 'Positive' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkPositiveTH 1)
-- UnsafePositive 1
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkPositiveTH :: (Integral a, Lift a, Show a) => a -> a -> Code Q (Positive a)
#else
mkPositiveTH :: (Integral a, Lift a, Show a) => a -> Q (TExp (Positive a))
#endif
mkPositiveTH x = maybe (error err) liftTyped $ mkPositive x
  where
    err =
      "Numeric.Data.Positive.mkPositiveTH: Passed value <= 0: "
        <> show x

-- | Smart constructor for 'Positive'. Returns 'Nothing' if the second
-- parameter is @<= 0@.
--
-- ==== __Examples__
-- >>> mkPositive 7
-- Just (UnsafePositive 7)
--
-- >>> mkPositive 0
-- Nothing
--
-- @since 0.1.0.0
mkPositive :: (Num a, Ord a) => a -> Maybe (Positive a)
mkPositive x
  | x > 0 = Just (UnsafePositive x)
  | otherwise = Nothing

-- | Variant of 'mkPositive' that throws an error when given a value <= 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafePositive 7
-- UnsafePositive 7
--
-- @since 0.1.0.0
unsafePositive :: (HasCallStack, Num a, Ord a, Show a) => a -> Positive a
unsafePositive x
  | x > 0 = UnsafePositive x
  | otherwise =
      error $
        "Numeric.Data.Positive.unsafePositive: Passed value <= 0: " <> show x

-- | This function is an alias for the unchecked constructor @UnsafePositive@
-- i.e. it allows us to construct a 'Positive' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafePositive') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1.0.0
reallyUnsafePositive :: a -> Positive a
reallyUnsafePositive = UnsafePositive
