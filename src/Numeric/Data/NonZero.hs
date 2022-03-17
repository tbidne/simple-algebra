{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Provides the 'NonZero' type for enforcing a non-zero invariant.
--
-- @since 0.1.0.0
module Numeric.Data.NonZero
  ( -- * Type
    NonZero (MkNonZero),

    -- * Creation
    mkNonZero,
    mkNonZeroTH,
    unsafeNonZero,
    reallyUnsafeNonZero,

    -- * Elimination
    unNonZero,
  )
where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))

-- | Smart-constructor for creating a \"non-zero\" @a@.
-- 'NonZero' is a:
--
-- * 'Numeric.Algebra.Multiplicative.MSemigroup.MSemigroup'
-- * 'Numeric.Algebra.Multiplicative.MMonoid.MMonoid'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroup'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroupIntegral'
--
-- @since 0.1.0.0
type NonZero :: Type -> Type
newtype NonZero a = UnsafeNonZero a
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | Unidirectional pattern synonym for 'NonZero'. This allows us to pattern
-- match on a nonzero term without exposing the unsafe internal details.
--
-- @since 0.1.0.0
pattern MkNonZero :: a -> NonZero a
pattern MkNonZero x <- UnsafeNonZero x

{-# COMPLETE MkNonZero #-}

-- | Unwraps a 'NonZero'.
--
-- @since 0.1.0.0
unNonZero :: NonZero a -> a
unNonZero (UnsafeNonZero x) = x

-- | Smart constructor for 'NonZero'.
--
-- ==== __Examples__
-- >>> mkNonZero 7
-- Just (UnsafeNonZero 7)
--
-- >>> mkNonZero 0
-- Nothing
--
-- @since 0.1.0.0
mkNonZero :: (Eq a, Num a) => a -> Maybe (NonZero a)
mkNonZero x
  | x == 0 = Nothing
  | otherwise = Just (UnsafeNonZero x)

-- | Template-haskell version of 'mkNonZero' for creating 'NonZero'
-- at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkNonZeroTH 7)
-- UnsafeNonZero 7
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkNonZeroTH :: (Eq a, Lift a, Num a) => a -> Code Q (NonZero a)
#else
mkNonZeroTH :: (Eq a, Lift a, Num a) => a -> Q (TExp (NonZero a))
#endif
mkNonZeroTH x
  | x == 0 = error "Numeric.Data.NonZero.mkNonZeroTH: Passed 0"
  | otherwise = liftTyped (UnsafeNonZero x)

-- | Variant of 'mkNonZero' that throws an error when given 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeNonZero 7
-- UnsafeNonZero 7
--
-- @since 0.1.0.0
unsafeNonZero :: (Eq a, HasCallStack, Num a) => a -> NonZero a
unsafeNonZero x
  | x == 0 = error "Numeric.Data.NonZero.unsafeNonZero: Passed 0"
  | otherwise = UnsafeNonZero x

-- | This function is an alias for the unchecked constructor @UnsafeNonZero@
-- i.e. it allows us to construct a 'NonZero' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeNonZero') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1.0.0
reallyUnsafeNonZero :: a -> NonZero a
reallyUnsafeNonZero = UnsafeNonZero
