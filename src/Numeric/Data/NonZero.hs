{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NonZero' type for enforcing a non-zero invariant.
--
-- @since 0.1
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
import Data.Data (Data)
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Class.Literal (NumLiteral (..))
import Optics.Core (A_Getter, LabelOptic (..), to)

-- | Smart-constructor for creating a \"non-zero\" @a@.
--
-- @since 0.1
type NonZero :: Type -> Type
newtype NonZero a = UnsafeNonZero
  { -- | @since 0.1
    unNonZero :: a
  }
  deriving stock
    ( -- | @since 0.1
      Data,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance (k ~ A_Getter, a ~ m, b ~ m) => LabelOptic "unNonZero" k (NonZero m) (NonZero m) a b where
  labelOptic = to unNonZero

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Num a, Ord a) => NumLiteral (NonZero a) where
  fromLit = unsafeNonZero . fromInteger

-- | Unidirectional pattern synonym for 'NonZero'. This allows us to pattern
-- match on a nonzero term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkNonZero :: a -> NonZero a
pattern MkNonZero x <- UnsafeNonZero x

{-# COMPLETE MkNonZero #-}

-- | Smart constructor for 'NonZero'.
--
-- ==== __Examples__
-- >>> mkNonZero 7
-- Just (UnsafeNonZero {unNonZero = 7})
--
-- >>> mkNonZero 0
-- Nothing
--
-- @since 0.1
mkNonZero :: (Eq a, Num a) => a -> Maybe (NonZero a)
mkNonZero x
  | x == 0 = Nothing
  | otherwise = Just (UnsafeNonZero x)

-- | Template-haskell version of 'mkNonZero' for creating 'NonZero'
-- at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkNonZeroTH 7)
-- UnsafeNonZero {unNonZero = 7}
--
-- @since 0.1
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
-- >>> UnsafeNonZero {unNonZero = 7}
-- UnsafeNonZero {unNonZero = 7}
--
-- @since 0.1
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
-- @since 0.1
reallyUnsafeNonZero :: a -> NonZero a
reallyUnsafeNonZero = UnsafeNonZero
