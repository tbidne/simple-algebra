-- | Provides the 'NonNat' type for enforcing a "Not n" invariant.
module Simple.NonNat
  ( -- * NonNat
    NonNat (MkNonNat, unNonNat),

    -- ** Creation
    mkNonNat,
    readNonNat,
    unsafeNonNat,

    -- * NonZero
    NonZero,
    pattern MkNonZero,
    unNonZero,

    -- ** Creation
    mkNonZero,
    readNonZero,
    unsafeNonZero,
  )
where

import Control.Monad ((>=>))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, natVal)
import Text.Read qualified as TR

-- | Newtype wrapper over /a/ which excludes some 'Nat' /x/. That is,
-- @NonNat x a@ is some \(n \in a \) such that \(n \neq x\).
type NonNat :: Nat -> Type -> Type
newtype NonNat x a = MkUnsafeNonNat
  { -- | Unwraps the 'NonNat'
    unNonNat :: a
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'NonNat'.
pattern MkNonNat :: a -> NonNat x a
pattern MkNonNat n <- MkUnsafeNonNat n

{-# COMPLETE MkNonNat #-}

-- | Constructs a 'NonNat'.
--
-- >>> mkNonNat @0 50
-- Just (MkUnsafeNonNat {unNonNat = 50})
--
-- >>> mkNonNat @50 50
-- Nothing
mkNonNat :: forall x a. (Eq a, KnownNat x, Num a) => a -> Maybe (NonNat x a)
mkNonNat n
  | n /= excluded = Just $ MkUnsafeNonNat n
  | otherwise = Nothing
  where
    excluded = fromInteger $ toInteger $ natVal $ Proxy @x

-- | Unsafe constructor for 'NonNat', intended to be used with
-- known constants, e.g. @unsafeNonNat \@0 50@. Exercise restraint!
unsafeNonNat :: forall x a. (Eq a, KnownNat x, Num a) => a -> NonNat x a
unsafeNonNat n
  | n /= excluded = MkUnsafeNonNat n
  | otherwise = error $ "Passed invalid NonNat: " <> show excludedNat
  where
    excludedNat = natVal $ Proxy @x
    excluded = fromIntegral excludedNat

-- | Safely attempts to read a 'NonNat'.
--
-- >>> readNonNat @5 "10"
-- Just (MkUnsafeNonNat {unNonNat = 10})
--
-- >>> readNonNat @5 "cat"
-- Nothing
--
-- >>> readNonNat @5 "5"
-- Nothing
readNonNat :: (KnownNat x, Num a, Ord a, Read a) => String -> Maybe (NonNat x a)
readNonNat = TR.readMaybe >=> mkNonNat

-- | 'NonNat' specialized to 0.
type NonZero = NonNat 0

-- | Allows pattern matching on 'NonZero'.
pattern MkNonZero :: a -> NonZero a
pattern MkNonZero n <- MkUnsafeNonNat n

-- | Unwraps the 'NonZero'.
unNonZero :: NonZero a -> a
unNonZero (MkNonZero n) = n

{-# COMPLETE MkNonZero #-}

-- | Constructs a 'NonZero'.
--
-- >>> mkNonZero 50
-- Just (MkUnsafeNonNat {unNonNat = 50})
--
-- >>> mkNonZero 0
-- Nothing
mkNonZero :: (Eq a, Num a) => a -> Maybe (NonNat 0 a)
mkNonZero = mkNonNat @0

-- | Unsafe constructor for 'NonZero', intended to be used with
-- known constants, e.g. @unsafeNonZero 50@. Exercise restraint!
unsafeNonZero :: (Eq a, Num a) => a -> NonNat 0 a
unsafeNonZero = unsafeNonNat @0

-- | Safely attempts to read a 'NonZero'.
--
-- >>> readNonZero "10"
-- Just (MkUnsafeNonNat {unNonNat = 10})
--
-- >>> readNonZero "cat"
-- Nothing
--
-- >>> readNonZero "0"
-- Nothing
readNonZero :: (Num a, Ord a, Read a) => String -> Maybe (NonZero a)
readNonZero = TR.readMaybe >=> mkNonZero
