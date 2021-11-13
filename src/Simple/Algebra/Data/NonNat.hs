-- | Provides the 'NonNat' type for enforcing a "Not n" invariant.
--
-- @since 0.1.0.0
module Simple.Algebra.Data.NonNat
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
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Simple.Algebra.Additive (Additive (..))
import Simple.Algebra.Multiplicative (Multiplicative (..))
import Simple.Algebra.MultiplicativeMonoid (MultiplicativeMonoid (..))
import Text.Read qualified as TR

-- | Newtype wrapper over /a/ which excludes some 'Nat' /x/. That is,
-- @NonNat x a@ is some \(n \in a \) such that \(n \neq x\).
--
-- @since 0.1.0.0
type NonNat :: Nat -> Type -> Type
newtype NonNat x a = MkUnsafeNonNat
  { -- | Unwraps the 'NonNat'
    --
    -- @since 0.1.0.0
    unNonNat :: a
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'NonNat'.
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
mkNonNat :: forall x a. (Eq a, KnownNat x, Num a) => a -> Maybe (NonNat x a)
mkNonNat n
  | n /= excluded = Just $ MkUnsafeNonNat n
  | otherwise = Nothing
  where
    excluded = fromInteger $ toInteger $ natVal $ Proxy @x

-- | Unsafe constructor for 'NonNat', intended to be used with
-- known constants, e.g. @unsafeNonNat \@0 50@. Exercise restraint!
--
-- >>> unsafeNonNat @7 10
-- MkUnsafeNonNat {unNonNat = 10}
--
-- >>> unsafeNonNat @7 7
-- Passed invalid NonNat: 7
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
readNonNat :: (KnownNat x, Num a, Ord a, Read a) => String -> Maybe (NonNat x a)
readNonNat = TR.readMaybe >=> mkNonNat

-- | 'NonNat' specialized to 0.
--
-- @since 0.1.0.0
type NonZero = NonNat 0

-- | Allows pattern matching on 'NonZero'.
--
-- @since 0.1.0.0
pattern MkNonZero :: a -> NonZero a
pattern MkNonZero n <- MkUnsafeNonNat n

-- | Unwraps the 'NonZero'.
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
mkNonZero :: (Eq a, Num a) => a -> Maybe (NonNat 0 a)
mkNonZero = mkNonNat @0

-- | Unsafe constructor for 'NonZero', intended to be used with
-- known constants, e.g. @unsafeNonZero 50@. Exercise restraint!
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
readNonZero :: (Num a, Ord a, Read a) => String -> Maybe (NonZero a)
readNonZero = TR.readMaybe >=> mkNonZero

-- | @since 0.1.0.0
instance Additive (NonZero Natural) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

-- | @since 0.1.0.0
instance Additive (NonZero Word) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

-- | @since 0.1.0.0
instance Additive (NonZero Word8) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

-- | @since 0.1.0.0
instance Additive (NonZero Word16) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

-- | @since 0.1.0.0
instance Additive (NonZero Word32) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

-- | @since 0.1.0.0
instance Additive (NonZero Word64) where
  MkNonZero x .+. MkNonZero y = unsafeNonZero $ x + y

-- | @since 0.1.0.0
instance (Num a, Ord a) => Multiplicative (NonZero a) where
  MkNonZero x .*. MkNonZero y = unsafeNonZero $ x * y

-- | @since 0.1.0.0
instance (Num a, Ord a) => MultiplicativeMonoid (NonZero a) where
  one = unsafeNonZero 1
