{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'MGroup' typeclass.
--
-- @since 0.1.0.0
module Algebra.Multiplicative.MGroup
  ( -- * Typeclass
    MGroup (..),

    -- * NonZero
    NonZero (MkNonZero, unNonZero),
    mkAMonoidNonZero,
    mkAMonoidNonZeroTH,
    unsafeAMonoidNonZero,

    -- * Refined
    refineAMonoidNonZero,
    refineAMonoidNonZeroTH,
    unsafeRefineAMonoidNonZero,
  )
where

import Algebra.Additive.AMonoid (AMonoid (..))
import Algebra.Multiplicative.MMonoid (MMonoid (..))
import Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Real (Ratio (..))
import Language.Haskell.TH.Syntax (Lift (..), Q, TExp)
import Refined (NonNegative, Refined, type (&&))
import Refined qualified as R
import Refined.Extras (Implies, pattern MkRefined)
import Refined.Unsafe qualified as RUnsafe
import Unsafe.Coerce (unsafeCoerce)

-- $setup
-- >>> :set -XTemplateHaskell

-- | Defines a multiplicative group.
--
-- @since 0.1.0.0
class MMonoid g => MGroup g where
  -- | The 'NZ' type family defines a 'non-zero' type for @g@, referencing
  -- 'AMonoid'\'s 'zero'. Ordinary types can use 'NonZero', though 'Refined'
  -- types can use add a 'R.NonZero' predicate to their type.
  --
  -- Types without an 'AMonoid' instance can instead choose to implement
  -- 'AMonoid' or take @NZ g = g@.
  --
  -- @since 0.1.0.0
  type NZ g

  -- | @since 0.1.0.0
  (.%.) :: g -> NZ g -> g

infixl 7 .%.

-- | @since 0.1.0.0
instance MGroup Double where
  type NZ Double = NonZero Double
  x .%. MkNonZero d = x / d

-- | @since 0.1.0.0
instance MGroup Float where
  type NZ Float = NonZero Float
  x .%. MkNonZero d = x / d

-- | @since 0.1.0.0
instance MGroup Int where
  type NZ Int = NonZero Int
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Int8 where
  type NZ Int8 = NonZero Int8
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Int16 where
  type NZ Int16 = NonZero Int16
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Int32 where
  type NZ Int32 = NonZero Int32
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Int64 where
  type NZ Int64 = NonZero Int64
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Integer where
  type NZ Integer = NonZero Integer
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Natural where
  type NZ Natural = NonZero Natural
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Word where
  type NZ Word = NonZero Word
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Word8 where
  type NZ Word8 = NonZero Word8
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Word16 where
  type NZ Word16 = NonZero Word16
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Word32 where
  type NZ Word32 = NonZero Word32
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Word64 where
  type NZ Word64 = NonZero Word64
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup (Ratio Integer) where
  type NZ (Ratio Integer) = NonZero (Ratio Integer)
  x .%. d = x .*. flipNonZero d

-- | @since 0.1.0.0
instance MGroup (Ratio Natural) where
  type NZ (Ratio Natural) = NonZero (Ratio Natural)
  x .%. d = x .*. flipNonZero d

-- | @since 0.1.0.0
instance (AMonoid a, MGroup a, Num a, Ord a) => MGroup (Refined NonNegative a) where
  type NZ (Refined NonNegative a) = Refined (NonNegative && R.NonZero) a
  MkRefined x .%. d = RUnsafe.unsafeRefine $ x .%. unsafeRefineToNZ d

-- | Smart-constructor for creating a 'non-zero' @a@, where zero is the
-- 'AMonoid' 'zero'.
--
-- @since 0.1.0.0
newtype NonZero a = UnsafeNonZero
  { -- | @since 0.1.0.0
    unNonZero :: a
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )

-- | Unidirectional pattern synonym for 'NonZero'. This allows us to pattern
-- match on a nonzero term without exposing the unsafe internal details.
--
-- @since 0.1.0.0
pattern MkNonZero :: a -> NonZero a
pattern MkNonZero x <- UnsafeNonZero x

{-# COMPLETE MkNonZero #-}

-- | Smart constructor for 'NonZero', based on its additive monoid instance.
--
-- ==== __Examples__
-- >>> mkAMonoidNonZero 7
-- Just (UnsafeNonZero {unNonZero = 7})
--
-- >>> mkAMonoidNonZero 0
-- Nothing
--
-- @since 0.1.0.0
mkAMonoidNonZero :: AMonoid g => g -> Maybe (NonZero g)
mkAMonoidNonZero x
  | x == zero = Nothing
  | otherwise = Just (UnsafeNonZero x)

-- | Template-haskell version of 'mkAMonoidNonZero' for creating 'NonZero'
-- at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkAMonoidNonZeroTH 7)
-- UnsafeNonZero {unNonZero = 7}
--
-- @since 0.1.0.0
mkAMonoidNonZeroTH :: (AMonoid g, Lift g) => g -> Q (TExp (NonZero g))
mkAMonoidNonZeroTH x
  | x == zero = error "Passed identity to mkAMonoidNonZero"
  | otherwise = liftTyped (UnsafeNonZero x)

-- | Unsafe constructor for 'NonZero', based on its additive monoid instance.
-- Intended to be used with known constants. Exercise restraint!
--
-- ==== __Examples__
-- >>> unsafeAMonoidNonZero 7
-- UnsafeNonZero {unNonZero = 7}
--
-- @since 0.1.0.0
unsafeAMonoidNonZero :: AMonoid g => g -> NonZero g
unsafeAMonoidNonZero x
  | x == zero = error "Passed identity to unsafeAMonoidNonZero"
  | otherwise = UnsafeNonZero x

-- | Smart constructor for 'Refined' 'R.NonZero', based on its additive
-- monoid instance. Checks 'zero' /and/ the refinement.
--
-- ==== __Examples__
-- >>> refineAMonoidNonZero 7
-- Just (Refined 7)
--
-- >>> refineAMonoidNonZero 0
-- Nothing
--
-- @since 0.1.0.0
refineAMonoidNonZero :: (AMonoid g, Num g) => g -> Maybe (Refined R.NonZero g)
refineAMonoidNonZero x
  | x == zero = Nothing
  | otherwise = case R.refine x of
    Left _ -> Nothing
    Right re -> Just re

-- | Template-haskell version of 'refineAMonoidNonZero' for creating 'Refined'
-- 'R.NonZero' at compile-time. Checks 'zero' /and/ the refinement.
--
-- ==== __Examples__
-- >>> $$(refineAMonoidNonZeroTH 7)
-- Refined 7
--
-- @since 0.1.0.0
refineAMonoidNonZeroTH :: (AMonoid g, Lift g, Num g) => g -> Q (TExp (Refined R.NonZero g))
refineAMonoidNonZeroTH x
  | x == zero = error "Passed identity to refineAMonoidNonZeroTH"
  | otherwise = R.refineTH x

-- | Unsafe constructor for 'Refined' 'R.NonZero', based on its additive
-- monoid instance. Checks 'zero' /and/ the refinement. Intended to be used
-- with known constants. Exercise restraint!
--
-- ==== __Examples__
-- >>> unsafeRefineAMonoidNonZero 7
-- Refined 7
--
-- @since 0.1.0.0
unsafeRefineAMonoidNonZero :: (AMonoid g, Num g) => g -> Refined R.NonZero g
unsafeRefineAMonoidNonZero x
  | x == zero = error "Passed identity to unsafeRefineAMonoidNonZero"
  | otherwise = RUnsafe.unsafeRefine x

flipNonZero :: Fractional a => NonZero a -> a
flipNonZero (MkNonZero x) = recip x

unsafeRefineToNZ :: (AMonoid g, Implies p R.NonZero) => Refined p g -> NZ g
unsafeRefineToNZ (MkRefined x)
  | x == zero = error "Passed identity to unsafeRefineToNZ!"
  | otherwise = unsafeCoerce x
