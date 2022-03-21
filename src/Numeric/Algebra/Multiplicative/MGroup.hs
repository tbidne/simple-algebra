{-# LANGUAGE CPP #-}

-- | Provides typeclasses for division.
--
-- @since 0.1.0.0
module Numeric.Algebra.Multiplicative.MGroup
  ( -- * Typeclasses
    MGroup (..),
    MGroupIntegral (..),

    -- * NonZero
    NonZero (MkNonZero),
    unNonZero,

    -- ** Creation
    -- $nonzero
    mkAMonoidNonZero,
    mkAMonoidNonZeroTH,
    unsafeAMonoidNonZero,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Real (Ratio (..))
import GHC.TypeNats (KnownNat)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Class.Division (Division (..))
import Numeric.Data.Fraction (Fraction (..))
import Numeric.Data.ModP (ModP (..))
import Numeric.Data.ModP qualified as ModP
import Numeric.Data.NonNegative (NonNegative (..), reallyUnsafeNonNegative)
import Numeric.Data.NonZero (NonZero (..), reallyUnsafeNonZero, unNonZero)
import Numeric.Data.Positive (Positive (..), reallyUnsafePositive)

-- $setup
-- >>> :set -XTemplateHaskell

-- | Defines a multiplicative group.
--
-- @since 0.1.0.0
class MMonoid g => MGroup g where
  -- | Possible constraint on the second argument to '(.%.)' e.g. for
  -- preventing division by zero.
  --
  -- @since 0.1.0.0
  type DivConstraint g

  -- | @since 0.1.0.0
  (.%.) :: g -> DivConstraint g -> g

infixl 7 .%.

-- | @since 0.1.0.0
instance MGroup Double where
  type DivConstraint Double = NonZero Double
  x .%. MkNonZero d = x / d

-- | @since 0.1.0.0
instance MGroup Float where
  type DivConstraint Float = NonZero Float
  x .%. MkNonZero d = x / d

-- | @since 0.1.0.0
instance MGroup Int where
  type DivConstraint Int = NonZero Int
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Int8 where
  type DivConstraint Int8 = NonZero Int8
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Int16 where
  type DivConstraint Int16 = NonZero Int16
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Int32 where
  type DivConstraint Int32 = NonZero Int32
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Int64 where
  type DivConstraint Int64 = NonZero Int64
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Integer where
  type DivConstraint Integer = NonZero Integer
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Word where
  type DivConstraint Word = NonZero Word
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Word8 where
  type DivConstraint Word8 = NonZero Word8
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Word16 where
  type DivConstraint Word16 = NonZero Word16
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Word32 where
  type DivConstraint Word32 = NonZero Word32
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Word64 where
  type DivConstraint Word64 = NonZero Word64
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup Natural where
  type DivConstraint Natural = NonZero Natural
  x .%. MkNonZero d = x `div` d

-- | @since 0.1.0.0
instance MGroup (Ratio Integer) where
  type DivConstraint (Ratio Integer) = NonZero (Ratio Integer)
  x .%. d = x .*. flipNonZero d

-- | @since 0.1.0.0
instance MGroup (Ratio Natural) where
  type DivConstraint (Ratio Natural) = NonZero (Ratio Natural)
  x .%. d = x .*. flipNonZero d

-- | @since 0.1.0.0
instance MGroup (Fraction Integer) where
  type DivConstraint (Fraction Integer) = NonZero (Fraction Integer)
  x .%. MkNonZero (n :%: d) = x .*. (d :%: n)

-- | @since 0.1.0.0
instance MGroup (Fraction Natural) where
  type DivConstraint (Fraction Natural) = NonZero (Fraction Natural)
  x .%. MkNonZero (n :%: d) = x .*. (d :%: n)

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Integer) where
  type DivConstraint (ModP p Integer) = NonZero (ModP p Integer)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Natural) where
  type DivConstraint (ModP p Natural) = NonZero (ModP p Natural)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance (Eq a, Division a, Num a, Ord a, Show a) => MGroup (NonNegative a) where
  type DivConstraint (NonNegative a) = NonZero (NonNegative a)
  MkNonNegative x .%. MkNonZero (MkNonNegative d) = reallyUnsafeNonNegative $ x `divide` d

-- | @since 0.1.0.0
instance (Eq a, Division a, Num a) => MGroup (NonZero a) where
  type DivConstraint (NonZero a) = NonZero a
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `divide` d

-- | @since 0.1.0.0
instance (Eq a, Division a, Num a, Ord a, Show a) => MGroup (Positive a) where
  type DivConstraint (Positive a) = Positive a
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `divide` d

-- $nonzero
-- These functions mirror those in "Numeric.Data.NonZero" except they are
-- based on 'AMonoid'\'s 'zero', not the literal 0.

-- | Smart constructor for 'NonZero', based on its additive monoid instance.
--
-- ==== __Examples__
-- >>> mkAMonoidNonZero 7
-- Just (UnsafeNonZero 7)
--
-- >>> mkAMonoidNonZero 0
-- Nothing
--
-- @since 0.1.0.0
mkAMonoidNonZero :: AMonoid g => g -> Maybe (NonZero g)
mkAMonoidNonZero x
  | x == zero = Nothing
  | otherwise = Just (reallyUnsafeNonZero x)

-- | Template-haskell version of 'mkAMonoidNonZero' for creating 'NonZero'
-- at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkAMonoidNonZeroTH 7)
-- UnsafeNonZero 7
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkAMonoidNonZeroTH :: (AMonoid g, Lift g) => g -> Code Q (NonZero g)
#else
mkAMonoidNonZeroTH :: (AMonoid g, Lift g) => g -> Q (TExp (NonZero g))
#endif
mkAMonoidNonZeroTH x
  | x == zero =
      error
        "Numeric.Algebra.Multiplicative.MGroup.mkAMonoidNonZeroTH: Passed identity"
  | otherwise = liftTyped (reallyUnsafeNonZero x)

-- | Unsafe constructor for 'NonZero', based on its additive monoid instance.
-- Intended to be used with known constants. Exercise restraint!
--
-- ==== __Examples__
-- >>> unsafeAMonoidNonZero 7
-- UnsafeNonZero 7
--
-- @since 0.1.0.0
unsafeAMonoidNonZero :: AMonoid g => g -> NonZero g
unsafeAMonoidNonZero x
  | x == zero =
      error
        "Numeric.Algebra.Multiplicative.MGroup.unsafeAMonoidNonZero: Passed identity"
  | otherwise = reallyUnsafeNonZero x

flipNonZero :: Fractional a => NonZero a -> a
flipNonZero (MkNonZero x) = recip x

-- | Additional functions for "integral" 'MGroup's.
--
-- @since 0.1.0.0
class MGroup g => MGroupIntegral g where
  type ModResult g

  -- | @since 0.1.0.0
  gmod :: g -> DivConstraint g -> ModResult g
  gmod x d = snd $ gdivMod x d

  -- | @since 0.1.0.0
  gdivMod :: g -> DivConstraint g -> (g, ModResult g)
  gdivMod x d = (x .%. d, x `gmod` d)

  {-# MINIMAL (gdivMod | gmod) #-}

-- | @since 0.1.0.0
instance MGroupIntegral Int where
  type ModResult Int = Int
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Int8 where
  type ModResult Int8 = Int8
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Int16 where
  type ModResult Int16 = Int16
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Int32 where
  type ModResult Int32 = Int32
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Int64 where
  type ModResult Int64 = Int64
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Integer where
  type ModResult Integer = Integer
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Word where
  type ModResult Word = Word
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Word8 where
  type ModResult Word8 = Word8
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Word16 where
  type ModResult Word16 = Word16
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Word32 where
  type ModResult Word32 = Word32
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Word64 where
  type ModResult Word64 = Word64
  x `gdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral Natural where
  type ModResult Natural = Natural
  x `gdivMod` MkNonZero d = x `divMod` d
