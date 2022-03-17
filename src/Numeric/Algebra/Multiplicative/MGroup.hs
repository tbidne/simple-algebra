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
    -- $nonzero
    mkAMonoidNonZero,
    mkAMonoidNonZeroTH,
    unsafeAMonoidNonZero,
  )
where

#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Data.Bifunctor (Bifunctor (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Real (Ratio (..))
import GHC.TypeNats (KnownNat)
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
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
instance KnownNat p => MGroup (ModP p Int) where
  type DivConstraint (ModP p Int) = NonZero (ModP p Int)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Int8) where
  type DivConstraint (ModP p Int8) = NonZero (ModP p Int8)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Int16) where
  type DivConstraint (ModP p Int16) = NonZero (ModP p Int16)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Int32) where
  type DivConstraint (ModP p Int32) = NonZero (ModP p Int32)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Int64) where
  type DivConstraint (ModP p Int64) = NonZero (ModP p Int64)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Integer) where
  type DivConstraint (ModP p Integer) = NonZero (ModP p Integer)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Word) where
  type DivConstraint (ModP p Word) = NonZero (ModP p Word)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Word8) where
  type DivConstraint (ModP p Word8) = NonZero (ModP p Word8)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Word16) where
  type DivConstraint (ModP p Word16) = NonZero (ModP p Word16)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Word32) where
  type DivConstraint (ModP p Word32) = NonZero (ModP p Word32)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Word64) where
  type DivConstraint (ModP p Word64) = NonZero (ModP p Word64)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance KnownNat p => MGroup (ModP p Natural) where
  type DivConstraint (ModP p Natural) = NonZero (ModP p Natural)
  x .%. d = x .*. ModP.invert d

-- | @since 0.1.0.0
instance MGroup (NonNegative Float) where
  type DivConstraint (NonNegative Float) = NonNegative Float
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x / d

-- | @since 0.1.0.0
instance MGroup (NonNegative Double) where
  type DivConstraint (NonNegative Double) = NonNegative Double
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x / d

-- | @since 0.1.0.0
instance MGroup (NonNegative Int) where
  type DivConstraint (NonNegative Int) = NonNegative Int
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Int8) where
  type DivConstraint (NonNegative Int8) = NonNegative Int8
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Int16) where
  type DivConstraint (NonNegative Int16) = NonNegative Int16
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Int32) where
  type DivConstraint (NonNegative Int32) = NonNegative Int32
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Int64) where
  type DivConstraint (NonNegative Int64) = NonNegative Int64
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Integer) where
  type DivConstraint (NonNegative Integer) = NonNegative Integer
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Word) where
  type DivConstraint (NonNegative Word) = NonNegative Word
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Word8) where
  type DivConstraint (NonNegative Word8) = NonNegative Word8
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Word16) where
  type DivConstraint (NonNegative Word16) = NonNegative Word16
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Word32) where
  type DivConstraint (NonNegative Word32) = NonNegative Word32
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Word64) where
  type DivConstraint (NonNegative Word64) = NonNegative Word64
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonNegative Natural) where
  type DivConstraint (NonNegative Natural) = NonNegative Natural
  MkNonNegative x .%. MkNonNegative d = reallyUnsafeNonNegative $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Float) where
  type DivConstraint (NonZero Float) = NonZero Float
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x / d

-- | @since 0.1.0.0
instance MGroup (NonZero Double) where
  type DivConstraint (NonZero Double) = NonZero Double
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x / d

-- | @since 0.1.0.0
instance MGroup (NonZero Int) where
  type DivConstraint (NonZero Int) = NonZero Int
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Int8) where
  type DivConstraint (NonZero Int8) = NonZero Int8
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Int16) where
  type DivConstraint (NonZero Int16) = NonZero Int16
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Int32) where
  type DivConstraint (NonZero Int32) = NonZero Int32
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Int64) where
  type DivConstraint (NonZero Int64) = NonZero Int64
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Integer) where
  type DivConstraint (NonZero Integer) = NonZero Integer
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Word) where
  type DivConstraint (NonZero Word) = NonZero Word
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Word8) where
  type DivConstraint (NonZero Word8) = NonZero Word8
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Word16) where
  type DivConstraint (NonZero Word16) = NonZero Word16
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Word32) where
  type DivConstraint (NonZero Word32) = NonZero Word32
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Word64) where
  type DivConstraint (NonZero Word64) = NonZero Word64
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (NonZero Natural) where
  type DivConstraint (NonZero Natural) = NonZero Natural
  MkNonZero x .%. MkNonZero d = reallyUnsafeNonZero $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Float) where
  type DivConstraint (Positive Float) = Positive Float
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x / d

-- | @since 0.1.0.0
instance MGroup (Positive Double) where
  type DivConstraint (Positive Double) = Positive Double
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x / d

-- | @since 0.1.0.0
instance MGroup (Positive Int) where
  type DivConstraint (Positive Int) = Positive Int
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Int8) where
  type DivConstraint (Positive Int8) = Positive Int8
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Int16) where
  type DivConstraint (Positive Int16) = Positive Int16
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Int32) where
  type DivConstraint (Positive Int32) = Positive Int32
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Int64) where
  type DivConstraint (Positive Int64) = Positive Int64
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Integer) where
  type DivConstraint (Positive Integer) = Positive Integer
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Word) where
  type DivConstraint (Positive Word) = Positive Word
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Word8) where
  type DivConstraint (Positive Word8) = Positive Word8
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Word16) where
  type DivConstraint (Positive Word16) = Positive Word16
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Word32) where
  type DivConstraint (Positive Word32) = Positive Word32
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Word64) where
  type DivConstraint (Positive Word64) = Positive Word64
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

-- | @since 0.1.0.0
instance MGroup (Positive Natural) where
  type DivConstraint (Positive Natural) = Positive Natural
  MkPositive x .%. MkPositive d = reallyUnsafePositive $ x `div` d

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

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Int) where
  type ModResult (NonNegative Int) = Int
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Int8) where
  type ModResult (NonNegative Int8) = Int8
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Int16) where
  type ModResult (NonNegative Int16) = Int16
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Int32) where
  type ModResult (NonNegative Int32) = Int32
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Int64) where
  type ModResult (NonNegative Int64) = Int64
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Integer) where
  type ModResult (NonNegative Integer) = Integer
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Word) where
  type ModResult (NonNegative Word) = Word
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Word8) where
  type ModResult (NonNegative Word8) = Word8
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Word16) where
  type ModResult (NonNegative Word16) = Word16
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Word32) where
  type ModResult (NonNegative Word32) = Word32
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Word64) where
  type ModResult (NonNegative Word64) = Word64
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonNegative Natural) where
  type ModResult (NonNegative Natural) = Natural
  MkNonNegative x `gdivMod` MkNonNegative d = first reallyUnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Int) where
  type ModResult (NonZero Int) = Int
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Int8) where
  type ModResult (NonZero Int8) = Int8
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Int16) where
  type ModResult (NonZero Int16) = Int16
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Int32) where
  type ModResult (NonZero Int32) = Int32
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Int64) where
  type ModResult (NonZero Int64) = Int64
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Integer) where
  type ModResult (NonZero Integer) = Integer
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Word) where
  type ModResult (NonZero Word) = Word
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Word8) where
  type ModResult (NonZero Word8) = Word8
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Word16) where
  type ModResult (NonZero Word16) = Word16
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Word32) where
  type ModResult (NonZero Word32) = Word32
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Word64) where
  type ModResult (NonZero Word64) = Word64
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d

-- | @since 0.1.0.0
instance MGroupIntegral (NonZero Natural) where
  type ModResult (NonZero Natural) = Natural
  MkNonZero x `gdivMod` MkNonZero d = first reallyUnsafeNonZero $ x `divMod` d
