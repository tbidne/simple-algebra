{-# LANGUAGE CPP #-}

-- | Provides typeclasses for division.
--
-- @since 0.1
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
import Data.Kind (Constraint, Type)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Real (Ratio (..))
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Data.NonZero (NonZero (..), reallyUnsafeNonZero, unNonZero)

-- $setup
-- >>> :set -XTemplateHaskell

-- | Defines a multiplicative group.
--
-- @since 0.1
type MGroup :: Type -> Constraint
class MMonoid g => MGroup g where
  -- | @since 0.1
  (.%.) :: g -> NonZero g -> g

infixl 7 .%.

-- | @since 0.1
instance MGroup Double where
  x .%. MkNonZero d = x / d

-- | @since 0.1
instance MGroup Float where
  x .%. MkNonZero d = x / d

-- | @since 0.1
instance MGroup Int where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Int8 where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Int16 where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Int32 where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Int64 where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Integer where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Word where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Word8 where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Word16 where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Word32 where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Word64 where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup Natural where
  x .%. MkNonZero d = x `div` d

-- | @since 0.1
instance MGroup (Ratio Integer) where
  x .%. d = x .*. flipNonZero d

-- | @since 0.1
instance MGroup (Ratio Natural) where
  x .%. d = x .*. flipNonZero d

-- $nonzero
-- These functions mirror those in "Numeric.Data.NonZero" except they are
-- based on 'AMonoid'\'s 'zero', not the literal 0.

-- | Smart constructor for 'NonZero', based on its additive monoid instance.
--
-- ==== __Examples__
-- >>> mkAMonoidNonZero 7
-- Just (UnsafeNonZero {unNonZero = 7})
--
-- >>> mkAMonoidNonZero 0
-- Nothing
--
-- @since 0.1
mkAMonoidNonZero :: AMonoid g => g -> Maybe (NonZero g)
mkAMonoidNonZero x
  | x == zero = Nothing
  | otherwise = Just (reallyUnsafeNonZero x)

-- | Template-haskell version of 'mkAMonoidNonZero' for creating 'NonZero'
-- at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkAMonoidNonZeroTH 7)
-- UnsafeNonZero {unNonZero = 7}
--
-- @since 0.1
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
-- UnsafeNonZero {unNonZero = 7}
--
-- @since 0.1
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
-- @since 0.1
type MGroupIntegral :: Type -> Constraint
class MGroup g => MGroupIntegral g where
  type ModResult g

  -- | @since 0.1
  mmod :: g -> NonZero g -> ModResult g
  mmod x d = snd $ mdivMod x d

  -- | @since 0.1
  mdivMod :: g -> NonZero g -> (g, ModResult g)
  mdivMod x d = (x .%. d, x `mmod` d)

  {-# MINIMAL (mdivMod | mmod) #-}

-- | @since 0.1
instance MGroupIntegral Int where
  type ModResult Int = Int
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Int8 where
  type ModResult Int8 = Int8
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Int16 where
  type ModResult Int16 = Int16
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Int32 where
  type ModResult Int32 = Int32
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Int64 where
  type ModResult Int64 = Int64
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Integer where
  type ModResult Integer = Integer
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Word where
  type ModResult Word = Word
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Word8 where
  type ModResult Word8 = Word8
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Word16 where
  type ModResult Word16 = Word16
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Word32 where
  type ModResult Word32 = Word32
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Word64 where
  type ModResult Word64 = Word64
  x `mdivMod` MkNonZero d = x `divMod` d

-- | @since 0.1
instance MGroupIntegral Natural where
  type ModResult Natural = Natural
  x `mdivMod` MkNonZero d = x `divMod` d
