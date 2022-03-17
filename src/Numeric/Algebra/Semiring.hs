-- | Provides the 'Semiring' typeclass.
--
-- @since 0.1.0.0
module Numeric.Algebra.Semiring
  ( Semiring,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat)
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Data.Fraction (Fraction)
import Numeric.Data.ModN (ModN)
import Numeric.Data.ModP (ModP)
import Numeric.Data.NonNegative (NonNegative)
import Numeric.Natural (Natural)

-- | Defines a semiring.
--
-- @since 0.1.0.0
class (AMonoid r, MMonoid r) => Semiring r

-- | @since 0.1.0.0
instance Semiring Double

-- | @since 0.1.0.0
instance Semiring Float

-- | @since 0.1.0.0
instance Semiring Int

-- | @since 0.1.0.0
instance Semiring Int8

-- | @since 0.1.0.0
instance Semiring Int16

-- | @since 0.1.0.0
instance Semiring Int32

-- | @since 0.1.0.0
instance Semiring Int64

-- | @since 0.1.0.0
instance Semiring Integer

-- | @since 0.1.0.0
instance Semiring Natural

-- | @since 0.1.0.0
instance Semiring Word

-- | @since 0.1.0.0
instance Semiring Word8

-- | @since 0.1.0.0
instance Semiring Word16

-- | @since 0.1.0.0
instance Semiring Word32

-- | @since 0.1.0.0
instance Semiring Word64

-- | @since 0.1.0.0
instance Semiring (Ratio Integer)

-- | @since 0.1.0.0
instance Semiring (Ratio Natural)

-- | @since 0.1.0.0
instance Semiring (Fraction Integer)

-- | @since 0.1.0.0
instance Semiring (Fraction Natural)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Int)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Int8)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Int16)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Int32)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Int64)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Integer)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Word)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Word8)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Word16)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Word32)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Word64)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Natural)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Int)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Int8)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Int16)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Int32)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Int64)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Integer)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Word)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Word8)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Word16)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Word32)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Word64)

-- | @since 0.1.0.0
instance KnownNat p => Semiring (ModP p Natural)

-- | @since 0.1.0.0
instance Semiring (NonNegative Float)

-- | @since 0.1.0.0
instance Semiring (NonNegative Double)

-- | @since 0.1.0.0
instance Semiring (NonNegative Int)

-- | @since 0.1.0.0
instance Semiring (NonNegative Int8)

-- | @since 0.1.0.0
instance Semiring (NonNegative Int16)

-- | @since 0.1.0.0
instance Semiring (NonNegative Int32)

-- | @since 0.1.0.0
instance Semiring (NonNegative Int64)

-- | @since 0.1.0.0
instance Semiring (NonNegative Integer)

-- | @since 0.1.0.0
instance Semiring (NonNegative Word)

-- | @since 0.1.0.0
instance Semiring (NonNegative Word8)

-- | @since 0.1.0.0
instance Semiring (NonNegative Word16)

-- | @since 0.1.0.0
instance Semiring (NonNegative Word32)

-- | @since 0.1.0.0
instance Semiring (NonNegative Word64)

-- | @since 0.1.0.0
instance Semiring (NonNegative Natural)
