-- | Provides the 'NumLiteral' typeclass.
--
-- @since 0.1.0.0
module Numeric.Literal
  ( NumLiteral (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat)
import Numeric.Data.Fraction (Fraction)
import Numeric.Data.ModN (ModN (..))
import Numeric.Data.ModP (ModP (..))
import Numeric.Data.Negative (Negative, unsafeNegative)
import Numeric.Data.NonNegative (NonNegative, unsafeNonNegative)
import Numeric.Data.NonPositive (NonPositive, unsafeNonPositive)
import Numeric.Data.Positive (Positive, unsafePositive)

-- | Replaces 'Num'\'s ' 'fromInteger' functionality for when we do not have
-- a 'Num' instance. Instead of, e.g., @1_000 :: Num a => a@ we have
-- @fromLit 1_000 :: NumLiteral a => a@. Unfortunately this is partial for
-- 'Natural' and has overflow issues for finite types.
--
-- @since 0.1.0.0
class NumLiteral a where
  -- | @since 0.1.0.0
  fromLit :: Integer -> a

-- | @since 0.1.0.0
instance NumLiteral Double where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Float where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Int where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Int8 where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Int16 where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Int32 where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Int64 where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Integer where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Natural where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Word where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Word8 where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Word16 where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Word32 where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral Word64 where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Ratio Int) where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Ratio Int8) where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Ratio Int16) where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Ratio Int32) where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Ratio Int64) where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Ratio Integer) where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Ratio Natural) where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Fraction Integer) where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Fraction Natural) where
  fromLit = fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Int) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Int8) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Int16) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Int32) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Int64) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Integer) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Word) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Word8) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Word16) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Word32) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Word64) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat n => NumLiteral (ModN n Natural) where
  fromLit = MkModN . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Int) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Int8) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Int16) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Int32) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Int64) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Integer) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Word) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Word8) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Word16) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Word32) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Word64) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance KnownNat p => NumLiteral (ModP p Natural) where
  fromLit = MkModP . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Negative Float) where
  fromLit = unsafeNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Negative Double) where
  fromLit = unsafeNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Negative Int) where
  fromLit = unsafeNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Negative Int8) where
  fromLit = unsafeNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Negative Int16) where
  fromLit = unsafeNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Negative Int32) where
  fromLit = unsafeNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Negative Int64) where
  fromLit = unsafeNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Negative Integer) where
  fromLit = unsafeNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Float) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Double) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Int) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Int8) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Int16) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Int32) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Int64) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Integer) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Word) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Word8) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Word16) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Word32) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Word64) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonNegative Natural) where
  fromLit = unsafeNonNegative . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonPositive Float) where
  fromLit = unsafeNonPositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonPositive Double) where
  fromLit = unsafeNonPositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonPositive Int) where
  fromLit = unsafeNonPositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonPositive Int8) where
  fromLit = unsafeNonPositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonPositive Int16) where
  fromLit = unsafeNonPositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonPositive Int32) where
  fromLit = unsafeNonPositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonPositive Int64) where
  fromLit = unsafeNonPositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (NonPositive Integer) where
  fromLit = unsafeNonPositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Float) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Double) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Int) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Int8) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Int16) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Int32) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Int64) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Integer) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Word) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Word8) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Word16) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Word32) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Word64) where
  fromLit = unsafePositive . fromInteger

-- | @since 0.1.0.0
instance NumLiteral (Positive Natural) where
  fromLit = unsafePositive . fromInteger
