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
import Numeric.Data.Fraction (Fraction)
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
