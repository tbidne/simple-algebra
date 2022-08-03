-- | Provides the 'NumLiteral' typeclass.
--
-- @since 0.1
module Numeric.Class.Literal
  ( NumLiteral (..),
  )
where

import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Replaces base's 'fromInteger' and 'fromRational' functionality for
-- when we do not have a 'Num' / 'Fractional' instance.
--
-- @
-- 1_000 :: Num a => a
-- 5.7 :: Fractional a => a
--
-- -- becomes
--
-- fromLit 1_000 :: NumLiteral a => a
-- fromLit 5.7 :: NumLiteral a => a
-- @
--
-- We favor 'fromRational' over 'fromInteger' when both are available. Note
-- that the deficiencies of these functions is inherited e.g. 'Natural' is
-- partial, bounded types have over/underflow issues.
--
-- @since 0.1
type NumLiteral :: Type -> Constraint
class NumLiteral a where
  -- | The type from which to convert. For types with a 'Fractional' instance,
  -- this should be 'Rational' to maximize expressiveness. Otherwise
  -- (i.e. 'Num' only) it should be Integer.
  type Literal a

  -- | @since 0.1
  fromLit :: Literal a -> a

-- | @since 0.1
instance NumLiteral Double where
  type Literal Double = Rational
  fromLit = fromRational
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Float where
  type Literal Float = Rational
  fromLit = fromRational
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Int where
  type Literal Int = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Int8 where
  type Literal Int8 = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Int16 where
  type Literal Int16 = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Int32 where
  type Literal Int32 = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Int64 where
  type Literal Int64 = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Integer where
  type Literal Integer = Integer
  fromLit = id
  {-# INLINE fromLit #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance NumLiteral Natural where
  type Literal Natural = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Word where
  type Literal Word = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Word8 where
  type Literal Word8 = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Word16 where
  type Literal Word16 = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Word32 where
  type Literal Word32 = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Word64 where
  type Literal Word64 = Integer
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral (Ratio Integer) where
  type Literal (Ratio Integer) = Rational
  fromLit = fromRational
  {-# INLINE fromLit #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance NumLiteral (Ratio Natural) where
  type Literal (Ratio Natural) = Rational
  fromLit = fromRational
  {-# INLINE fromLit #-}

-- | @since 0.1
instance RealFloat a => NumLiteral (Complex a) where
  type Literal (Complex a) = Rational
  fromLit = fromRational
  {-# INLINE fromLit #-}
