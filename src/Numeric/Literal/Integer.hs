-- | Provides the 'FromInteger' typeclass.
--
-- @since 0.1
module Numeric.Literal.Integer
  ( FromInteger (..),
    fromℤ,
    ToInteger (..),
    toℤ,
  )
where

import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Stack.Types (HasCallStack)

{- HLINT ignore FromInteger "Redundant bracket" -}

-- | Replaces base's @fromInteger@ functionality for when we do not have a
-- 'Num' instance.
--
-- @
-- 1_000 :: Num a => a
--
-- -- becomes
--
-- fromZ 1_000 :: FromInteger a => a
-- @
--
-- Note that @fromInteger@'s deficiencies are inherited e.g. 'Natural' is
-- partial, bounded types have over/underflow issues.
--
-- @since 0.1
type FromInteger :: Type -> Constraint
class FromInteger a where
  -- | @since 0.1
  fromZ :: (HasCallStack) => Integer -> a

-- | Unicode alias for 'fromZ', with U+2114.
--
-- @since 0.1
fromℤ :: (FromInteger a) => Integer -> a
fromℤ = fromZ

-- | @since 0.1
instance FromInteger Double where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Float where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Int where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Int8 where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Int16 where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Int32 where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Int64 where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Integer where
  fromZ = id
  {-# INLINE fromZ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance FromInteger Natural where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Word where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Word8 where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Word16 where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Word32 where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger Word64 where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance FromInteger (Ratio Integer) where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance FromInteger (Ratio Natural) where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance (RealFloat a) => FromInteger (Complex a) where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | Integer embedding.
--
-- @since 0.1
type ToInteger :: Type -> Constraint
class ToInteger a where
  -- | @since 0.1
  toZ :: (HasCallStack) => a -> Integer

-- | Unicode alias for 'toZ', with U+2114.
--
-- @since 0.1
toℤ :: (ToInteger a) => a -> Integer
toℤ = toZ

-- | @since 0.1
instance ToInteger Int where
  toZ = toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Int8 where
  toZ = toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Int16 where
  toZ = toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Int32 where
  toZ = toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Int64 where
  toZ = toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Integer where
  toZ = id
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Natural where
  toZ = toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Word where
  toZ = toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Word8 where
  toZ = toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Word16 where
  toZ = toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Word32 where
  toZ = toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
instance ToInteger Word64 where
  toZ = toInteger
  {-# INLINE toZ #-}
