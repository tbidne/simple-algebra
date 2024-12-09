-- | Provides the 'FromReal' and 'ToReal' typeclasses.
--
-- @since 0.1
module Numeric.Literal.Real
  ( FromReal (..),
    fromℝ,
    ToReal (..),
    toℝ,
  )
where

import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Stack.Types (HasCallStack)

{- HLINT ignore FromReal "Redundant bracket" -}

-- | Conversion from 'Double'.
--
-- @since 0.1
type FromReal :: Type -> Constraint
class FromReal a where
  -- | @since 0.1
  fromR :: (HasCallStack) => Double -> a

-- | Unicode alias for 'fromR', with U+211D.
--
-- @since 0.1
fromℝ :: (FromReal a) => Double -> a
fromℝ = fromR

-- | @since 0.1
instance FromReal Double where
  fromR = id
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Float where
  fromR = realToFrac
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Int8 where
  fromR = floor
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Int16 where
  fromR = floor
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Int32 where
  fromR = floor
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Int64 where
  fromR = floor
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Word8 where
  fromR = floor
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Word16 where
  fromR = floor
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Word32 where
  fromR = floor
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Word64 where
  fromR = floor
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Natural where
  fromR = floor
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal (Ratio Integer) where
  fromR = realToFrac
  {-# INLINE fromR #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance FromReal (Ratio Natural) where
  fromR = realToFrac
  {-# INLINE fromR #-}

-- | @since 0.1
instance (RealFloat a) => FromReal (Complex a) where
  fromR = realToFrac
  {-# INLINE fromR #-}

-- | Conversion to Double.
--
-- @since 0.1
type ToReal :: Type -> Constraint
class ToReal a where
  -- | @since 0.1
  toR :: (HasCallStack) => a -> Double

-- | Unicode alias for 'toR', with U+211D.
--
-- @since 0.1
toℝ :: (ToReal a) => a -> Double
toℝ = toR

-- | @since 0.1
instance ToReal Double where
  toR = id
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Float where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Int where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Int8 where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Int16 where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Int32 where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Int64 where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Integer where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Natural where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Word where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Word8 where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Word16 where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Word32 where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal Word64 where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal (Ratio Integer) where
  toR = realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance ToReal (Ratio Natural) where
  toR = realToFrac
  {-# INLINE toR #-}
