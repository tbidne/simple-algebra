-- | Provides the 'FromInteger' typeclass.
--
-- @since 0.1
module Numeric.Literal.Integer
  ( FromInteger (..),
  )
where

import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Replaces base's 'fromInteger' functionality for when we do not have a
-- 'Num' instance.
--
-- @
-- 1_000 :: Num a => a
--
-- -- becomes
--
-- afromInteger 1_000 :: FromInteger a => a
-- @
--
-- Note that 'fromInteger''s deficiencies are inherited e.g. 'Natural' is
-- partial, bounded types have over/underflow issues.
--
-- @since 0.1
type FromInteger :: Type -> Constraint
class FromInteger a where
  -- | @since 0.1
  afromInteger :: Integer -> a

-- | @since 0.1
instance FromInteger Double where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Float where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Int where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Int8 where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Int16 where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Int32 where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Int64 where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Integer where
  afromInteger = id
  {-# INLINE afromInteger #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance FromInteger Natural where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Word where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Word8 where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Word16 where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Word32 where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger Word64 where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromInteger (Ratio Integer) where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance FromInteger (Ratio Natural) where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance RealFloat a => FromInteger (Complex a) where
  afromInteger = fromInteger
  {-# INLINE afromInteger #-}
