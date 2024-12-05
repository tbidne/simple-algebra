-- | Provides the 'FromRational' typeclass.
--
-- @since 0.1
module Numeric.Literal.Rational
  ( FromRational (..),
    ToRational (..),
  )
where

import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Stack.Types (HasCallStack)

{- HLINT ignore FromRational "Redundant bracket" -}

-- | Replaces base's @fromRational@ functionality for when we do not have a
-- 'Fractional' instance.
--
-- @
-- 5.5 :: Fractional a => a
--
-- -- becomes
--
-- fromQ 5.5 :: FromRational a => a
-- @
--
-- Note that @fromRational@'s deficiencies are inherited e.g. 'Natural' is
-- partial, bounded types have over/underflow issues.
--
-- @since 0.1
type FromRational :: Type -> Constraint
class FromRational a where
  -- | @since 0.1
  fromQ :: (HasCallStack) => Rational -> a

-- | @since 0.1
instance FromRational Double where
  fromQ = fromRational
  {-# INLINE fromQ #-}

-- | @since 0.1
instance FromRational Float where
  fromQ = fromRational
  {-# INLINE fromQ #-}

-- | @since 0.1
instance FromRational (Ratio Integer) where
  fromQ = fromRational
  {-# INLINE fromQ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance FromRational (Ratio Natural) where
  fromQ = fromRational
  {-# INLINE fromQ #-}

-- | @since 0.1
instance (RealFloat a) => FromRational (Complex a) where
  fromQ = fromRational
  {-# INLINE fromQ #-}

-- | Rational embedding.
--
-- @since 0.1
type ToRational :: Type -> Constraint
class ToRational a where
  -- | @since 0.1
  toQ :: (HasCallStack) => a -> Rational

-- | @since 0.1
instance ToRational Double where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Float where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Int where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Int8 where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Int16 where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Int32 where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Int64 where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Integer where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Natural where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Word where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Word8 where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Word16 where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Word32 where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational Word64 where
  toQ = toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational (Ratio Integer) where
  toQ = fromRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance ToRational (Ratio Natural) where
  toQ = toRational
  {-# INLINE toQ #-}
