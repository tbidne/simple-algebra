-- | @since 0.1
module Numeric.Convert.Internal
  ( -- * Integers
    FromInteger (..),
    ToInteger (..),

    -- * Rationals
    FromRational (..),
    ToRational (..),

    -- * Reals
    FromReal (..),
    ToReal (..),
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Stack.Types (HasCallStack)

-- NOTE: Internal module so that we can give our typeclasses mutual
-- constraints.
--
-- In general, conversions @a -> b@ are intended to be embeddings i.e.
-- well-behaved. This is why e.g. ToInteger requires a ToRational constraint
-- -- if you can embed in Z, you can embed in Q -- and why some instances are
-- missing e.g. fromReal :: Double -> Integer.
--
-- That said, there are still some instances of "bad behavior":
--
-- - Natural instances are partial.
-- - Bounded types may over/underflow.
-- - General floating point issues.

{- HLINT ignore FromInteger "Redundant bracket" -}

-------------------------------------------------------------------------------
----------------------------------- INTEGERS ----------------------------------
-------------------------------------------------------------------------------

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

-- | @since 0.1
instance (HasResolution k) => FromInteger (Fixed k) where
  fromZ = fromInteger
  {-# INLINE fromZ #-}

-- | Integer embedding.
--
-- @since 0.1
type ToInteger :: Type -> Constraint
class (ToRational a) => ToInteger a where
  -- | @since 0.1
  toZ :: (HasCallStack) => a -> Integer

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

-------------------------------------------------------------------------------
---------------------------------- RATIONALS ----------------------------------
-------------------------------------------------------------------------------

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
class (FromInteger a) => FromRational a where
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

-- | @since 0.1
instance (HasResolution k) => FromRational (Fixed k) where
  fromQ = fromRational
  {-# INLINE fromQ #-}

-- | Rational embedding.
--
-- @since 0.1
type ToRational :: Type -> Constraint
class (ToReal a) => ToRational a where
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

-- | @since 0.1
instance (HasResolution k) => ToRational (Fixed k) where
  toQ = toRational
  {-# INLINE toQ #-}

-------------------------------------------------------------------------------
------------------------------------- REALS -----------------------------------
-------------------------------------------------------------------------------

-- | Conversion from 'Double'.
--
-- @since 0.1
type FromReal :: Type -> Constraint
class (FromRational a) => FromReal a where
  -- | @since 0.1
  fromR :: (HasCallStack) => Double -> a

-- | @since 0.1
instance FromReal Double where
  fromR = id
  {-# INLINE fromR #-}

-- | @since 0.1
instance FromReal Float where
  fromR = realToFrac
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

-- | @since 0.1
instance (HasResolution k) => FromReal (Fixed k) where
  fromR = realToFrac
  {-# INLINE fromR #-}

-- | Conversion to Double.
--
-- @since 0.1
type ToReal :: Type -> Constraint
class ToReal a where
  -- | @since 0.1
  toR :: (HasCallStack) => a -> Double

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

-- | @since 0.1
instance (HasResolution k) => ToReal (Fixed k) where
  toR = realToFrac
  {-# INLINE toR #-}
