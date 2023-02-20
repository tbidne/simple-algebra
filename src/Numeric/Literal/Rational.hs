-- | Provides the 'FromRational' typeclass.
--
-- @since 0.1
module Numeric.Literal.Rational
  ( FromRational (..),
  )
where

import Data.Complex (Complex)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import GHC.Natural (Natural)
import GHC.Stack.Types (HasCallStack)

-- | Replaces base's @fromRational@ functionality for when we do not have a
-- 'Fractional' instance.
--
-- @
-- 5.5 :: Fractional a => a
--
-- -- becomes
--
-- afromRational 5.5 :: Fractional a => a
-- @
--
-- Note that @fromRational@'s deficiencies are inherited e.g. 'Natural' is
-- partial, bounded types have over/underflow issues.
--
-- @since 0.1
type FromRational :: Type -> Constraint
class FromRational a where
  -- | @since 0.1
  afromRational :: (HasCallStack) => Rational -> a

-- | @since 0.1
instance FromRational Double where
  afromRational = fromRational
  {-# INLINE afromRational #-}

-- | @since 0.1
instance FromRational Float where
  afromRational = fromRational
  {-# INLINE afromRational #-}

-- | @since 0.1
instance FromRational (Ratio Integer) where
  afromRational = fromRational
  {-# INLINE afromRational #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance FromRational (Ratio Natural) where
  afromRational = fromRational
  {-# INLINE afromRational #-}

-- | @since 0.1
instance (RealFloat a) => FromRational (Complex a) where
  afromRational = fromRational
  {-# INLINE afromRational #-}
