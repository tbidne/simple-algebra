{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

-- | Provides the 'MMonoid' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MMonoid
  ( MMonoid (..),
    pattern One,
    pattern NonOne,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup)

-- | Defines a monoid over a multiplicative semigroup.
--
-- @since 0.1
type MMonoid :: Type -> Constraint
class (MSemigroup m) => MMonoid m where
  -- | @since 0.1
  one :: m

-- | Pattern synonym for 'one'.
--
-- @since 0.1
pattern One :: (MMonoid m, Eq m) => m
pattern One <- ((== one) -> True)

-- | Pattern synonym for @x /= 'one'@.
--
-- @since 0.1
pattern NonOne :: (MMonoid m, Eq m) => m -> m
pattern NonOne y <- (\x -> (x == one, x) -> (False, y))

-- see NOTE: [Pattern Synonym COMPLETE]

#if MIN_VERSION_base(4, 16, 0)
{-# COMPLETE One, NonOne #-}
#endif

-- | @since 0.1
instance MMonoid Double where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Float where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Int where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Int8 where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Int16 where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Int32 where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Int64 where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Integer where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Word where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Word8 where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Word16 where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Word32 where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Word64 where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid Natural where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid (Ratio Integer) where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance MMonoid (Ratio Natural) where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance (RealFloat a) => MMonoid (Complex a) where
  one = 1
  {-# INLINE one #-}

-- | @since 0.1
instance (HasResolution k) => MMonoid (Fixed k) where
  one = 1
  {-# INLINE one #-}
