{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

-- | Provides the 'AMonoid' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Additive.AMonoid
  ( AMonoid (..),
    pattern Zero,
    pattern NonZero,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup)

-- | Defines a monoid over an additive semigroup.
--
-- @since 0.1
type AMonoid :: Type -> Constraint
class (ASemigroup m) => AMonoid m where
  -- | Should satisfy:
  --
  -- @
  -- -- identity
  -- x .+. zero = x = zero .+. x
  -- @since 0.1
  zero :: m

-- | Pattern synonym for 'zero'.
--
-- @since 0.1
pattern Zero :: (AMonoid m, Eq m) => m
pattern Zero <- ((== zero) -> True)
  where
    Zero = zero

-- | Pattern synonym for @x /= 'zero'@.
--
-- @since 0.1
pattern NonZero :: (AMonoid m, Eq m) => m -> m
pattern NonZero y <- (\x -> (x == zero, x) -> (False, y))

-- NOTE: [Pattern Synonym COMPLETE]
--
-- Such COMPLETE pragmas not supported in GHC < 9.2:
--
-- - https://gitlab.haskell.org/ghc/ghc/-/issues/19160
-- - https://gitlab.haskell.org/ghc/ghc/-/issues/14422

#if MIN_VERSION_base(4, 16, 0)
{-# COMPLETE Zero, NonZero #-}
#endif

-- | @since 0.1
instance AMonoid Double where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Float where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Int where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Int8 where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Int16 where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Int32 where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Int64 where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Integer where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Word where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Word8 where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Word16 where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Word32 where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Word64 where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid Natural where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid (Ratio Integer) where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance AMonoid (Ratio Natural) where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance (RealFloat a) => AMonoid (Complex a) where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance (HasResolution k) => AMonoid (Fixed k) where
  zero = 0
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a) where
  zero = (zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a) where
  zero = (zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a) where
  zero = (zero, zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero, zero)
  {-# INLINE zero #-}
