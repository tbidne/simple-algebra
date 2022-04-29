-- | Provides the 'AMonoid' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Additive.AMonoid
  ( AMonoid (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))

-- | Defines a monoid over an additive semigroup.
--
-- @since 0.1
class ASemigroup m => AMonoid m where
  -- | @since 0.1
  zero :: m

  -- | Returns @|x|@. Should satisfy
  --
  -- @
  -- Non-negative: aabs x >= zero
  -- Positive-definite: aabs x = 0 <=> x = 0
  -- Triangle equality: aabs (x .+. y) <= aabs x .+. aabs y
  -- @
  --
  -- @since 0.1
  aabs :: m -> m

-- | @since 0.1
instance AMonoid Double where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Float where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Int where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Int8 where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Int16 where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Int32 where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Int64 where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Integer where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Word where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Word8 where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Word16 where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Word32 where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Word64 where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid Natural where
  zero = 0
  aabs = abs

instance AMonoid (Ratio Integer) where
  zero = 0
  aabs = abs

instance AMonoid (Ratio Natural) where
  zero = 0
  aabs = abs

-- | @since 0.1
instance AMonoid a => AMonoid (a, a) where
  zero = (zero, zero)
  aabs (x1, x2) = (aabs x1, aabs x2)

-- | @since 0.1
instance AMonoid a => AMonoid (a, a, a) where
  zero = (zero, zero, zero)
  aabs (x1, x2, x3) = (aabs x1, aabs x2, aabs x3)

-- | @since 0.1
instance AMonoid a => AMonoid (a, a, a, a) where
  zero = (zero, zero, zero, zero)
  aabs (x1, x2, x3, x4) = (aabs x1, aabs x2, aabs x3, aabs x4)

-- | @since 0.1
instance AMonoid a => AMonoid (a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero)
  aabs (x1, x2, x3, x4, x5) = (aabs x1, aabs x2, aabs x3, aabs x4, aabs x5)

-- | @since 0.1
instance AMonoid a => AMonoid (a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero)
  aabs (x1, x2, x3, x4, x5, x6) =
    ( aabs x1,
      aabs x2,
      aabs x3,
      aabs x4,
      aabs x5,
      aabs x6
    )

-- | @since 0.1
instance AMonoid a => AMonoid (a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero)
  aabs (x1, x2, x3, x4, x5, x6, x7) =
    ( aabs x1,
      aabs x2,
      aabs x3,
      aabs x4,
      aabs x5,
      aabs x6,
      aabs x7
    )

-- | @since 0.1
instance AMonoid a => AMonoid (a, a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero)
  aabs (x1, x2, x3, x4, x5, x6, x7, x8) =
    ( aabs x1,
      aabs x2,
      aabs x3,
      aabs x4,
      aabs x5,
      aabs x6,
      aabs x7,
      aabs x8
    )

-- | @since 0.1
instance AMonoid a => AMonoid (a, a, a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero, zero)
  aabs (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
    ( aabs x1,
      aabs x2,
      aabs x3,
      aabs x4,
      aabs x5,
      aabs x6,
      aabs x7,
      aabs x8,
      aabs x9
    )
