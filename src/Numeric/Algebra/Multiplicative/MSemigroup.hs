-- | Provides the 'MSemigroup' typeclass.
--
-- @since 0.1.0.0
module Numeric.Algebra.Multiplicative.MSemigroup
  ( MSemigroup (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Refined (Even, NonNegative, NonZero, Odd, Positive, Refined)
import Refined.Extras qualified as RExtras

-- | Defines a multiplicative semigroup.
--
-- @since 0.1.0.0
class Eq s => MSemigroup s where
  -- | @since 0.1.0.0
  (.*.) :: s -> s -> s

infixl 7 .*.

-- | @since 0.1.0.0
instance MSemigroup Double where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Float where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Int where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Int8 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Int16 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Int32 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Int64 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Integer where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Natural where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Word where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Word8 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Word16 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Word32 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup Word64 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup (Ratio Integer) where
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup (Ratio Natural) where
  (.*.) = (*)

-- | @since 0.1.0.0
instance (MSemigroup a, Num a, Ord a) => MSemigroup (Refined Positive a) where
  (.*.) = RExtras.unsafeLiftR2 (.*.)

-- | @since 0.1.0.0
instance (MSemigroup a, Num a, Ord a) => MSemigroup (Refined NonNegative a) where
  (.*.) = RExtras.unsafeLiftR2 (.*.)

-- | @since 0.1.0.0
instance (MSemigroup a, Num a) => MSemigroup (Refined NonZero a) where
  (.*.) = RExtras.unsafeLiftR2 (.*.)

-- | @since 0.1.0.0
instance (MSemigroup a, Integral a) => MSemigroup (Refined Even a) where
  (.*.) = RExtras.unsafeLiftR2 (.*.)

-- | @since 0.1.0.0
instance (Integral a, MSemigroup a) => MSemigroup (Refined Odd a) where
  (.*.) = RExtras.unsafeLiftR2 (.*.)
