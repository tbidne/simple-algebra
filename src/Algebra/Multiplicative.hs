-- | Provides the 'Multiplicative' typeclass.
--
-- @since 0.1.0.0
module Algebra.Multiplicative
  ( Multiplicative (..),
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
class Eq g => Multiplicative g where
  -- | @since 0.1.0.0
  (.*.) :: g -> g -> g

infixl 7 .*.

-- | @since 0.1.0.0
instance Multiplicative Double where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Float where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Int where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Int8 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Int16 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Int32 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Int64 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Integer where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Natural where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Word where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Word8 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Word16 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Word32 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Multiplicative Word64 where
  (.*.) = (*)

-- | @since 0.1.0.0
instance Integral a => Multiplicative (Ratio a) where
  (.*.) = (*)

-- | @since 0.1.0.0
instance (Num a, Ord a) => Multiplicative (Refined Positive a) where
  (.*.) = RExtras.unsafeLiftR2 (*)

-- | @since 0.1.0.0
instance (Num a, Ord a) => Multiplicative (Refined NonNegative a) where
  (.*.) = RExtras.unsafeLiftR2 (*)

-- | @since 0.1.0.0
instance (Num a, Ord a) => Multiplicative (Refined NonZero a) where
  (.*.) = RExtras.unsafeLiftR2 (*)

-- | @since 0.1.0.0
instance Integral a => Multiplicative (Refined Even a) where
  (.*.) = RExtras.unsafeLiftR2 (*)

-- | @since 0.1.0.0
instance Integral a => Multiplicative (Refined Odd a) where
  (.*.) = RExtras.unsafeLiftR2 (*)
