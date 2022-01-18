-- | Provides the 'Ring' typeclass.
--
-- @since 0.1.0.0
module Algebra.Ring
  ( Ring,
  )
where

import Algebra.Additive.AGroup (AGroup)
import Algebra.Multiplicative.MMonoid (MMonoid)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)

-- | Defines a ring.
--
-- @since 0.1.0.0
class (AGroup r, MMonoid r) => Ring r

-- | @since 0.1.0.0
instance Ring Double

-- | @since 0.1.0.0
instance Ring Float

-- | @since 0.1.0.0
instance Ring Int

-- | @since 0.1.0.0
instance Ring Int8

-- | @since 0.1.0.0
instance Ring Int16

-- | @since 0.1.0.0
instance Ring Int32

-- | @since 0.1.0.0
instance Ring Int64

-- | @since 0.1.0.0
instance Ring Integer

-- | @since 0.1.0.0
instance Ring Word

-- | @since 0.1.0.0
instance Ring Word8

-- | @since 0.1.0.0
instance Ring Word16

-- | @since 0.1.0.0
instance Ring Word32

-- | @since 0.1.0.0
instance Ring Word64

-- | @since 0.1.0.0
instance Ring (Ratio Integer)
