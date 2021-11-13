-- | Provides the 'Semiring' typeclass.
--
-- @since 0.1.0.0
module Simple.Algebra.Semiring
  ( Semiring,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import Simple.Algebra.AdditiveMonoid (AdditiveMonoid (..))
import Simple.Algebra.MultiplicativeMonoid (MultiplicativeMonoid (..))

-- | Defines a semiring.
--
-- @since 0.1.0.0
class (AdditiveMonoid r, MultiplicativeMonoid r) => Semiring r

-- | @since 0.1.0.0
instance Semiring Double

-- | @since 0.1.0.0
instance Semiring Float

-- | @since 0.1.0.0
instance Semiring Int

-- | @since 0.1.0.0
instance Semiring Int8

-- | @since 0.1.0.0
instance Semiring Int16

-- | @since 0.1.0.0
instance Semiring Int32

-- | @since 0.1.0.0
instance Semiring Int64

-- | @since 0.1.0.0
instance Semiring Integer

-- | @since 0.1.0.0
instance Semiring Natural

-- | @since 0.1.0.0
instance Semiring Word

-- | @since 0.1.0.0
instance Semiring Word8

-- | @since 0.1.0.0
instance Semiring Word16

-- | @since 0.1.0.0
instance Semiring Word32

-- | @since 0.1.0.0
instance Semiring Word64

-- | @since 0.1.0.0
instance Integral a => Semiring (Ratio a)
