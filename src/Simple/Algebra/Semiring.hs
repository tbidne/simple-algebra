-- | Provides the 'Semiring' typeclass.
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
class (AdditiveMonoid r, MultiplicativeMonoid r) => Semiring r

instance Semiring Double

instance Semiring Float

instance Semiring Int

instance Semiring Int8

instance Semiring Int16

instance Semiring Int32

instance Semiring Int64

instance Semiring Integer

instance Semiring Natural

instance Semiring Word

instance Semiring Word8

instance Semiring Word16

instance Semiring Word32

instance Semiring Word64

instance Integral a => Semiring (Ratio a)
