-- | Provides the 'Ring' typeclass.
module Simple.Algebra.Ring
  ( Ring,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Simple.Algebra.Group (Group)
import Simple.Algebra.MMonoid (MMonoid)

-- | Defines an algebraic ring.
class (Group r, MMonoid r) => Ring r

instance Ring Double

instance Ring Float

instance Ring Int

instance Ring Int8

instance Ring Int16

instance Ring Int32

instance Ring Int64

instance Ring Integer

instance Ring Natural

instance Ring Word

instance Ring Word8

instance Ring Word16

instance Ring Word32

instance Ring Word64

instance Integral a => Ring (Ratio a)
