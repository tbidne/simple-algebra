-- | Provides the 'Ring' typeclass.
module Simple.Algebra.Ring
  ( Ring,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Simple.Algebra.Group (Group)
import Simple.Algebra.MultiplicativeMonoid (MultiplicativeMonoid)

-- | Defines an algebraic ring.
class (Group r, MultiplicativeMonoid r) => Ring r

instance Ring Double

instance Ring Float

instance Ring Int

instance Ring Int8

instance Ring Int16

instance Ring Int32

instance Ring Int64

instance Ring Integer

instance Ring (Ratio Int)

instance Ring (Ratio Int8)

instance Ring (Ratio Int16)

instance Ring (Ratio Int32)

instance Ring (Ratio Int64)

instance Ring (Ratio Integer)
