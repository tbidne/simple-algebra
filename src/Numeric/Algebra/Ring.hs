-- | Provides the 'Ring' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Ring
  ( Ring,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Additive.AGroup (AGroup)
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid)
import Numeric.Algebra.Semiring (Semiring)

-- | Defines a ring i.e. a structure that is an 'AGroup' and 'MMonoid'.
--
-- @since 0.1
type Ring :: Type -> Constraint
class (AGroup r, MMonoid r, Semiring r) => Ring r

-- | @since 0.1
instance Ring Double

-- | @since 0.1
instance Ring Float

-- | @since 0.1
instance Ring Int

-- | @since 0.1
instance Ring Int8

-- | @since 0.1
instance Ring Int16

-- | @since 0.1
instance Ring Int32

-- | @since 0.1
instance Ring Int64

-- | @since 0.1
instance Ring Integer

-- | @since 0.1
instance Ring Word

-- | @since 0.1
instance Ring Word8

-- | @since 0.1
instance Ring Word16

-- | @since 0.1
instance Ring Word32

-- | @since 0.1
instance Ring Word64

-- | @since 0.1
instance Ring (Ratio Integer)

-- | @since 0.1
instance (RealFloat a) => Ring (Complex a)

-- | @since 0.1
instance (HasResolution k) => Ring (Fixed k)
