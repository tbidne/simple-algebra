-- | Provides the 'Semifield' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Semifield
  ( Semifield,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Additive.AMonoid (AMonoid)
import Numeric.Algebra.Multiplicative.MGroup (MGroup)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Natural (Natural)

-- | Defines a semifield i.e. a structure that is an 'AMonoid' and
-- 'MGroup'.
--
-- @since 0.1
type Semifield :: Type -> Constraint
class (AMonoid r, MGroup r, Semiring r) => Semifield r

-- | @since 0.1
instance Semifield Double

-- | @since 0.1
instance Semifield Float

-- | @since 0.1
instance Semifield Int

-- | @since 0.1
instance Semifield Int8

-- | @since 0.1
instance Semifield Int16

-- | @since 0.1
instance Semifield Int32

-- | @since 0.1
instance Semifield Int64

-- | @since 0.1
instance Semifield Integer

-- | @since 0.1
instance Semifield Natural

-- | @since 0.1
instance Semifield Word

-- | @since 0.1
instance Semifield Word8

-- | @since 0.1
instance Semifield Word16

-- | @since 0.1
instance Semifield Word32

-- | @since 0.1
instance Semifield Word64

-- | @since 0.1
instance Semifield (Ratio Integer)

-- | @since 0.1
instance Semifield (Ratio Natural)

-- | @since 0.1
instance (RealFloat a) => Semifield (Complex a)

-- | @since 0.1
instance (HasResolution k) => Semifield (Fixed k)
