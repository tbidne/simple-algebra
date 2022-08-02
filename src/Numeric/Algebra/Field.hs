-- | Provides the 'Field' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Field
  ( Field,
  )
where

import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Semifield (Semifield)

-- | Defines a field.
--
-- @since 0.1
type Field :: Type -> Constraint
class (Ring f, Semifield f) => Field f

-- | @since 0.1
instance Field Float

-- | @since 0.1
instance Field Double

-- | @since 0.1
instance Field Int

-- | @since 0.1
instance Field Int8

-- | @since 0.1
instance Field Int16

-- | @since 0.1
instance Field Int32

-- | @since 0.1
instance Field Int64

-- | @since 0.1
instance Field Integer

-- | @since 0.1
instance Field Word

-- | @since 0.1
instance Field Word8

-- | @since 0.1
instance Field Word16

-- | @since 0.1
instance Field Word32

-- | @since 0.1
instance Field Word64

-- | @since 0.1
instance Field (Ratio Integer)

-- | @since 0.1
instance RealFloat a => Field (Complex a)
