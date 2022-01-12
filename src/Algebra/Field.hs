-- | Provides the 'Field' typeclass.
--
-- @since 0.1.0.0
module Algebra.Field
  ( Field,
  )
where

import Algebra.Multiplicative.MGroup (MGroup (..))
import Algebra.Ring (Ring)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)

-- | Defines a field.
--
-- @since 0.1.0.0
class (Ring f, MGroup f) => Field f

instance Field Float

instance Field Double

instance Field Int

instance Field Int8

instance Field Int16

instance Field Int32

instance Field Int64

instance Field Integer

instance Field (Ratio Int)

instance Field (Ratio Int8)

instance Field (Ratio Int16)

instance Field (Ratio Int32)

instance Field (Ratio Int64)

instance Field (Ratio Integer)
