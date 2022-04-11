-- | Provides the 'MSemigroup' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MSemigroup
  ( MSemigroup (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines a multiplicative semigroup.
--
-- @since 0.1
class Eq s => MSemigroup s where
  -- | Possible constraint on the second argument to '(.*.)' e.g. for
  -- preventing overflow.
  --
  -- @since 0.1
  type MultConstraint s

  -- | @since 0.1
  (.*.) :: s -> MultConstraint s -> s

infixl 7 .*.

-- | @since 0.1
instance MSemigroup Double where
  type MultConstraint Double = Double
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Float where
  type MultConstraint Float = Float
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Int where
  type MultConstraint Int = Int
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Int8 where
  type MultConstraint Int8 = Int8
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Int16 where
  type MultConstraint Int16 = Int16
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Int32 where
  type MultConstraint Int32 = Int32
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Int64 where
  type MultConstraint Int64 = Int64
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Integer where
  type MultConstraint Integer = Integer
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Word where
  type MultConstraint Word = Word
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Word8 where
  type MultConstraint Word8 = Word8
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Word16 where
  type MultConstraint Word16 = Word16
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Word32 where
  type MultConstraint Word32 = Word32
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Word64 where
  type MultConstraint Word64 = Word64
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Natural where
  type MultConstraint Natural = Natural
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup (Ratio Integer) where
  type MultConstraint (Ratio Integer) = Ratio Integer
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup (Ratio Natural) where
  type MultConstraint (Ratio Natural) = Ratio Natural
  (.*.) = (*)
