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
  -- | @since 0.1
  (.*.) :: s -> s -> s

infixl 7 .*.

-- | @since 0.1
instance MSemigroup Double where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Float where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Int where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Int8 where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Int16 where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Int32 where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Int64 where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Integer where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Word where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Word8 where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Word16 where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Word32 where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Word64 where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup Natural where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup (Ratio Integer) where
  (.*.) = (*)

-- | @since 0.1
instance MSemigroup (Ratio Natural) where
  (.*.) = (*)
