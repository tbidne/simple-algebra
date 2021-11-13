-- | Provides the 'Additive' typeclass.
--
-- @since 0.1.0.0
module Simple.Algebra.Additive
  ( Additive (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines an additive semigroup.
--
-- @since 0.1.0.0
class Eq g => Additive g where
  -- | @since 0.1.0.0
  (.+.) :: g -> g -> g

infixl 6 .+.

-- | @since 0.1.0.0
instance Additive Double where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Float where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Int where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Int8 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Int16 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Int32 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Int64 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Integer where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Natural where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Word where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Word8 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Word16 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Word32 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Additive Word64 where
  (.+.) = (+)

-- | @since 0.1.0.0
instance Integral a => Additive (Ratio a) where
  (.+.) = (+)
