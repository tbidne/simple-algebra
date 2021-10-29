-- | Provides the 'ASemigroup' typeclass.
module Simple.Algebra.ASemigroup
  ( ASemigroup (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Smart.Data.Math.NonNegative (NonNegative (..), unsafeNonNegative)
import Smart.Data.Math.Positive (Positive (..), unsafePositive)

-- | Defines an algebraic semigroup. The \"A\" prefix refers to \"Additive\",
-- as opposed to \"Multiplicative\". This is certainly clunky from a formal
-- perspective, but having separate typeclasses allows us to recover the
-- usual mathematical operators we want (addition, multiplication) without
-- an explosion of newtype wrappers.
class Eq g => ASemigroup g where
  (.+.) :: g -> g -> g

infixl 6 .+.

instance ASemigroup Double where
  (.+.) = (+)

instance ASemigroup Float where
  (.+.) = (+)

instance ASemigroup Int where
  (.+.) = (+)

instance ASemigroup Int8 where
  (.+.) = (+)

instance ASemigroup Int16 where
  (.+.) = (+)

instance ASemigroup Int32 where
  (.+.) = (+)

instance ASemigroup Int64 where
  (.+.) = (+)

instance ASemigroup Integer where
  (.+.) = (+)

instance ASemigroup Natural where
  (.+.) = (+)

instance ASemigroup Word where
  (.+.) = (+)

instance ASemigroup Word8 where
  (.+.) = (+)

instance ASemigroup Word16 where
  (.+.) = (+)

instance ASemigroup Word32 where
  (.+.) = (+)

instance ASemigroup Word64 where
  (.+.) = (+)

instance Integral a => ASemigroup (Ratio a) where
  (.+.) = (+)

instance (Eq a, Num a, Ord a) => ASemigroup (NonNegative a) where
  MkNonNegative x .+. MkNonNegative y = unsafeNonNegative $ x + y

instance (Eq a, Num a, Ord a) => ASemigroup (Positive a) where
  MkPositive x .+. MkPositive y = unsafePositive $ x + y
