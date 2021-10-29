-- | Provides the 'MSemigroup' typeclass.
module Simple.Algebra.MSemigroup
  ( MSemigroup (..)
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines an algebraic semigroup. The \"M\" prefix refers to
-- \"Multiplicative\", as opposed to \"Additive\". This is certainly clunky
-- from a formal perspective, but having separate typeclasses allows us to
-- recover the usual mathematical operators we want (addition, multiplicative)
-- without an explosion of newtype wrappers.
class Eq g => MSemigroup g where
  (.*.) :: g -> g -> g

infixl 7 .*.

instance MSemigroup Double where
  (.*.) = (*)

instance MSemigroup Float where
  (.*.) = (*)

instance MSemigroup Int where
  (.*.) = (*)

instance MSemigroup Int8 where
  (.*.) = (*)

instance MSemigroup Int16 where
  (.*.) = (*)

instance MSemigroup Int32 where
  (.*.) = (*)

instance MSemigroup Int64 where
  (.*.) = (*)

instance MSemigroup Integer where
  (.*.) = (*)

instance MSemigroup Natural where
  (.*.) = (*)

instance MSemigroup Word where
  (.*.) = (*)

instance MSemigroup Word8 where
  (.*.) = (*)

instance MSemigroup Word16 where
  (.*.) = (*)

instance MSemigroup Word32 where
  (.*.) = (*)

instance MSemigroup Word64 where
  (.*.) = (*)

instance Integral a => MSemigroup (Ratio a) where
  (.*.) = (*)