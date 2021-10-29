-- | Provides the 'AMonoid' typeclass.
module Simple.Algebra.AMonoid
  ( AMonoid (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Simple.Algebra.ASemigroup (ASemigroup (..))

-- | Defines an algebraic monoid over an \"additive\" semigroup.
class ASemigroup g => AMonoid g where
  aid :: g

instance AMonoid Double where
  aid = 0

instance AMonoid Float where
  aid = 0

instance AMonoid Int where
  aid = 0

instance AMonoid Int8 where
  aid = 0

instance AMonoid Int16 where
  aid = 0

instance AMonoid Int32 where
  aid = 0

instance AMonoid Int64 where
  aid = 0

instance AMonoid Integer where
  aid = 0

instance AMonoid Natural where
  aid = 0

instance AMonoid Word where
  aid = 0

instance AMonoid Word8 where
  aid = 0

instance AMonoid Word16 where
  aid = 0

instance AMonoid Word32 where
  aid = 0

instance AMonoid Word64 where
  aid = 0

instance Integral a => AMonoid (Ratio a) where
  aid = 0