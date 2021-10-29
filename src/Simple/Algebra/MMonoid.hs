-- | Provides the 'MMonoid' typeclass.
module Simple.Algebra.MMonoid
  ( MMonoid (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Simple.Algebra.MSemigroup (MSemigroup (..))

-- | Defines an algebraic monoid over a \"multiplicative\" semigroup.
class MSemigroup g => MMonoid g where
  mid :: g

instance MMonoid Double where
  mid = 1

instance MMonoid Float where
  mid = 1

instance MMonoid Int where
  mid = 1

instance MMonoid Int8 where
  mid = 1

instance MMonoid Int16 where
  mid = 1

instance MMonoid Int32 where
  mid = 1

instance MMonoid Int64 where
  mid = 1

instance MMonoid Integer where
  mid = 1

instance MMonoid Natural where
  mid = 1

instance MMonoid Word where
  mid = 1

instance MMonoid Word8 where
  mid = 1

instance MMonoid Word16 where
  mid = 1

instance MMonoid Word32 where
  mid = 1

instance MMonoid Word64 where
  mid = 1

instance Integral a => MMonoid (Ratio a) where
  mid = 1