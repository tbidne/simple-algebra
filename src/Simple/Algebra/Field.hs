-- | Provides the 'Field' typeclass.
module Simple.Algebra.Field
  ( Field (..),
    NonZero (MkNonZero, unNonZero),
    AM.mkNonZeroA,
    AM.unsafeNonZeroA,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Simple.Algebra.AMonoid (NonZero (..))
import Simple.Algebra.AMonoid qualified as AM
import Simple.Algebra.Ring (Ring)

-- | Defines an algebraic field.
class Ring f => Field f where
  (.%.) :: f -> NonZero f -> f

infixl 7 .%.

instance Field Double where x .%. MkNonZero d = x / d

instance Field Float where x .%. MkNonZero d = x / d

instance Field Int where x .%. MkNonZero d = x `div` d

instance Field Int8 where x .%. MkNonZero d = x `div` d

instance Field Int16 where x .%. MkNonZero d = x `div` d

instance Field Int32 where x .%. MkNonZero d = x `div` d

instance Field Int64 where x .%. MkNonZero d = x `div` d

instance Field Integer where x .%. MkNonZero d = x `div` d

instance Field Natural where x .%. MkNonZero d = x `div` d

instance Field Word where x .%. MkNonZero d = x `div` d

instance Field Word8 where x .%. MkNonZero d = x `div` d

instance Field Word16 where x .%. MkNonZero d = x `div` d

instance Field Word32 where x .%. MkNonZero d = x `div` d

instance Field Word64 where x .%. MkNonZero d = x `div` d

instance Integral a => Field (Ratio a) where x .%. MkNonZero d = x / d
