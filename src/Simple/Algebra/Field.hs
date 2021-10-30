-- | Provides the 'Field' typeclass.
module Simple.Algebra.Field
  ( Field (..),
    NonZero (MkNonZero, unNonZero),
    AM.mkMonoidNonZero,
    AM.unsafeMonoidNonZero,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Simple.Algebra.AdditiveMonoid (NonZero (..))
import Simple.Algebra.AdditiveMonoid qualified as AM
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

instance Field (Ratio Int) where x .%. MkNonZero d = x / d

instance Field (Ratio Int8) where x .%. MkNonZero d = x / d

instance Field (Ratio Int16) where x .%. MkNonZero d = x / d

instance Field (Ratio Int32) where x .%. MkNonZero d = x / d

instance Field (Ratio Int64) where x .%. MkNonZero d = x / d

instance Field (Ratio Integer) where x .%. MkNonZero d = x / d
