-- | Provides the 'VectorSpace' typeclass.
module Simple.Algebra.VectorSpace
  ( VectorSpace (..),
    NonZero (MkNonZero, unNonZero),
    F.mkNonZeroA,
    F.unsafeNonZeroA,
  )
where

import Simple.Algebra.Field (Field (..), NonZero (..))
import Simple.Algebra.Field qualified as F
import Simple.Algebra.Module (Module (..))

-- | Defines an algebraic vector space over a field. Ideally, this class need
-- not include any functions. The only difference between a 'Module'
-- and a 'VectorSpace' is that the former is over a
-- 'Simple.Algebra.Ring' and the latter a 'Field', so we get
-- scalar \"division\" for free simply by reusing multiplicative
-- inverses. Sadly, this cannot be trusted in the real world, so we require
-- division to be defined manually (presumably using a sensible instance for
-- '(.%.)')
class (Field k, Module v k) => VectorSpace v k | v -> k where
  (.%) :: v -> NonZero k -> v

infixl 7 .%
