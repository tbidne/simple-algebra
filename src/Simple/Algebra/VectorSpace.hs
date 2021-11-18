-- | Provides the 'VectorSpace' typeclass.
--
-- @since 0.1.0.0
module Simple.Algebra.VectorSpace
  ( VectorSpace (..),
  )
where

import Refined (Implies, NonZero, Refined)
import Simple.Algebra.Field (Field (..))
import Simple.Algebra.Module (Module (..))

-- | Defines a vector space over a field. Ideally, this class need
-- not include any functions. The only difference between a 'Module'
-- and a 'VectorSpace' is that the former is over a
-- 'Simple.Algebra.Ring' and the latter a 'Field', so we get
-- scalar \"division\" for free simply by reusing multiplicative
-- inverses. Sadly, this cannot be trusted in the real world, so we require
-- division to be defined manually (presumably using a sensible instance for
-- '(.%.)')
--
-- @since 0.1.0.0
class (Field k, Module v k) => VectorSpace v k | v -> k where
  -- | @since 0.1.0.0
  (.%) :: Implies ps NonZero => v -> Refined ps k -> v

infixl 7 .%

-- | @since 0.1.0.0
instance Field k => VectorSpace (k, k) k where
  (x1, x2) .% k = (x1 .%. k, x2 .%. k)

-- | @since 0.1.0.0
instance Field k => VectorSpace (k, k, k) k where
  (x1, x2, x3) .% k = (x1 .%. k, x2 .%. k, x3 .%. k)

-- | @since 0.1.0.0
instance Field k => VectorSpace (k, k, k, k) k where
  (x1, x2, x3, x4) .% k = (x1 .%. k, x2 .%. k, x3 .%. k, x4 .%. k)

-- | @since 0.1.0.0
instance Field k => VectorSpace (k, k, k, k, k) k where
  (x1, x2, x3, x4, x5) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k
    )

-- | @since 0.1.0.0
instance Field k => VectorSpace (k, k, k, k, k, k) k where
  (x1, x2, x3, x4, x5, x6) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k,
      x6 .%. k
    )

-- | @since 0.1.0.0
instance Field k => VectorSpace (k, k, k, k, k, k, k) k where
  (x1, x2, x3, x4, x5, x6, x7) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k,
      x6 .%. k,
      x7 .%. k
    )

-- | @since 0.1.0.0
instance Field k => VectorSpace (k, k, k, k, k, k, k, k) k where
  (x1, x2, x3, x4, x5, x6, x7, x8) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k,
      x6 .%. k,
      x7 .%. k,
      x8 .%. k
    )

-- | @since 0.1.0.0
instance Field k => VectorSpace (k, k, k, k, k, k, k, k, k) k where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9) .% k =
    ( x1 .%. k,
      x2 .%. k,
      x3 .%. k,
      x4 .%. k,
      x5 .%. k,
      x6 .%. k,
      x7 .%. k,
      x8 .%. k,
      x9 .%. k
    )
