{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides the 'BoundedNat' type for enforcing a bounds invariant.
module Simple.Algebra.Data.BoundedNat
  ( -- * Type
    BoundedNat (MkBoundedNat, unBoundedNat),

    -- * Creation
    mkBoundedNat,
    readBoundedNat,
    unsafeBoundedNat,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Simple.Algebra.Data.Utils qualified as U

-- | Newtype wrapper over 'Natural'. The underlying 'Natural' is in \([l, u]\).
type BoundedNat :: Nat -> Nat -> Type
newtype BoundedNat l u = UnsafeBoundedNat
  { -- | Unwraps the 'BoundedNat'
    unBoundedNat :: Natural
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'BoundedNat'.
pattern MkBoundedNat :: Natural -> BoundedNat l u
pattern MkBoundedNat n <- UnsafeBoundedNat n

{-# COMPLETE MkBoundedNat #-}

instance (KnownNat l, KnownNat u) => Bounded (BoundedNat l u) where
  minBound = unsafeBoundedNat $ natVal $ Proxy @l
  maxBound = unsafeBoundedNat $ natVal $ Proxy @u

-- | Contructs a 'BoundedNat'.
--
-- >>> mkBoundedNat @0 @100 50
-- Just (UnsafeBoundedNat {unBoundedNat = 50})
--
-- >>> mkBoundedNat @10 @20 25
-- Nothing
mkBoundedNat :: forall l u. (KnownNat l, KnownNat u) => Natural -> Maybe (BoundedNat l u)
mkBoundedNat = U.mkX (inBounds @l @u) UnsafeBoundedNat

-- | Unsafe constructor for 'BoundedNat', intended to be used with
-- known constants, e.g. @unsafeBoundedNat \@0 \@100 50@. Exercise restraint!
--
-- >>> unsafeBoundedNat @0 @10 5
-- UnsafeBoundedNat {unBoundedNat = 5}
--
-- >>> unsafeBoundedNat @0 @10 15
-- Passed invalid 15 bounded by [0,10]
unsafeBoundedNat :: forall l u. (KnownNat l, KnownNat u) => Natural -> BoundedNat l u
unsafeBoundedNat n = U.unsafeX msg (inBounds @l @u) UnsafeBoundedNat n
  where
    lower = natVal $ Proxy @l
    upper = natVal $ Proxy @u
    msg =
      concat
        [ "Passed invalid ",
          show n,
          " bounded by [",
          show lower,
          ",",
          show upper,
          "]"
        ]

-- | Safely attempts to read a 'BoundedNat'.
--
-- >>> readBoundedNat @0 @100 "50"
-- Just (UnsafeBoundedNat {unBoundedNat = 50})
--
-- >>> readBoundedNat @0 @100 "200"
-- Nothing
--
-- >>> readBoundedNat "cat"
-- No instance for (KnownNat l0) arising from a use of ‘it’
readBoundedNat :: forall l u. (KnownNat l, KnownNat u) => String -> Maybe (BoundedNat l u)
readBoundedNat = U.readX (inBounds @l @u) UnsafeBoundedNat

inBounds :: forall l u. (KnownNat l, KnownNat u) => Natural -> Bool
inBounds n = n >= lower && n <= upper
  where
    lower = natVal $ Proxy @l
    upper = natVal $ Proxy @u
