{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides the 'BoundedN' type for enforcing a bounds invariant.
--
-- @since 0.1.0.0
module Simple.Algebra.Data.BoundedN
  ( -- * Type
    BoundedN (MkBoundedN, unBoundedN),

    -- * Creation
    mkBoundedN,
    readBoundedN,
    unsafeBoundedN,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, natVal)
import Simple.Algebra.Data.Utils qualified as U

-- | Newtype wrapper over some /a/. The underlying /a/ is in \([l, u]\)
-- where \(l \ge 0\).
--
-- @since 0.1.0.0
type BoundedN :: Nat -> Nat -> Type -> Type
newtype BoundedN l u a = UnsafeBoundedN
  { -- | Unwraps the 'BoundedN'
    --
    -- @since 0.1.0.0
    unBoundedN :: a
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'BoundedN'.
--
-- @since 0.1.0.0
pattern MkBoundedN :: a -> BoundedN l u a
pattern MkBoundedN n <- UnsafeBoundedN n

{-# COMPLETE MkBoundedN #-}

-- | @since 0.1.0.0
instance
  (KnownNat l, KnownNat u, Num a, Ord a, Show a) =>
  Bounded (BoundedN l u a)
  where
  minBound = unsafeBoundedN $ fromIntegral $ natVal $ Proxy @l
  maxBound = unsafeBoundedN $ fromIntegral $ natVal $ Proxy @u

-- | Constructs a 'BoundedN'.
--
-- >>> mkBoundedN @0 @100 50
-- Just (UnsafeBoundedN {unBoundedN = 50})
--
-- >>> mkBoundedN @10 @20 25
-- Nothing
--
-- @since 0.1.0.0
mkBoundedN ::
  forall l u a.
  (KnownNat l, KnownNat u, Num a, Ord a) =>
  a ->
  Maybe (BoundedN l u a)
mkBoundedN = U.mkX (inBounds @l @u) UnsafeBoundedN

-- | Unsafe constructor for 'BoundedN', intended to be used with
-- known constants, e.g. @unsafeBoundedN \@0 \@100 50@. Exercise restraint!
--
-- >>> unsafeBoundedN @0 @10 5
-- UnsafeBoundedN {unBoundedN = 5}
--
-- >>> unsafeBoundedN @0 @10 15
-- Passed invalid 15 bounded by [0,10]
--
-- @since 0.1.0.0
unsafeBoundedN ::
  forall l u a.
  (KnownNat l, KnownNat u, Num a, Ord a, Show a) =>
  a ->
  BoundedN l u a
unsafeBoundedN n = U.unsafeX msg (inBounds @l @u) UnsafeBoundedN n
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

-- | Safely attempts to read a 'BoundedN'.
--
-- >>> readBoundedN @0 @100 "50"
-- Just (UnsafeBoundedN {unBoundedN = 50})
--
-- >>> readBoundedN @0 @100 "200"
-- Nothing
--
-- >>> readBoundedN "cat"
-- No instance for (KnownNat l0) arising from a use of ‘it’
--
-- @since 0.1.0.0
readBoundedN ::
  forall l u a.
  (KnownNat l, KnownNat u, Num a, Ord a, Read a) =>
  String ->
  Maybe (BoundedN l u a)
readBoundedN = U.readX (inBounds @l @u) UnsafeBoundedN

inBounds :: forall l u a. (KnownNat l, KnownNat u, Num a, Ord a) => a -> Bool
inBounds n = n >= lower' && n <= upper'
  where
    lower = natVal $ Proxy @l
    upper = natVal $ Proxy @u
    lower' = fromIntegral lower
    upper' = fromIntegral upper