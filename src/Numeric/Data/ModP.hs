{-# LANGUAGE CPP #-}

-- | Provides the 'ModP' type for modular arithmetic.
--
-- @since 0.1.0.0
module Numeric.Data.ModP
  ( -- * Type
    ModP (MkModP),

    -- * Creation
    mkModP,
    mkModPTH,
    unsafeModP,
    reallyUnsafeModP,

    -- * Elimination
    unModP,

    -- * Functions
    invert,

    -- * Utilities
    -- $utils

    -- ** Primality
    MaybePrime (..),
    ModPI.isPrime,

    -- ** Inverses
    Modulus (..),
    ModPI.findInverse,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Data.ModP.Internal (MaybePrime (..), Modulus (..))
import Numeric.Data.ModP.Internal qualified as ModPI
import Numeric.Data.NonZero (NonZero (..))
import System.Random (UniformRange)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Numeric.Algebra.Multiplicative.MGroup (unsafeAMonoidNonZero)

-- | Newtype wrapper that represents \( \mathbb{Z}/p\mathbb{Z} \) for prime @p@.
-- 'ModP' is a:
--
-- * 'Numeric.Algebra.Additive.ASemigroup.ASemigroup'
-- * 'Numeric.Algebra.Additive.AMonoid.AMonoid'
-- * 'Numeric.Algebra.Additive.AGroup.AGroup'
-- * 'Numeric.Algebra.Multiplicative.MSemigroup.MSemigroup'
-- * 'Numeric.Algebra.Multiplicative.MMonoid.MMonoid'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroup'
-- * 'Numeric.Algebra.Semiring.Semiring'
-- * 'Numeric.Algebra.Ring.Ring'
-- * 'Numeric.Algebra.Field.Field'
--
-- @since 0.1.0.0
type ModP :: Nat -> Type -> Type
newtype ModP p a = UnsafeModP a
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Ord
    )

-- | @since 0.1.0.0
instance Show a => Show (ModP p a) where
  -- manual so we show "MkModP" instead of "UnsafeModP"
  showsPrec i (UnsafeModP x) =
    showParen
      (i >= 11)
      (showString "MkModP " . showsPrec 11 x)

-- | Bidirectional pattern synonym for 'ModP'. Construction fails when @p@ is
-- not prime.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> MkModP @7 12
-- MkModP 5
--
-- @since 0.1.0.0
pattern MkModP ::
  forall p a.
  (HasCallStack, KnownNat p, Integral a, UniformRange a) =>
  a ->
  ModP p a
pattern MkModP x <-
  UnsafeModP x
  where
    MkModP x = unsafeModP x

{-# COMPLETE MkModP #-}

-- | Unwraps a 'ModP'.
--
-- @since 0.1.0.0
unModP :: ModP p a -> a
unModP (UnsafeModP x) = x

-- | Constructor for 'ModP'. Fails if @p@ is not prime.
--
-- ==== __Examples__
-- >>> mkModP @5 7
-- Just (MkModP 2)
--
-- >>> mkModP @10 7
-- Nothing
--
-- @since 0.1.0.0
mkModP :: forall p a. (Integral a, KnownNat p, UniformRange a) => a -> Maybe (ModP p a)
mkModP x = case ModPI.isPrime p' of
  Composite -> Nothing
  ProbablyPrime -> Just $ UnsafeModP x'
  where
    p' = fromIntegral $ natVal @p Proxy
    x' = x `mod` p'

-- | Template haskell for creating a 'ModP' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkModPTH @11 7)
-- MkModP 7
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkModPTH ::
  forall p a.
  (Integral a, KnownNat p, Lift a, UniformRange a) =>
  a ->
  Code Q (ModP p a)
#else
mkModPTH ::
  forall p a.
  (Integral a, KnownNat p, Lift a, UniformRange a) =>
  a ->
  Q (TExp (ModP p a))
#endif
mkModPTH = maybe (error err) liftTyped . mkModP
  where
    err =
      "Numeric.Data.ModP.mkModPTH: Passed non-prime: "
        <> show p'
    p' = natVal @p Proxy

-- | Variant of 'mkModP' that throws an error when given a non-prime.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeModP @7 12
-- MkModP 5
--
-- @since 0.1.0.0
unsafeModP ::
  forall p a.
  (HasCallStack, Integral a, KnownNat p, UniformRange a) =>
  a ->
  ModP p a
unsafeModP x = case mkModP x of
  Just mp -> mp
  Nothing ->
    error $
      "Numeric.Data.ModP.unsafeModP: Passed non-prime: " <> show p'
  where
    p' = natVal @p Proxy

-- | This function reduces the argument modulo @p@ but does __not__ check
-- that @p@ is prime. Note that the correct behavior of some functionality
-- (e.g. division) is reliant on primality, so this is dangerous. This is
-- intended only for when we absolutely know @p@ is prime and the check
-- is undesirable for performance reasons. Exercise extreme caution.
--
-- @since 0.1.0.0
reallyUnsafeModP :: forall p a. (KnownNat p, Integral a) => a -> ModP p a
reallyUnsafeModP = UnsafeModP . (`mod` p')
  where
    p' = fromIntegral $ natVal @p Proxy

-- | Given non-zero \(d\), returns the inverse i.e. finds \(e\) s.t.
--
-- \[
-- de \equiv 1 \pmod p.
-- \]
--
-- ==== __Examples__
--
-- >>> invert $ unsafeAMonoidNonZero $ MkModP @7 5
-- MkModP 3
--
-- >>> invert $ unsafeAMonoidNonZero $ MkModP @19 12
-- MkModP 8
--
-- @since 0.1.0.0
invert ::
  forall p a.
  (Integral a, KnownNat p) =>
  NonZero (ModP p a) ->
  ModP p a
invert (MkNonZero (UnsafeModP d)) = reallyUnsafeModP $ ModPI.findInverse d p'
  where
    p' = MkModulus $ fromIntegral $ natVal @p Proxy

-- $utils
-- This section contains functions that are used in the implementation. They
-- are not the main focus but are exported as they could be useful to
-- consumers.
