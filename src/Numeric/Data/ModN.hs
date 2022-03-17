{-# LANGUAGE DeriveAnyClass #-}

-- | Provides the 'ModN' type for modular arithmetic.
--
-- @since 0.1.0.0
module Numeric.Data.ModN
  ( -- * Type
    ModN (MkModN),

    -- * Creation
    mkModN,

    -- * Elimination
    unModN,
  )
where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> :set -XTemplateHaskell

-- | Newtype wrapper that represents \( \mathbb{Z}/n\mathbb{Z} \).
-- 'ModN' is a:
--
-- * 'Numeric.Algebra.Additive.ASemigroup.ASemigroup'
-- * 'Numeric.Algebra.Additive.AMonoid.AMonoid'
-- * 'Numeric.Algebra.Additive.AGroup.AGroup'
-- * 'Numeric.Algebra.Multiplicative.MSemigroup.MSemigroup'
-- * 'Numeric.Algebra.Multiplicative.MMonoid.MMonoid'
-- * 'Numeric.Algebra.Semiring.Semiring'
-- * 'Numeric.Algebra.Ring.Ring'
--
-- @since 0.1.0.0
type ModN :: Nat -> Type -> Type
newtype ModN n a = UnsafeModN a
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | Bidirectional pattern synonym for 'ModN'. Construction will apply
-- modular reduction to the parameter.
--
-- @since 0.1.0.0
pattern MkModN :: forall n a. (KnownNat n, Integral a) => a -> ModN n a
pattern MkModN x <-
  UnsafeModN x
  where
    MkModN x = mkModN x

{-# COMPLETE MkModN #-}

-- | Unwraps a 'ModN'.
--
-- @since 0.1.0.0
unModN :: ModN n a -> a
unModN (UnsafeModN x) = x

-- | Constructor for 'ModN'.
--
-- ==== __Examples__
-- >>> mkModN @5 7
-- UnsafeModN 2
--
-- >>> mkModN @10 7
-- UnsafeModN 7
--
-- @since 0.1.0.0
mkModN :: forall n a. (Integral a, KnownNat n) => a -> ModN n a
mkModN x = UnsafeModN x'
  where
    n' = fromIntegral $ natVal @n Proxy
    x' = x `mod` n'
