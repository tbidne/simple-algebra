{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Data.ModP (props) where

import Data.Functor.Identity (Identity)
import GHC.Natural (Natural)
import Gens qualified
import Hedgehog (GenBase, MonadGen, Property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Algebra.Additive.AGroup (AGroup (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..), unsafeAMonoidNonZero)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Class.Boundless (UpperBoundless)
import Numeric.Data.ModP (ModP (..), reallyUnsafeModP)
import Numeric.Data.ModP qualified as ModP
import Numeric.Data.NonZero (NonZero (..))
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import Test.TestBounds (TestBounds (..))

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.ModP"
    [ mkModPSucceed,
      mkModPFail,
      intProps,
      natProps
    ]

mkModPSucceed :: TestTree
mkModPSucceed = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "mkModP x succeeds for prime" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.natural
        case ModP.mkModP @65537 x of
          Nothing -> H.failure
          Just (MkModP x') -> x `mod` 65537 === x'

mkModPFail :: TestTree
mkModPFail = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "mkModP x fails for non-prime" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.natural
        Nothing === ModP.mkModP @65536 x

intProps :: TestTree
intProps =
  T.testGroup
    "Integer"
    [ addTotalInt,
      subTotalInt,
      multTotalInt,
      divTotalInt,
      invertInt
    ]

addTotalInt :: TestTree
addTotalInt = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(.+.) implements modular addition over Integers" $
    H.withTests limit $
      addTotal' @Integer

subTotalInt :: TestTree
subTotalInt = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(.-.) implements modular subtraction over Integers" $
    H.withTests limit $
      subTotal' @Integer

multTotalInt :: TestTree
multTotalInt = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(.*.) implements modular multiplication over Integers" $
    H.withTests limit $
      multTotal' @Integer

divTotalInt :: TestTree
divTotalInt = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(.%.) implements modular division over Integers" $
    H.withTests limit $
      divTotal' @Integer

invertInt :: TestTree
invertInt = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "1 == x * invert x over Integers" $
    H.withTests limit $
      invert' @Integer

natProps :: TestTree
natProps =
  T.testGroup
    "Natural"
    [ addTotalNat,
      subTotalNat,
      multTotalNat,
      divTotalNat,
      invertNat
    ]

addTotalNat :: TestTree
addTotalNat = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(.+.) implements modular addition over Naturals" $
    H.withTests limit $
      addTotal' @Natural

subTotalNat :: TestTree
subTotalNat = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(.-.) implements modular subtraction over Naturals" $
    H.withTests limit $
      subTotal' @Natural

multTotalNat :: TestTree
multTotalNat = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(.*.) implements modular multiplication over Naturals" $
    H.withTests limit $
      multTotal' @Natural

divTotalNat :: TestTree
divTotalNat = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(.%.) implements modular division over Naturals" $
    H.withTests limit $
      divTotal' @Natural

invertNat :: TestTree
invertNat = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "1 == x * invert x over Naturals" $
    H.withTests limit $
      invert' @Natural

addTotal' ::
  forall a.
  ( AddConstraint (ModP 65537 a) ~ ModP 65537 a,
    ASemigroup (ModP 65537 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
addTotal' = H.property $ do
  mx@(MkModP x) <- H.forAll (anyNat @a)
  my@(MkModP y) <- H.forAll anyNat
  let MkModP mz = mx .+. my
      z = (x + y) `mod` 65537
  z === mz

subTotal' ::
  forall a.
  ( SubtractConstraint (ModP 65537 a) ~ ModP 65537 a,
    AGroup (ModP 65537 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
subTotal' = H.property $ do
  mx@(MkModP x) <- H.forAll (anyNat @a)
  my@(MkModP y) <- H.forAll anyNat
  let MkModP mz = mx .-. my
      z = (65537 - y + x) `mod` 65537
  z === mz

multTotal' ::
  forall a.
  ( MultConstraint (ModP 65537 a) ~ ModP 65537 a,
    MSemigroup (ModP 65537 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
multTotal' = H.property $ do
  mx@(MkModP x) <- H.forAll (anyNat @a)
  my@(MkModP y) <- H.forAll anyNat
  let MkModP mz = mx .*. my
      z = (x * y) `mod` 65537
  z === mz

divTotal' ::
  forall a.
  ( AMonoid (ModP 65537 a),
    MultConstraint (ModP 65537 a) ~ ModP 65537 a,
    DivConstraint (ModP 65537 a) ~ NonZero (ModP 65537 a),
    MGroup (ModP 65537 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
divTotal' = H.property $ do
  mx <- H.forAll (anyNat @a)
  nzy@(MkNonZero my) <- H.forAll (genNZ @a)
  let mz = mx .%. nzy
  mx === mz .*. my

invert' ::
  forall a.
  ( AMonoid (ModP 65537 a),
    MultConstraint (ModP 65537 a) ~ ModP 65537 a,
    MGroup (ModP 65537 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
invert' = H.property $ do
  nz@(MkNonZero n) <- H.forAll (genNZ @a)
  let nInv = ModP.invert nz
  reallyUnsafeModP 1 === n .*. nInv

genNZ :: forall a m. (AMonoid (ModP 65537 a), GenBase m ~ Identity, MonadGen m, TestBounds a, UpperBoundless a) => m (NonZero (ModP 65537 a))
genNZ = do
  x <- HG.filter (\x' -> x' `mod` 65537 /= 0) $ HG.integral $ HR.exponential 2 maxVal
  let y = unsafeAMonoidNonZero $ reallyUnsafeModP @65537 x
  pure y

anyNat :: forall a m. (MonadGen m, TestBounds a, UpperBoundless a) => m (ModP 65537 a)
anyNat = reallyUnsafeModP <$> HG.integral (HR.exponentialFrom 0 0 maxVal)
