module Test.Data.NonZero (props) where

import Hedgehog (MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Data.NonZero (NonZero (..))
import Numeric.Data.NonZero qualified as NonZero
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import Test.TestBounds (TestBounds (..))

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.NonZero"
    [ mkNonZeroSucceeds,
      mkNonZeroFails,
      multTotal,
      divTotal
    ]

mkNonZeroSucceeds :: TestTree
mkNonZeroSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "x /= 0 succeeds" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll nonzero
        Just (NonZero.reallyUnsafeNonZero x) === NonZero.mkNonZero x

mkNonZeroFails :: TestTree
mkNonZeroFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "x == 0 fails" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll zero
        Nothing === NonZero.mkNonZero x

multTotal :: TestTree
multTotal = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(.*.) is total" $
    H.withTests limit $
      H.property $ do
        px@(MkNonZero x) <- H.forAll nonzeroCons
        py@(MkNonZero y) <- H.forAll nonzeroCons
        let MkNonZero pz = px .*. py
        x * y === pz

divTotal :: TestTree
divTotal = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(.%.) is total" $
    H.withTests limit $
      H.property $ do
        px@(MkNonZero x) <- H.forAll nonzeroCons
        py@(MkNonZero y) <- H.forAll nonzeroCons
        let MkNonZero pz = px .%. py
        x `div` y === pz

nonzero :: MonadGen m => m Int
nonzero =
  HG.choice
    [ HG.integral $ HR.exponentialFrom minVal 1 1,
      HG.integral $ HR.exponentialFrom 1 1 maxVal
    ]

zero :: MonadGen m => m Integer
zero = pure 0

nonzeroCons :: MonadGen m => m (NonZero Int)
nonzeroCons = NonZero.unsafeNonZero <$> nonzero
