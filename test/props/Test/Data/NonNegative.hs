module Test.Data.NonNegative (props) where

import Hedgehog (MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Data.NonNegative qualified as NonNeg
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import Test.TestBounds (TestBounds (..))

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.NonNegative"
    [ mkNonNegativeSucceeds,
      mkNonNegativeFails
    ]

mkNonNegativeSucceeds :: TestTree
mkNonNegativeSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "x >= 0 succeeds" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll nonneg
        Just (NonNeg.reallyUnsafeNonNegative x) === NonNeg.mkNonNegative x

mkNonNegativeFails :: TestTree
mkNonNegativeFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "x < 0 fails" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll neg
        Nothing === NonNeg.mkNonNegative x

nonneg :: MonadGen m => m Integer
nonneg = HG.integral $ HR.exponentialFrom 0 0 maxVal

neg :: MonadGen m => m Integer
neg = HG.integral $ HR.exponentialFrom minVal -1 -1
