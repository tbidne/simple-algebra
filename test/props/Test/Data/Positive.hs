module Test.Data.Positive (props) where

import Hedgehog (MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Data.Positive qualified as Pos
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import Test.TestBounds (TestBounds (..))

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.Positive"
    [ mkPositiveSucceeds,
      mkPositiveFails
    ]

mkPositiveSucceeds :: TestTree
mkPositiveSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "x > 0 succeeds" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll pos
        Just (Pos.reallyUnsafePositive x) === Pos.mkPositive x

mkPositiveFails :: TestTree
mkPositiveFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "x < 1 fails" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll nonpos
        Nothing === Pos.mkPositive x

pos :: MonadGen m => m Integer
pos = HG.integral $ HR.exponentialFrom 1 1 maxVal

nonpos :: MonadGen m => m Integer
nonpos = HG.integral $ HR.exponentialFrom minVal 0 0
