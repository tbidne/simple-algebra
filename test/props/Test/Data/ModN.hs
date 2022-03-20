module Test.Data.ModN (props) where

import Hedgehog (MonadGen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Data.ModN (ModN (..))
import Numeric.Data.ModN qualified as ModN
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import Test.TestBounds (TestBounds (..))

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.ModN"
    [ mkModN
    ]

mkModN :: TestTree
mkModN = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "mkModN x " $
    H.withTests limit $
      H.property $ do
        x <- H.forAll nonneg
        let MkModN x' = ModN.mkModN @12 x
        H.diff x' (<) 12

nonneg :: MonadGen m => m Integer
nonneg = HG.integral $ HR.exponentialFrom 20 20 maxVal
