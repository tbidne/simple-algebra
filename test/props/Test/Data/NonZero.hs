module Test.Data.NonZero (props) where

import Data.Functor.Identity (Identity)
import Hedgehog (GenBase, MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Data.NonZero qualified as Pos
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import Test.TestBounds (TestBounds (..))

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.NonZero"
    [ mkNonZeroSucceeds,
      mkNonZeroFails
    ]

mkNonZeroSucceeds :: TestTree
mkNonZeroSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "x /= 0 succeeds" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll nonzero
        Just (Pos.reallyUnsafeNonZero x) === Pos.mkNonZero x

mkNonZeroFails :: TestTree
mkNonZeroFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "x == 0 fails" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll zero
        Nothing === Pos.mkNonZero x

nonzero :: (GenBase m ~ Identity, MonadGen m) => m Integer
nonzero = HG.filter (/= 0) $ HG.integral $ HR.exponentialFrom minVal 0 maxVal

zero :: MonadGen m => m Integer
zero = pure 0
