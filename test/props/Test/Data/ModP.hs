module Test.Data.ModP (props) where

import Data.Functor.Identity (Identity)
import GHC.Natural (Natural)
import Hedgehog (GenBase, MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Algebra.Multiplicative.MGroup (unsafeAMonoidNonZero)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
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
    [ invert
    ]

invert :: TestTree
invert = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "1 == x * invert x" $
    H.withTests limit $
      H.property $ do
        nz@(MkNonZero n) <- H.forAll genNZ
        let nInv = ModP.invert nz
        MkModP 1 === n .*. nInv

genNZ :: (GenBase m ~ Identity, MonadGen m) => m (NonZero (ModP 65537 Natural))
genNZ = do
  x <- HG.filter (\x' -> x' `mod` 65537 /= 0) $ HG.integral $ HR.exponential 2 maxVal
  let y = unsafeAMonoidNonZero $ reallyUnsafeModP @65537 x
  pure y
