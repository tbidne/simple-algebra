module Test.Algebra.MetricSpace (props) where

import Control.Monad (when)
import Equality (Equality (MkEqEpsilon))
import Gens qualified
import Hedgehog (Gen, PropertyName, annotateShow, (===))
import Hedgehog qualified as H
import Hedgehog.Internal.Property (diff)
import Numeric.Algebra.MetricSpace (MetricSpace (diffR))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "MetricSpace"
    [ diffProps
    ]

diffProps :: TestTree
diffProps =
  T.testGroup
    "Diff props"
    [ floatDiff,
      doubleDiff,
      intDiff,
      int8Diff,
      int16Diff,
      int32Diff,
      int64Diff,
      integerDiff,
      wordDiff,
      word8Diff,
      word16Diff,
      word32Diff,
      word64Diff,
      naturalDiff,
      rationalDiff,
      rationalNatDiff
    ]

floatDiff :: TestTree
floatDiff = testDiffProps Gens.float "Float" "floatDiff"

doubleDiff :: TestTree
doubleDiff = testDiffProps Gens.double "Double" "doubleDiff"

intDiff :: TestTree
intDiff = testDiffProps Gens.int "Int" "intDiff"

int8Diff :: TestTree
int8Diff = testDiffProps Gens.int8 "Int8" "int8Diff"

int16Diff :: TestTree
int16Diff = testDiffProps Gens.int16 "Int16" "int16Diff"

int32Diff :: TestTree
int32Diff = testDiffProps Gens.int32 "Int32" "int32Diff"

int64Diff :: TestTree
int64Diff = testDiffProps Gens.int64 "Int64" "int64Diff"

integerDiff :: TestTree
integerDiff = testDiffProps Gens.integer "Integer" "integerDiff"

wordDiff :: TestTree
wordDiff = testDiffProps Gens.word "Word" "wordDiff"

word8Diff :: TestTree
word8Diff = testDiffProps Gens.word8 "Word8" "word8Diff"

word16Diff :: TestTree
word16Diff = testDiffProps Gens.word16 "Word16" "word16Diff"

word32Diff :: TestTree
word32Diff = testDiffProps Gens.word32 "Word32" "word32Diff"

word64Diff :: TestTree
word64Diff = testDiffProps Gens.word64 "Word64" "word64Diff"

naturalDiff :: TestTree
naturalDiff = testDiffProps Gens.natural "Natural" "naturalDiff"

rationalDiff :: TestTree
rationalDiff = testDiffProps Gens.rational "Rational" "rationalDiff"

rationalNatDiff :: TestTree
rationalNatDiff = testDiffProps Gens.rationalNat "Rational Nat" "rationalNatDiff"

testDiffProps ::
  (Eq a, MetricSpace a, Show a) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
testDiffProps gen desc propName =
  Utils.testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      y <- H.forAll gen
      z <- H.forAll gen

      let xy = x `diffR` y
          yx = y `diffR` x

      annotateShow xy

      -- positivity
      if x == y
        then
          MkEqEpsilon 1.0 xy === MkEqEpsilon 1.0 0
        else
          diff xy (epsCompare 1.0 (>)) 0

      -- symmetry
      MkEqEpsilon 1.0 xy === MkEqEpsilon 1.0 yx

      -- triangle
      let xz = x `diffR` z
          yz = y `diffR` z

      annotateShow xz
      annotateShow yz

      -- Crude, but when xy gets really large, floating point equality strikes
      -- again. A more robust method would be to base the epsilon on the
      -- size, but this is simpler.
      when (xz < 1_000_000_000) $
        diff xz (epsCompare 1.0 (<)) (xy + yz)
  where
    epsCompare eps f a b = MkEqEpsilon eps a == MkEqEpsilon eps b || a `f` b
