module Test.Algebra.Normed (props) where

import Equality (Equality (MkEqEpsilon, MkEqExact, MkEqRatio))
import Gens qualified
import Hedgehog (Gen, PropertyName, (===))
import Hedgehog qualified as H
import Numeric.Algebra.Additive (AMonoid (zero), ASemigroup ((.+.)))
import Numeric.Algebra.Multiplicative (MGroup ((.%.)))
import Numeric.Algebra.Normed (Normed (norm, sgn))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils ((<=>))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Normed"
    [ normProps,
      sgnProps
    ]

-- No bounded props since e.g. abs (-128 :: Int8) == -128
normProps :: TestTree
normProps =
  T.testGroup
    "Norm props"
    [ floatNorm,
      doubleNorm,
      intNorm,
      integerNorm,
      wordNorm,
      naturalNorm,
      rationalNorm,
      rationalNatNorm
    ]

floatNorm :: TestTree
floatNorm = testNormProps Gens.float (MkEqEpsilon 1.0) "Float" "floatNorm"

doubleNorm :: TestTree
doubleNorm = testNormProps Gens.double (MkEqEpsilon 1.0) "Double" "doubleNorm"

intNorm :: TestTree
intNorm = testNormProps Gens.int MkEqExact "Int" "intNorm"

integerNorm :: TestTree
integerNorm = testNormProps Gens.integer MkEqExact "Integer" "integerNorm"

wordNorm :: TestTree
wordNorm = testNormProps Gens.word MkEqExact "Word" "wordNorm"

naturalNorm :: TestTree
naturalNorm = testNormProps Gens.natural MkEqExact "Natural" "naturalNorm"

rationalNorm :: TestTree
rationalNorm = testNormProps Gens.rational MkEqRatio "Rational" "rationalNorm"

rationalNatNorm :: TestTree
rationalNatNorm = testNormProps Gens.rationalNat MkEqRatio "Rational Nat" "rationalNatNorm"

testNormProps ::
  (AMonoid a, Normed a, Show a, Ord a) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
testNormProps gen eqCons desc propName =
  Utils.testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      y <- H.forAll gen

      -- idempotence: |x| = ||x||
      let eqX = eqCons x
          eqAbs = eqCons (norm x)
      eqAbs === eqCons (norm (norm x))

      -- non-negative: |x| >= 0
      let eqZero = eqCons zero
      H.diff eqAbs (>=) eqZero

      -- positive-definite: |x| == 0 <=> x == 0
      H.diff (eqAbs == eqZero) (<=>) (eqX == eqZero)

      -- triangle equality: |x + y| <= |x| + |y|
      let sumAbs = eqCons $ norm x .+. norm y
          absSum = eqCons $ norm (x .+. y)
      H.diff absSum (<=) sumAbs

-- In addition to bounded problems, floating point also has Infinity.
sgnProps :: TestTree
sgnProps =
  T.testGroup
    "Sgn props"
    [ integerSgn,
      naturalSgn
    ]

integerSgn :: TestTree
integerSgn = testSgnProps Gens.integerNZ MkEqExact "Integer" "integerSgn"

naturalSgn :: TestTree
naturalSgn = testSgnProps Gens.naturalNZ MkEqExact "Natural" "naturalSgn"

testSgnProps ::
  (MGroup a, Normed a, Show a) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
testSgnProps gen eqCons desc propName =
  Utils.testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen

      let s = sgn x
          n = norm x
          r = x .%. n

      H.annotateShow n

      eqCons s === eqCons r
