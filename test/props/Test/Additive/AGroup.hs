module Test.Additive.AGroup (props) where

import Algebra.Additive.AGroup (AGroup (..))
import Algebra.Additive.AMonoid (AMonoid (..))
import Algebra.Additive.ASemigroup (ASemigroup (..))
import Gens qualified
import Hedgehog (Gen, (/==), (===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import Utils ((<=>))
import Utils qualified
import Equality (Equality (..))

props :: TestTree
props =
  T.testGroup
    "Additive Group"
    [ subProps,
      subIdentProps,
      invProps,
      absProps
    ]

subProps :: TestTree
subProps =
  T.testGroup
    "(.-.) === (-)"
    [ floatSub,
      doubleSub,
      intSub,
      int8Sub,
      int16Sub,
      int32Sub,
      int64Sub,
      integerSub,
      ratioIntegerSub
    ]

floatSub :: TestTree
floatSub = agroupSubEq Gens.float (MkEqEpsilon 1.0) "Float"

doubleSub :: TestTree
doubleSub = agroupSubEq Gens.double (MkEqEpsilon 1.0) "Double"

intSub :: TestTree
intSub = agroupSubEq Gens.int MkEqExact "Int"

int8Sub :: TestTree
int8Sub = agroupSubEq Gens.int8 MkEqExact "Int8"

int16Sub :: TestTree
int16Sub = agroupSubEq Gens.int16 MkEqExact "Int16"

int32Sub :: TestTree
int32Sub = agroupSubEq Gens.int32 MkEqExact "Int32"

int64Sub :: TestTree
int64Sub = agroupSubEq Gens.int64 MkEqExact "Int64"

integerSub :: TestTree
integerSub = agroupSubEq Gens.integer MkEqExact "Integer"

ratioIntegerSub :: TestTree
ratioIntegerSub = agroupSubEq Gens.rational MkEqExact "Rational"

subIdentProps :: TestTree
subIdentProps =
  T.testGroup
    "Subtraction is the inverse: zero == x .-. x"
    [ intSubIdent,
      int8SubIdent,
      int16SubIdent,
      int32SubIdent,
      int64SubIdent,
      integerSubIdent,
      rationalSubIdent
    ]

intSubIdent :: TestTree
intSubIdent = agroupSubIdent Gens.int "Int"

int8SubIdent :: TestTree
int8SubIdent = agroupSubIdent Gens.int8 "Int8"

int16SubIdent :: TestTree
int16SubIdent = agroupSubIdent Gens.int16 "Int16"

int32SubIdent :: TestTree
int32SubIdent = agroupSubIdent Gens.int32 "Int32"

int64SubIdent :: TestTree
int64SubIdent = agroupSubIdent Gens.int64 "Int64"

integerSubIdent :: TestTree
integerSubIdent = agroupSubIdent Gens.integer "Integer"

rationalSubIdent :: TestTree
rationalSubIdent = agroupSubIdent Gens.rational "Rational"

invProps :: TestTree
invProps =
  T.testGroup
    "x .+. inv x == zero == inv x .+. x"
    [ intInv,
      int8Inv,
      int16Inv,
      int32Inv,
      int64Inv,
      integerInv,
      rationalInv
    ]

intInv :: TestTree
intInv = agroupInv Gens.int MkEqExact "Int"

int8Inv :: TestTree
int8Inv = agroupInv Gens.int8 MkEqExact "Int8"

int16Inv :: TestTree
int16Inv = agroupInv Gens.int16 MkEqExact "Int16"

int32Inv :: TestTree
int32Inv = agroupInv Gens.int32 MkEqExact "Int32"

int64Inv :: TestTree
int64Inv = agroupInv Gens.int64 MkEqExact "Int64"

integerInv :: TestTree
integerInv = agroupInv Gens.integer MkEqExact "Integer"

rationalInv :: TestTree
rationalInv = agroupInv Gens.rational MkEqRatio "Rational"

absProps :: TestTree
absProps =
  T.testGroup
    "Absolute Value"
    [ intAbs,
      integerAbs,
      rationalAbs
    ]

intAbs :: TestTree
intAbs = agroupAbs Gens.int MkEqExact "Int"

integerAbs :: TestTree
integerAbs = agroupAbs Gens.integer MkEqExact "Integer"

rationalAbs :: TestTree
rationalAbs = agroupAbs Gens.rational MkEqRatio "Rational"

agroupSubEq :: (AGroup a, Num a, Show a) => Gen a -> (a -> Equality eq a) -> TestName -> TestTree
agroupSubEq = Utils.binaryEq (-) (.-.)

agroupInv :: (AGroup a, Show a) => Gen a -> (a -> Equality eq a) -> TestName -> TestTree
agroupInv gen equalityCons desc = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty desc $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        let x' = ginv x
        equalityCons zero === equalityCons (x .+. x')
        equalityCons zero === equalityCons (x' .+. x)

        if equalityCons x /= equalityCons zero
          then equalityCons (ginv x) /== equalityCons x
          else pure ()

agroupAbs :: (AGroup a, Ord a, Show a) => Gen a -> (a -> Equality eq a) -> TestName -> TestTree
agroupAbs gen eqCons desc = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty desc $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        y <- H.forAll gen

        -- idempotence: |x| = ||x||
        let eqX = eqCons x
            eqAbs = eqCons (gabs x)
        eqAbs === eqCons (gabs (gabs x))

        -- non-negative: |x| >= 0
        let eqZero = eqCons zero
        H.diff eqAbs (>=) eqZero

        -- positive-definite: |x| == 0 <=> x == 0
        H.diff (eqAbs == eqZero) (<=>) (eqX == eqZero)

        -- triangle equality: |x + y| <= |x| + |y|
        let sumAbs = eqCons $ gabs x .+. gabs y
            absSum = eqCons $ gabs (x .+. y)
        H.diff absSum (<=) sumAbs

agroupSubIdent :: (AGroup a, Show a) => Gen a -> TestName -> TestTree
agroupSubIdent gen desc = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty desc $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        zero === x .-. x