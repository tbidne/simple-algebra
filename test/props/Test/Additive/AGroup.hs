module Test.Additive.AGroup (props) where

import Algebra.Additive.AGroup (AGroup (..))
import Algebra.Additive.AMonoid (AMonoid (..))
import Algebra.Additive.ASemigroup (ASemigroup (..))
import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, (===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import Utils ((<=>))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Additive Group"
    [ subProps,
      subRefinedProps,
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
      wordSub,
      word8Sub,
      word16Sub,
      word32Sub,
      word64Sub,
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

wordSub :: TestTree
wordSub = agroupSubEq Gens.word MkEqExact "Word"

word8Sub :: TestTree
word8Sub = agroupSubEq Gens.word8 MkEqExact "Word8"

word16Sub :: TestTree
word16Sub = agroupSubEq Gens.word16 MkEqExact "Word16"

word32Sub :: TestTree
word32Sub = agroupSubEq Gens.word32 MkEqExact "Word32"

word64Sub :: TestTree
word64Sub = agroupSubEq Gens.word64 MkEqExact "Word64"

ratioIntegerSub :: TestTree
ratioIntegerSub = agroupSubEq Gens.rational MkEqExact "Rational"

subRefinedProps :: TestTree
subRefinedProps =
  T.testGroup
    "Refined (.-.) === base (.-.) and preserves refinement"
    [ refinedEvenSub
    ]

refinedEvenSub :: TestTree
refinedEvenSub = Utils.refinedBinaryEq (-) (.-.) Gens.refinedEven "Refined Even"

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
      wordSubIdent,
      word8SubIdent,
      word16SubIdent,
      word32SubIdent,
      word64SubIdent,
      rationalSubIdent,
      refinedEvenSubIdent
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

wordSubIdent :: TestTree
wordSubIdent = agroupSubIdent Gens.word "Word"

word8SubIdent :: TestTree
word8SubIdent = agroupSubIdent Gens.word8 "Word8"

word16SubIdent :: TestTree
word16SubIdent = agroupSubIdent Gens.word16 "Word16"

word32SubIdent :: TestTree
word32SubIdent = agroupSubIdent Gens.word32 "Word32"

word64SubIdent :: TestTree
word64SubIdent = agroupSubIdent Gens.word64 "Word64"

rationalSubIdent :: TestTree
rationalSubIdent = agroupSubIdent Gens.rational "Rational"

refinedEvenSubIdent :: TestTree
refinedEvenSubIdent = agroupSubIdent Gens.refinedEven "Refined Even"

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
      wordInv,
      word8Inv,
      word16Inv,
      word32Inv,
      word64Inv,
      rationalInv,
      refinedEvenInv
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

wordInv :: TestTree
wordInv = agroupInv Gens.word MkEqExact "Word"

word8Inv :: TestTree
word8Inv = agroupInv Gens.word8 MkEqExact "Word8"

word16Inv :: TestTree
word16Inv = agroupInv Gens.word16 MkEqExact "Word16"

word32Inv :: TestTree
word32Inv = agroupInv Gens.word32 MkEqExact "Word32"

word64Inv :: TestTree
word64Inv = agroupInv Gens.word64 MkEqExact "Word64"

rationalInv :: TestTree
rationalInv = agroupInv Gens.rational MkEqRatio "Rational"

refinedEvenInv :: TestTree
refinedEvenInv = agroupInv Gens.refinedEven MkEqExact "Refined Even"

absProps :: TestTree
absProps =
  T.testGroup
    "Absolute Value"
    [ intAbs,
      integerAbs,
      rationalAbs,
      refinedEvenAbs
    ]

intAbs :: TestTree
intAbs = agroupAbs Gens.int MkEqExact "Int"

integerAbs :: TestTree
integerAbs = agroupAbs Gens.integer MkEqExact "Integer"

rationalAbs :: TestTree
rationalAbs = agroupAbs Gens.rational MkEqRatio "Rational"

refinedEvenAbs :: TestTree
refinedEvenAbs = agroupAbs Gens.refinedEven MkEqExact "Refined Even"

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
