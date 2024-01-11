module Test.Algebra.Multiplicative.MGroup (props) where

import Equality (Equality (MkEqEpsilon, MkEqExact, MkEqRatio))
import Gens qualified
import Hedgehog (Gen, PropertyName, (===))
import Hedgehog qualified as H
import Numeric.Algebra.Multiplicative.MEuclidean (MEuclidean, mmod)
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Multiplicative Group"
    [ divProps,
      divIdentProps,
      divIntegralProps
    ]

divProps :: TestTree
divProps =
  T.testGroup
    "(.%.) === div / (/)"
    [ floatDiv,
      doubleDiv,
      intDiv,
      int8Div,
      int16Div,
      int32Div,
      int64Div,
      integerDiv,
      wordDiv,
      word8Div,
      word16Div,
      word32Div,
      word64Div,
      naturalDiv,
      rationalDiv
    ]

floatDiv :: TestTree
floatDiv = mgroupDivEq (/) Gens.float Gens.floatNZ (MkEqEpsilon 1.0) "Float" "floatDiv"

doubleDiv :: TestTree
doubleDiv = mgroupDivEq (/) Gens.double Gens.doubleNZ (MkEqEpsilon 1.0) "Double" "doubleDiv"

intDiv :: TestTree
intDiv = mgroupDivEq div Gens.int Gens.intNZ MkEqExact "Int" "intDiv"

int8Div :: TestTree
int8Div = mgroupDivEq div Gens.int8 Gens.int8NZ MkEqExact "Int8" "int8Div"

int16Div :: TestTree
int16Div = mgroupDivEq div Gens.int16 Gens.int16NZ MkEqExact "Int16" "int16Div"

int32Div :: TestTree
int32Div = mgroupDivEq div Gens.int32 Gens.int32NZ MkEqExact "Int32" "int32Div"

int64Div :: TestTree
int64Div = mgroupDivEq div Gens.int64 Gens.int64NZ MkEqExact "Int64" "int64Div"

integerDiv :: TestTree
integerDiv = mgroupDivEq div Gens.integer Gens.integerNZ MkEqExact "Integer" "integerDiv"

wordDiv :: TestTree
wordDiv = mgroupDivEq div Gens.word Gens.wordNZ MkEqExact "Word" "wordDiv"

word8Div :: TestTree
word8Div = mgroupDivEq div Gens.word8 Gens.word8NZ MkEqExact "Word8" "word8Div"

word16Div :: TestTree
word16Div = mgroupDivEq div Gens.word16 Gens.word16NZ MkEqExact "Word16" "word16Div"

word32Div :: TestTree
word32Div = mgroupDivEq div Gens.word32 Gens.word32NZ MkEqExact "Word32" "word32Div"

word64Div :: TestTree
word64Div = mgroupDivEq div Gens.word64 Gens.word64NZ MkEqExact "Word64" "word64Div"

naturalDiv :: TestTree
naturalDiv = mgroupDivEq div Gens.natural Gens.naturalNZ MkEqExact "Natural" "naturalDiv"

rationalDiv :: TestTree
rationalDiv = mgroupDivEq (/) Gens.rational Gens.rationalNZ MkEqRatio "Rational" "rationalDiv"

divIdentProps :: TestTree
divIdentProps =
  T.testGroup
    "Division is the inverse: one == x .%. x"
    [ intDivIdent,
      int8DivIdent,
      int16DivIdent,
      int32DivIdent,
      int64DivIdent,
      integerDivIdent,
      wordDivIdent,
      word8DivIdent,
      word16DivIdent,
      word32DivIdent,
      word64DivIdent,
      naturalDivIdent,
      rationalDivIdent
    ]

intDivIdent :: TestTree
intDivIdent = agroupDivIdent Gens.intNZ MkEqExact "Int" "intDivIdent"

int8DivIdent :: TestTree
int8DivIdent = agroupDivIdent Gens.int8NZ MkEqExact "Int8" "int8DivIdent"

int16DivIdent :: TestTree
int16DivIdent = agroupDivIdent Gens.int16NZ MkEqExact "Int16" "int16DivIdent"

int32DivIdent :: TestTree
int32DivIdent = agroupDivIdent Gens.int32NZ MkEqExact "Int32" "int32DivIdent"

int64DivIdent :: TestTree
int64DivIdent = agroupDivIdent Gens.int64NZ MkEqExact "Int64" "int64DivIdent"

integerDivIdent :: TestTree
integerDivIdent = agroupDivIdent Gens.integerNZ MkEqExact "Integer" "integerDivIdent"

wordDivIdent :: TestTree
wordDivIdent = agroupDivIdent Gens.wordNZ MkEqExact "Word" "wordDivIdent"

word8DivIdent :: TestTree
word8DivIdent = agroupDivIdent Gens.word8NZ MkEqExact "Word8" "word8DivIdent"

word16DivIdent :: TestTree
word16DivIdent = agroupDivIdent Gens.word16NZ MkEqExact "Word16" "word16DivIdent"

word32DivIdent :: TestTree
word32DivIdent = agroupDivIdent Gens.word32NZ MkEqExact "Word32" "word32DivIdent"

word64DivIdent :: TestTree
word64DivIdent = agroupDivIdent Gens.word64NZ MkEqExact "Word64" "word64DivIdent"

naturalDivIdent :: TestTree
naturalDivIdent = agroupDivIdent Gens.naturalNZ MkEqExact "Natural" "naturalDivIdent"

rationalDivIdent :: TestTree
rationalDivIdent = agroupDivIdent Gens.rationalNZ MkEqRatio "Rational" "rationalDivIdent"

mgroupDivEq ::
  (MGroup a, Show a) =>
  (a -> a -> a) ->
  Gen a ->
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
mgroupDivEq expectedFn gen genNZ eqCons desc propName =
  Utils.testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      d <- H.forAll genNZ
      let actual = x .%. d
          expected = expectedFn x d
      eqCons expected === eqCons actual

agroupDivIdent ::
  (MGroup a, Show a) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
agroupDivIdent gen eqCons desc propName =
  Utils.testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      eqCons one === eqCons (x .%. x)

divIntegralProps :: TestTree
divIntegralProps =
  T.testGroup
    "mmod === mod"
    [ intDivIntegral,
      int8DivIntegral,
      int16DivIntegral,
      int32DivIntegral,
      int64DivIntegral,
      integerDivIntegral,
      wordDivIntegral,
      word8DivIntegral,
      word16DivIntegral,
      word32DivIntegral,
      word64DivIntegral,
      naturalDivIntegral
    ]

intDivIntegral :: TestTree
intDivIntegral = mgroupDivIntegralEq Gens.int Gens.intNZ "Int" "intDivIntegral"

int8DivIntegral :: TestTree
int8DivIntegral = mgroupDivIntegralEq Gens.int8 Gens.int8NZ "Int8" "int8DivIntegral"

int16DivIntegral :: TestTree
int16DivIntegral = mgroupDivIntegralEq Gens.int16 Gens.int16NZ "Int16" "int16DivIntegral"

int32DivIntegral :: TestTree
int32DivIntegral = mgroupDivIntegralEq Gens.int32 Gens.int32NZ "Int32" "int32DivIntegral"

int64DivIntegral :: TestTree
int64DivIntegral = mgroupDivIntegralEq Gens.int64 Gens.int64NZ "Int64" "int64DivIntegral"

integerDivIntegral :: TestTree
integerDivIntegral = mgroupDivIntegralEq Gens.integer Gens.integerNZ "Integer" "integerDivIntegral"

wordDivIntegral :: TestTree
wordDivIntegral = mgroupDivIntegralEq Gens.word Gens.wordNZ "Word" "wordDivIntegral"

word8DivIntegral :: TestTree
word8DivIntegral = mgroupDivIntegralEq Gens.word8 Gens.word8NZ "Word8" "word8DivIntegral"

word16DivIntegral :: TestTree
word16DivIntegral = mgroupDivIntegralEq Gens.word16 Gens.word16NZ "Word16" "word15DivIntegral"

word32DivIntegral :: TestTree
word32DivIntegral = mgroupDivIntegralEq Gens.word32 Gens.word32NZ "Word32" "word32DivIntegral"

word64DivIntegral :: TestTree
word64DivIntegral = mgroupDivIntegralEq Gens.word64 Gens.word64NZ "Word64" "word64DivIntegral"

naturalDivIntegral :: TestTree
naturalDivIntegral = mgroupDivIntegralEq Gens.natural Gens.naturalNZ "Natural" "naturalDivIntegral"

mgroupDivIntegralEq ::
  ( Integral a,
    MEuclidean a,
    Show a
  ) =>
  Gen a ->
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
mgroupDivIntegralEq gen genNZ desc propName =
  Utils.testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      d <- H.forAll genNZ
      x `mod` d === x `mmod` d
