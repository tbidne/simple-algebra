module Test.Algebra.Multiplicative.MSemigroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, PropertyName)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Multiplicative Semigroup"
    [ multNumProps,
      assocProps
    ]

multNumProps :: TestTree
multNumProps =
  T.testGroup
    "(.*.) === (*)"
    [ floatMultNum,
      doubleMultNum,
      intMultNum,
      int8MultNum,
      int16MultNum,
      int32MultNum,
      int64MultNum,
      integerMultNum,
      wordMultNum,
      word8MultNum,
      word16MultNum,
      word32MultNum,
      word64MultNum,
      naturalMultNum,
      rationalMultNum
    ]

floatMultNum :: TestTree
floatMultNum = msemigroupMultNum Gens.float (MkEqEpsilon 1.0) "Float" "floatMultNum"

doubleMultNum :: TestTree
doubleMultNum = msemigroupMultNum Gens.double (MkEqEpsilon 1.0) "Double" "doubleMultNum"

intMultNum :: TestTree
intMultNum = msemigroupMultNum Gens.int MkEqExact "Int" "intMultNum"

int8MultNum :: TestTree
int8MultNum = msemigroupMultNum Gens.int8 MkEqExact "Int8" "int8MultNum"

int16MultNum :: TestTree
int16MultNum = msemigroupMultNum Gens.int16 MkEqExact "Int16" "int16MultNum"

int32MultNum :: TestTree
int32MultNum = msemigroupMultNum Gens.int32 MkEqExact "Int32" "int32MultNum"

int64MultNum :: TestTree
int64MultNum = msemigroupMultNum Gens.int64 MkEqExact "Int64" "int64MultNum"

integerMultNum :: TestTree
integerMultNum = msemigroupMultNum Gens.integer MkEqExact "Integer" "integerMultNum"

wordMultNum :: TestTree
wordMultNum = msemigroupMultNum Gens.word MkEqExact "Word" "wordMultNum"

word8MultNum :: TestTree
word8MultNum = msemigroupMultNum Gens.word8 MkEqExact "Word8" "word8MultNum"

word16MultNum :: TestTree
word16MultNum = msemigroupMultNum Gens.word16 MkEqExact "Word16" "word16MultNum"

word32MultNum :: TestTree
word32MultNum = msemigroupMultNum Gens.word32 MkEqExact "Word32" "word32MultNum"

word64MultNum :: TestTree
word64MultNum = msemigroupMultNum Gens.word64 MkEqExact "Word64" "word64MultNum"

naturalMultNum :: TestTree
naturalMultNum = msemigroupMultNum Gens.natural MkEqExact "Natural" "naturalMultNum"

rationalMultNum :: TestTree
rationalMultNum = msemigroupMultNum Gens.rational MkEqExact "Rational" "rationalMultNum"

msemigroupMultNum ::
  ( MultConstraint a ~ a,
    MSemigroup a,
    Num a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
msemigroupMultNum = Utils.binaryEq (*) (.*.)

assocProps :: TestTree
assocProps =
  T.testGroup
    "Associativity: (x .*. y) .*. z === x .*. (y .*. z)"
    [ intAssoc,
      int8Assoc,
      int16Assoc,
      int32Assoc,
      int64Assoc,
      integerAssoc,
      wordAssoc,
      word8Assoc,
      word16Assoc,
      word32Assoc,
      word64Assoc,
      naturalAssoc,
      rationalAssoc,
      nonZeroAssoc
    ]

intAssoc :: TestTree
intAssoc = msemigroupAssoc Gens.int "Int" "intAssoc"

int8Assoc :: TestTree
int8Assoc = msemigroupAssoc Gens.int8 "Int8" "int8Assoc"

int16Assoc :: TestTree
int16Assoc = msemigroupAssoc Gens.int16 "Int16" "int16Assoc"

int32Assoc :: TestTree
int32Assoc = msemigroupAssoc Gens.int32 "Int32" "int32Assoc"

int64Assoc :: TestTree
int64Assoc = msemigroupAssoc Gens.int64 "Int64" "int64Assoc"

integerAssoc :: TestTree
integerAssoc = msemigroupAssoc Gens.integer "Integer" "integerAssoc"

wordAssoc :: TestTree
wordAssoc = msemigroupAssoc Gens.word "Word" "wordAssoc"

word8Assoc :: TestTree
word8Assoc = msemigroupAssoc Gens.word8 "Word8" "word8Assoc"

word16Assoc :: TestTree
word16Assoc = msemigroupAssoc Gens.word16 "Word16" "word16Assoc"

word32Assoc :: TestTree
word32Assoc = msemigroupAssoc Gens.word32 "Word32" "word32Assoc"

word64Assoc :: TestTree
word64Assoc = msemigroupAssoc Gens.word64 "Word64" "word64Assoc"

naturalAssoc :: TestTree
naturalAssoc = msemigroupAssoc Gens.natural "Natural" "naturalAssoc"

rationalAssoc :: TestTree
rationalAssoc = msemigroupAssoc Gens.rational "Rational" "rationalAssoc"

nonZeroAssoc :: TestTree
nonZeroAssoc = msemigroupAssoc Gens.nonZero "NonZero" "nonZeroAssoc"

msemigroupAssoc ::
  ( MultConstraint a ~ a,
    MSemigroup a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
msemigroupAssoc = Utils.associativity (.*.)
