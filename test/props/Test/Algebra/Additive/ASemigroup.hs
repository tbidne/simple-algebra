module Test.Algebra.Additive.ASemigroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, PropertyName)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Additive Semigroup"
    [ addNumProps,
      assocProps
    ]

addNumProps :: TestTree
addNumProps =
  T.testGroup
    "(.+.) === (+)"
    [ floatAddNum,
      doubleAddNum,
      intAddNum,
      int8AddNum,
      int16AddNum,
      int32AddNum,
      int64AddNum,
      integerAddNum,
      wordAddNum,
      word8AddNum,
      word16AddNum,
      word32AddNum,
      word64AddNum,
      naturalAddNum,
      rationalAddNum
    ]

floatAddNum :: TestTree
floatAddNum = asemigroupAddNum Gens.float (MkEqEpsilon 1.0) "Float" "floatAddNum"

doubleAddNum :: TestTree
doubleAddNum = asemigroupAddNum Gens.double (MkEqEpsilon 1.0) "Double" "doubleAddNum"

intAddNum :: TestTree
intAddNum = asemigroupAddNum Gens.int MkEqExact "Int" "intAddNum"

int8AddNum :: TestTree
int8AddNum = asemigroupAddNum Gens.int8 MkEqExact "Int8" "int8AddNum"

int16AddNum :: TestTree
int16AddNum = asemigroupAddNum Gens.int16 MkEqExact "Int16" "int16AddNum"

int32AddNum :: TestTree
int32AddNum = asemigroupAddNum Gens.int32 MkEqExact "Int32" "int32AddNum"

int64AddNum :: TestTree
int64AddNum = asemigroupAddNum Gens.int64 MkEqExact "Int64" "int64AddNum"

integerAddNum :: TestTree
integerAddNum = asemigroupAddNum Gens.integer MkEqExact "Integer" "integerAddNum"

wordAddNum :: TestTree
wordAddNum = asemigroupAddNum Gens.word MkEqExact "Word" "wordAddNum"

word8AddNum :: TestTree
word8AddNum = asemigroupAddNum Gens.word8 MkEqExact "Word8" "word8AddNum"

word16AddNum :: TestTree
word16AddNum = asemigroupAddNum Gens.word16 MkEqExact "Word16" "word16AddNum"

word32AddNum :: TestTree
word32AddNum = asemigroupAddNum Gens.word32 MkEqExact "Word32" "word32AddNum"

word64AddNum :: TestTree
word64AddNum = asemigroupAddNum Gens.word64 MkEqExact "Word64" "word64AddNum"

naturalAddNum :: TestTree
naturalAddNum = asemigroupAddNum Gens.natural MkEqExact "Natural" "naturalAddNum"

rationalAddNum :: TestTree
rationalAddNum = asemigroupAddNum Gens.rational MkEqExact "Rational" "rationalAddNum"

asemigroupAddNum ::
  ( ASemigroup a,
    Num a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
asemigroupAddNum = Utils.binaryEq (+) (.+.)

assocProps :: TestTree
assocProps =
  T.testGroup
    "Associativity: (x .+. y) .+. z === x .+. (y .+. z)"
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
      rationalAssoc
    ]

intAssoc :: TestTree
intAssoc = asemigroupAssoc Gens.int "Int" "intAssoc"

int8Assoc :: TestTree
int8Assoc = asemigroupAssoc Gens.int8 "Int8" "int8Assoc"

int16Assoc :: TestTree
int16Assoc = asemigroupAssoc Gens.int16 "Int16" "int16Assoc"

int32Assoc :: TestTree
int32Assoc = asemigroupAssoc Gens.int32 "Int32" "int32Assoc"

int64Assoc :: TestTree
int64Assoc = asemigroupAssoc Gens.int64 "Int64" "int64Assoc"

integerAssoc :: TestTree
integerAssoc = asemigroupAssoc Gens.integer "Integer" "integerAssoc"

wordAssoc :: TestTree
wordAssoc = asemigroupAssoc Gens.word "Word" "wordAssoc"

word8Assoc :: TestTree
word8Assoc = asemigroupAssoc Gens.word8 "Word8" "word8Assoc"

word16Assoc :: TestTree
word16Assoc = asemigroupAssoc Gens.word16 "Word16" "word16Assoc"

word32Assoc :: TestTree
word32Assoc = asemigroupAssoc Gens.word32 "Word32" "word32Assoc"

word64Assoc :: TestTree
word64Assoc = asemigroupAssoc Gens.word64 "Word64" "word64Assoc"

naturalAssoc :: TestTree
naturalAssoc = asemigroupAssoc Gens.natural "Natural" "naturalAssoc"

rationalAssoc :: TestTree
rationalAssoc = asemigroupAssoc Gens.rational "Rational" "rationalAssoc"

asemigroupAssoc ::
  ( ASemigroup a,
    Eq a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
asemigroupAssoc = Utils.associativity (.+.)
