module Test.Additive.ASemigroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen)
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
      naturalAddNum,
      wordAddNum,
      word8AddNum,
      word16AddNum,
      word32AddNum,
      word64AddNum,
      ratioIntegerAddNum,
      fractionAddNum
    ]

floatAddNum :: TestTree
floatAddNum = asemigroupAddNum Gens.float (MkEqEpsilon 1.0) "Float"

doubleAddNum :: TestTree
doubleAddNum = asemigroupAddNum Gens.double (MkEqEpsilon 1.0) "Double"

intAddNum :: TestTree
intAddNum = asemigroupAddNum Gens.int MkEqExact "Int"

int8AddNum :: TestTree
int8AddNum = asemigroupAddNum Gens.int8 MkEqExact "Int8"

int16AddNum :: TestTree
int16AddNum = asemigroupAddNum Gens.int16 MkEqExact "Int16"

int32AddNum :: TestTree
int32AddNum = asemigroupAddNum Gens.int32 MkEqExact "Int32"

int64AddNum :: TestTree
int64AddNum = asemigroupAddNum Gens.int64 MkEqExact "Int64"

integerAddNum :: TestTree
integerAddNum = asemigroupAddNum Gens.integer MkEqExact "Integer"

naturalAddNum :: TestTree
naturalAddNum = asemigroupAddNum Gens.natural MkEqExact "Natural"

wordAddNum :: TestTree
wordAddNum = asemigroupAddNum Gens.word MkEqExact "Word"

word8AddNum :: TestTree
word8AddNum = asemigroupAddNum Gens.word8 MkEqExact "Word8"

word16AddNum :: TestTree
word16AddNum = asemigroupAddNum Gens.word16 MkEqExact "Word16"

word32AddNum :: TestTree
word32AddNum = asemigroupAddNum Gens.word32 MkEqExact "Word32"

word64AddNum :: TestTree
word64AddNum = asemigroupAddNum Gens.word64 MkEqExact "Word64"

ratioIntegerAddNum :: TestTree
ratioIntegerAddNum = asemigroupAddNum Gens.rational MkEqExact "Rational"

fractionAddNum :: TestTree
fractionAddNum = asemigroupAddNum Gens.fraction MkEqExact "Rational"

asemigroupAddNum :: (ASemigroup a, Num a, Show a) => Gen a -> (a -> Equality eq a) -> TestName -> TestTree
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
      naturalAssoc,
      wordAssoc,
      word8Assoc,
      word16Assoc,
      word32Assoc,
      word64Assoc,
      rationalAssoc,
      fractionAssoc
    ]

intAssoc :: TestTree
intAssoc = asemigroupAssoc Gens.int "Int"

int8Assoc :: TestTree
int8Assoc = asemigroupAssoc Gens.int8 "Int8"

int16Assoc :: TestTree
int16Assoc = asemigroupAssoc Gens.int16 "Int16"

int32Assoc :: TestTree
int32Assoc = asemigroupAssoc Gens.int32 "Int32"

int64Assoc :: TestTree
int64Assoc = asemigroupAssoc Gens.int64 "Int64"

integerAssoc :: TestTree
integerAssoc = asemigroupAssoc Gens.integer "Integer"

naturalAssoc :: TestTree
naturalAssoc = asemigroupAssoc Gens.natural "Natural"

wordAssoc :: TestTree
wordAssoc = asemigroupAssoc Gens.word "Word"

word8Assoc :: TestTree
word8Assoc = asemigroupAssoc Gens.word8 "Word8"

word16Assoc :: TestTree
word16Assoc = asemigroupAssoc Gens.word16 "Word16"

word32Assoc :: TestTree
word32Assoc = asemigroupAssoc Gens.word32 "Word32"

word64Assoc :: TestTree
word64Assoc = asemigroupAssoc Gens.word64 "Word64"

rationalAssoc :: TestTree
rationalAssoc = asemigroupAssoc Gens.rational "Rational"

fractionAssoc :: TestTree
fractionAssoc = asemigroupAssoc Gens.fraction "Fraction"

asemigroupAssoc :: (ASemigroup a, Show a) => Gen a -> TestName -> TestTree
asemigroupAssoc = Utils.associativity (.+.)
