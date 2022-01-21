module Test.Multiplicative.MSemigroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Refined (Refined)
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
      naturalMultNum,
      wordMultNum,
      word8MultNum,
      word16MultNum,
      word32MultNum,
      word64MultNum,
      rationalMultNum,
      fractionMultNum,
      refinedPositiveMultNum,
      refinedNonNegativeMultNum,
      refinedNonZeroMultNum,
      refinedEvenMultNum,
      refinedOddMultNum
    ]

floatMultNum :: TestTree
floatMultNum = msemigroupMultNum Gens.float (MkEqEpsilon 1.0) "Float"

doubleMultNum :: TestTree
doubleMultNum = msemigroupMultNum Gens.double (MkEqEpsilon 1.0) "Double"

intMultNum :: TestTree
intMultNum = msemigroupMultNum Gens.int MkEqExact "Int"

int8MultNum :: TestTree
int8MultNum = msemigroupMultNum Gens.int8 MkEqExact "Int8"

int16MultNum :: TestTree
int16MultNum = msemigroupMultNum Gens.int16 MkEqExact "Int16"

int32MultNum :: TestTree
int32MultNum = msemigroupMultNum Gens.int32 MkEqExact "Int32"

int64MultNum :: TestTree
int64MultNum = msemigroupMultNum Gens.int64 MkEqExact "Int64"

integerMultNum :: TestTree
integerMultNum = msemigroupMultNum Gens.integer MkEqExact "Integer"

naturalMultNum :: TestTree
naturalMultNum = msemigroupMultNum Gens.natural MkEqExact "Natural"

wordMultNum :: TestTree
wordMultNum = msemigroupMultNum Gens.word MkEqExact "Word"

word8MultNum :: TestTree
word8MultNum = msemigroupMultNum Gens.word8 MkEqExact "Word8"

word16MultNum :: TestTree
word16MultNum = msemigroupMultNum Gens.word16 MkEqExact "Word16"

word32MultNum :: TestTree
word32MultNum = msemigroupMultNum Gens.word32 MkEqExact "Word32"

word64MultNum :: TestTree
word64MultNum = msemigroupMultNum Gens.word64 MkEqExact "Word64"

rationalMultNum :: TestTree
rationalMultNum = msemigroupMultNum Gens.rational MkEqExact "Rational"

fractionMultNum :: TestTree
fractionMultNum = msemigroupMultNum Gens.fraction MkEqExact "Fraction"

refinedPositiveMultNum :: TestTree
refinedPositiveMultNum = msemigroupRefinedMultNum Gens.refinedPositive "Refined Positive"

refinedNonNegativeMultNum :: TestTree
refinedNonNegativeMultNum = msemigroupRefinedMultNum Gens.refinedNonNegative "Refined NonNegative"

refinedNonZeroMultNum :: TestTree
refinedNonZeroMultNum = msemigroupRefinedMultNum Gens.refinedNZ "Refined NonZero"

refinedEvenMultNum :: TestTree
refinedEvenMultNum = msemigroupRefinedMultNum Gens.refinedEven "Refined Even"

refinedOddMultNum :: TestTree
refinedOddMultNum = msemigroupRefinedMultNum Gens.refinedOdd "Refined Odd"

msemigroupMultNum :: (MSemigroup a, Num a, Show a) => Gen a -> (a -> Equality eq a) -> TestName -> TestTree
msemigroupMultNum = Utils.binaryEq (*) (.*.)

msemigroupRefinedMultNum ::
  ( MSemigroup a,
    MSemigroup (Refined p a),
    Show a
  ) =>
  Gen (Refined p a) ->
  TestName ->
  TestTree
msemigroupRefinedMultNum = Utils.refinedBinaryEq (.*.) (.*.)

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
      naturalAssoc,
      wordAssoc,
      word8Assoc,
      word16Assoc,
      word32Assoc,
      word64Assoc,
      rationalAssoc,
      fractionAssoc,
      refinedPositiveAssoc,
      refinedNonNegativeAssoc,
      refinedNonZeroAssoc,
      refinedEvenAssoc,
      refinedOddAssoc
    ]

intAssoc :: TestTree
intAssoc = msemigroupAssoc Gens.int "Int"

int8Assoc :: TestTree
int8Assoc = msemigroupAssoc Gens.int8 "Int8"

int16Assoc :: TestTree
int16Assoc = msemigroupAssoc Gens.int16 "Int16"

int32Assoc :: TestTree
int32Assoc = msemigroupAssoc Gens.int32 "Int32"

int64Assoc :: TestTree
int64Assoc = msemigroupAssoc Gens.int64 "Int64"

integerAssoc :: TestTree
integerAssoc = msemigroupAssoc Gens.integer "Integer"

naturalAssoc :: TestTree
naturalAssoc = msemigroupAssoc Gens.natural "Natural"

wordAssoc :: TestTree
wordAssoc = msemigroupAssoc Gens.word "Word"

word8Assoc :: TestTree
word8Assoc = msemigroupAssoc Gens.word8 "Word8"

word16Assoc :: TestTree
word16Assoc = msemigroupAssoc Gens.word16 "Word16"

word32Assoc :: TestTree
word32Assoc = msemigroupAssoc Gens.word32 "Word32"

word64Assoc :: TestTree
word64Assoc = msemigroupAssoc Gens.word64 "Word64"

rationalAssoc :: TestTree
rationalAssoc = msemigroupAssoc Gens.rational "Rational"

fractionAssoc :: TestTree
fractionAssoc = msemigroupAssoc Gens.fraction "Fraction"

refinedPositiveAssoc :: TestTree
refinedPositiveAssoc = msemigroupAssoc Gens.refinedPositive "Refined Positive"

refinedNonNegativeAssoc :: TestTree
refinedNonNegativeAssoc = msemigroupAssoc Gens.refinedNonNegative "Refined NonNegative"

refinedNonZeroAssoc :: TestTree
refinedNonZeroAssoc = msemigroupAssoc Gens.refinedNZ "Refined NonZero"

refinedEvenAssoc :: TestTree
refinedEvenAssoc = msemigroupAssoc Gens.refinedEven "Refined Even"

refinedOddAssoc :: TestTree
refinedOddAssoc = msemigroupAssoc Gens.refinedOdd "Refined Odd"

msemigroupAssoc :: (MSemigroup a, Show a) => Gen a -> TestName -> TestTree
msemigroupAssoc = Utils.associativity (.*.)
