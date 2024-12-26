module Test.Algebra.Multiplicative.MEuclidean (props) where

import Gens qualified
import Hedgehog (Gen, PropertyName, (===))
import Hedgehog qualified as H
import Numeric.Algebra.Additive.AMonoid (AMonoid)
import Numeric.Algebra.Multiplicative.MEuclidean (MEuclidean, mgcd, mlcm, mmod)
import Numeric.Algebra.Normed (Normed)
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "MEuclidean"
    [ divIntegralProps,
      gcdProps,
      lcmProps
    ]

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

gcdProps :: TestTree
gcdProps =
  T.testGroup
    "gcd === mgcd"
    [ intGcd,
      int8Gcd,
      int16Gcd,
      int32Gcd,
      int64Gcd,
      integerGcd,
      wordGcd,
      word8Gcd,
      word16Gcd,
      word32Gcd,
      word64Gcd,
      naturalGcd
    ]

intGcd :: TestTree
intGcd = testGcd Gens.int "Int" "intGcd"

int8Gcd :: TestTree
int8Gcd = testGcd Gens.int8 "Int8" "int8Gcd"

int16Gcd :: TestTree
int16Gcd = testGcd Gens.int16 "Int16" "int16Gcd"

int32Gcd :: TestTree
int32Gcd = testGcd Gens.int32 "Int32" "int32Gcd"

int64Gcd :: TestTree
int64Gcd = testGcd Gens.int64 "Int64" "int64Gcd"

integerGcd :: TestTree
integerGcd = testGcd Gens.integer "Integer" "integerGcd"

wordGcd :: TestTree
wordGcd = testGcd Gens.word "Word" "wordGcd"

word8Gcd :: TestTree
word8Gcd = testGcd Gens.word8 "Word8" "word8Gcd"

word16Gcd :: TestTree
word16Gcd = testGcd Gens.word16 "Word16" "word15Gcd"

word32Gcd :: TestTree
word32Gcd = testGcd Gens.word32 "Word32" "word32Gcd"

word64Gcd :: TestTree
word64Gcd = testGcd Gens.word64 "Word64" "word64Gcd"

naturalGcd :: TestTree
naturalGcd = testGcd Gens.natural "Natural" "naturalGcd"

testGcd ::
  ( AMonoid a,
    Integral a,
    MEuclidean a,
    Normed a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
testGcd gen desc propName =
  Utils.testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      y <- H.forAll gen

      gcd x y === mgcd x y

lcmProps :: TestTree
lcmProps =
  T.testGroup
    "lcm === mlcm"
    [ testLcm Gens.int "Int" "intLcm",
      testLcm Gens.int8 "Int8" "int8Lcm",
      testLcm Gens.int16 "Int16" "int16Lcm",
      testLcm Gens.int32 "Int32" "in32tLcm",
      testLcm Gens.int64 "Int64" "int54Lcm",
      testLcm Gens.integer "Integer" "integerLcm",
      testLcm Gens.word "Word" "wordLcm",
      testLcm Gens.word8 "Word8" "word8Lcm",
      testLcm Gens.word16 "Word16" "word16Lcm",
      testLcm Gens.word32 "Word32" "word32Lcm",
      testLcm Gens.word64 "Word64" "word64Lcm",
      testLcm Gens.natural "Natural" "naturalLcm"
    ]

testLcm ::
  ( AMonoid a,
    Integral a,
    MEuclidean a,
    Normed a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
testLcm gen desc propName =
  Utils.testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      y <- H.forAll gen

      lcm x y === mlcm x y
