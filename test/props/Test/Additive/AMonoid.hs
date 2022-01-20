{-# LANGUAGE OverloadedStrings #-}

module Test.Additive.AMonoid (props) where

import Algebra.Additive.AMonoid (AMonoid (..))
import Algebra.Additive.ASemigroup (ASemigroup (..))
import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen)
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Additive Monoid"
    [ identityProps
    ]

identityProps :: TestTree
identityProps =
  T.testGroup
    "Identity: zero .+. x == x == x .+. zero"
    [ intId,
      int8Id,
      int16Id,
      int32Id,
      int64Id,
      integerId,
      naturalId,
      wordId,
      word8Id,
      word16Id,
      word32Id,
      word64Id,
      ratioIntegerId,
      fractionId,
      refinedNonNegativeId,
      refinedNonPositiveId,
      refinedEvenId
    ]

intId :: TestTree
intId = amonoidIdentity Gens.int MkEqExact "Int"

int8Id :: TestTree
int8Id = amonoidIdentity Gens.int8 MkEqExact "Int8"

int16Id :: TestTree
int16Id = amonoidIdentity Gens.int16 MkEqExact "Int16"

int32Id :: TestTree
int32Id = amonoidIdentity Gens.int32 MkEqExact "Int32"

int64Id :: TestTree
int64Id = amonoidIdentity Gens.int64 MkEqExact "Int64"

integerId :: TestTree
integerId = amonoidIdentity Gens.integer MkEqExact "Integer"

naturalId :: TestTree
naturalId = amonoidIdentity Gens.natural MkEqExact "Natural"

wordId :: TestTree
wordId = amonoidIdentity Gens.word MkEqExact "Word"

word8Id :: TestTree
word8Id = amonoidIdentity Gens.word8 MkEqExact "Word8"

word16Id :: TestTree
word16Id = amonoidIdentity Gens.word16 MkEqExact "Word16"

word32Id :: TestTree
word32Id = amonoidIdentity Gens.word32 MkEqExact "Word32"

word64Id :: TestTree
word64Id = amonoidIdentity Gens.word64 MkEqExact "Word64"

ratioIntegerId :: TestTree
ratioIntegerId = amonoidIdentity Gens.rational MkEqRatio "Rational"

fractionId :: TestTree
fractionId = amonoidIdentity Gens.fraction MkEqExact "Fraction"

refinedNonNegativeId :: TestTree
refinedNonNegativeId = amonoidIdentity Gens.refinedNonNegative MkEqExact "Refined NonNegative"

refinedNonPositiveId :: TestTree
refinedNonPositiveId = amonoidIdentity Gens.refinedNonPositive MkEqExact "Refined NonPositive"

refinedEvenId :: TestTree
refinedEvenId = amonoidIdentity Gens.refinedEven MkEqExact "Refined Even"

amonoidIdentity :: (AMonoid a, Show a) => Gen a -> (a -> Equality eq a) -> TestName -> TestTree
amonoidIdentity = Utils.identity (.+.) zero
