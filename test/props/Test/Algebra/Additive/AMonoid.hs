{-# LANGUAGE CPP #-}

-- see NOTE: [Pattern Synonym COMPLETE]
#if !MIN_VERSION_base(4, 16, 0)
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Test.Algebra.Additive.AMonoid (props) where

import Equality (Equality (MkEqExact, MkEqRatio))
import Gens qualified
import Hedgehog (Gen, PropertyName, (/==), (===))
import Hedgehog qualified as H
import Numeric.Algebra.Additive.AMonoid
  ( AMonoid (zero),
    pattern NonZero,
    pattern Zero,
  )
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Additive Monoid"
    [ identityProps,
      testAMonoidSynonym
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
      wordId,
      word8Id,
      word16Id,
      word32Id,
      word64Id,
      naturalId,
      rationalId
    ]

intId :: TestTree
intId = amonoidIdentity Gens.int MkEqExact "Int" "intId"

int8Id :: TestTree
int8Id = amonoidIdentity Gens.int8 MkEqExact "Int8" "int8Id"

int16Id :: TestTree
int16Id = amonoidIdentity Gens.int16 MkEqExact "Int16" "int16Id"

int32Id :: TestTree
int32Id = amonoidIdentity Gens.int32 MkEqExact "Int32" "int32Id"

int64Id :: TestTree
int64Id = amonoidIdentity Gens.int64 MkEqExact "Int64" "int64Id"

integerId :: TestTree
integerId = amonoidIdentity Gens.integer MkEqExact "Integer" "integerId"

wordId :: TestTree
wordId = amonoidIdentity Gens.word MkEqExact "Word" "wordId"

word8Id :: TestTree
word8Id = amonoidIdentity Gens.word8 MkEqExact "Word8" "word8Id"

word16Id :: TestTree
word16Id = amonoidIdentity Gens.word16 MkEqExact "Word16" "word16Id"

word32Id :: TestTree
word32Id = amonoidIdentity Gens.word32 MkEqExact "Word32" "word32Id"

word64Id :: TestTree
word64Id = amonoidIdentity Gens.word64 MkEqExact "Word64" "word64Id"

naturalId :: TestTree
naturalId = amonoidIdentity Gens.natural MkEqExact "Natural" "naturalId"

rationalId :: TestTree
rationalId = amonoidIdentity Gens.rational MkEqRatio "Rational" "rationalId"

amonoidIdentity ::
  ( AMonoid a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
amonoidIdentity = Utils.identity (.+.) zero

testAMonoidSynonym :: TestTree
testAMonoidSynonym = Utils.testPropertyCompat desc "testAMonoidSynonym" $
  H.property $ do
    x <- H.forAll Gens.integer
    case x of
      Zero -> zero === x
      NonZero y -> do
        x === y
        zero /== y
  where
    desc = "AMonoid pattern synonym"
