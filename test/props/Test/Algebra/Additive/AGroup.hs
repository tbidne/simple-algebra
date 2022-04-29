module Test.Algebra.Additive.AGroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, PropertyName, (===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Numeric.Algebra.Additive.AGroup (AGroup (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Additive Group"
    [ subProps,
      subIdentProps
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
      rationalSub
    ]

floatSub :: TestTree
floatSub = agroupSubEq Gens.float (MkEqEpsilon 1.0) "Float" "floatSub"

doubleSub :: TestTree
doubleSub = agroupSubEq Gens.double (MkEqEpsilon 1.0) "Double" "doubleSub"

intSub :: TestTree
intSub = agroupSubEq Gens.int MkEqExact "Int" "intSub"

int8Sub :: TestTree
int8Sub = agroupSubEq Gens.int8 MkEqExact "Int8" "int8Sub"

int16Sub :: TestTree
int16Sub = agroupSubEq Gens.int16 MkEqExact "Int16" "int16Sub"

int32Sub :: TestTree
int32Sub = agroupSubEq Gens.int32 MkEqExact "Int32" "int32Sub"

int64Sub :: TestTree
int64Sub = agroupSubEq Gens.int64 MkEqExact "Int64" "int64Sub"

integerSub :: TestTree
integerSub = agroupSubEq Gens.integer MkEqExact "Integer" "integerSub"

wordSub :: TestTree
wordSub = agroupSubEq Gens.word MkEqExact "Word" "wordSub"

word8Sub :: TestTree
word8Sub = agroupSubEq Gens.word8 MkEqExact "Word8" "word8Sub"

word16Sub :: TestTree
word16Sub = agroupSubEq Gens.word16 MkEqExact "Word16" "word16Sub"

word32Sub :: TestTree
word32Sub = agroupSubEq Gens.word32 MkEqExact "Word32" "word32Sub"

word64Sub :: TestTree
word64Sub = agroupSubEq Gens.word64 MkEqExact "Word64" "word64Sub"

rationalSub :: TestTree
rationalSub = agroupSubEq Gens.rational MkEqExact "Rational" "rationalSub"

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
      rationalSubIdent
    ]

intSubIdent :: TestTree
intSubIdent = agroupSubIdent Gens.int "Int" "intSubIdent"

int8SubIdent :: TestTree
int8SubIdent = agroupSubIdent Gens.int8 "Int8" "int8SubIdent"

int16SubIdent :: TestTree
int16SubIdent = agroupSubIdent Gens.int16 "Int16" "int16SubIdent"

int32SubIdent :: TestTree
int32SubIdent = agroupSubIdent Gens.int32 "Int32" "int32SubIdent"

int64SubIdent :: TestTree
int64SubIdent = agroupSubIdent Gens.int64 "Int64" "int64SubIdent"

integerSubIdent :: TestTree
integerSubIdent = agroupSubIdent Gens.integer "Integer" "integerSubIdent"

wordSubIdent :: TestTree
wordSubIdent = agroupSubIdent Gens.word "Word" "wordSubIdent"

word8SubIdent :: TestTree
word8SubIdent = agroupSubIdent Gens.word8 "Word8" "word8SubIdent"

word16SubIdent :: TestTree
word16SubIdent = agroupSubIdent Gens.word16 "Word16" "word16SubIdent"

word32SubIdent :: TestTree
word32SubIdent = agroupSubIdent Gens.word32 "Word32" "word32SubIdent"

word64SubIdent :: TestTree
word64SubIdent = agroupSubIdent Gens.word64 "Word64" "word64SubIdent"

rationalSubIdent :: TestTree
rationalSubIdent = agroupSubIdent Gens.rational "Rational" "rationalSubIdent"

agroupSubEq ::
  ( AGroup a,
    Num a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
agroupSubEq = Utils.binaryEq (-) (.-.)

agroupSubIdent ::
  ( AGroup a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
agroupSubIdent gen desc propName = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat desc propName $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        zero === x .-. x
