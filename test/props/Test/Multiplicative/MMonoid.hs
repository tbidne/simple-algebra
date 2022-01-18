module Test.Multiplicative.MMonoid (props) where

import Algebra.Multiplicative.MMonoid (MMonoid (..))
import Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen)
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Multiplicative Monoid"
    [ identityProps
    ]

identityProps :: TestTree
identityProps =
  T.testGroup
    "Identity: one .*. x == x == x .*. one"
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
      ratioIntegerId
    ]

intId :: TestTree
intId = mmonoidIdentity Gens.int MkEqExact "Int"

int8Id :: TestTree
int8Id = mmonoidIdentity Gens.int8 MkEqExact "Int8"

int16Id :: TestTree
int16Id = mmonoidIdentity Gens.int16 MkEqExact "Int16"

int32Id :: TestTree
int32Id = mmonoidIdentity Gens.int32 MkEqExact "Int32"

int64Id :: TestTree
int64Id = mmonoidIdentity Gens.int64 MkEqExact "Int64"

integerId :: TestTree
integerId = mmonoidIdentity Gens.integer MkEqExact "Integer"

naturalId :: TestTree
naturalId = mmonoidIdentity Gens.natural MkEqExact "Natural"

wordId :: TestTree
wordId = mmonoidIdentity Gens.word MkEqExact "Word"

word8Id :: TestTree
word8Id = mmonoidIdentity Gens.word8 MkEqExact "Word8"

word16Id :: TestTree
word16Id = mmonoidIdentity Gens.word16 MkEqExact "Word16"

word32Id :: TestTree
word32Id = mmonoidIdentity Gens.word32 MkEqExact "Word32"

word64Id :: TestTree
word64Id = mmonoidIdentity Gens.word64 MkEqExact "Word64"

ratioIntegerId :: TestTree
ratioIntegerId = mmonoidIdentity Gens.rational MkEqRatio "Rational"

mmonoidIdentity :: (MMonoid a, Show a) => Gen a -> (a -> Equality eq a) -> TestName -> TestTree
mmonoidIdentity = Utils.identity (.*.) one
