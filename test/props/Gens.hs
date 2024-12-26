-- | Exports generators.
--
-- @since 0.1
module Gens
  ( -- * Basic

    -- ** Floating
    float,
    double,

    -- ** Integral
    int,
    int8,
    int16,
    int32,
    int64,
    integer,
    natural,
    word,
    word8,
    word16,
    word32,
    word64,
    rational,
    rationalNat,

    -- * NonZero

    -- ** Specializations
    floatNZ,
    doubleNZ,
    intNZ,
    int8NZ,
    int16NZ,
    int32NZ,
    int64NZ,
    integerNZ,
    naturalNZ,
    wordNZ,
    word8NZ,
    word16NZ,
    word32NZ,
    word64NZ,
    rationalNZ,

    -- ** Combinators
    nzBounded,
    nzBounds,
    nzFloatingBounds,
  )
where

import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Real (Ratio ((:%)))
import Hedgehog (MonadGen (GenBase))
import Hedgehog.Gen qualified as HG
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as HR
import Test.TestBounds (TestBounds (maxVal, minVal))

float :: (MonadGen m) => m Float
float = HG.float $ HR.exponentialFloatFrom minVal 0 maxVal

double :: (MonadGen m) => m Double
double = HG.double $ HR.exponentialFloatFrom minVal 0 maxVal

integer :: (MonadGen m) => m Integer
integer = HG.integral $ HR.exponentialFrom minVal 0 maxVal

natural :: (MonadGen m) => m Natural
natural = HG.integral $ HR.exponential minVal maxVal

int :: (MonadGen m) => m Int
int = bounded HG.int

int8 :: (MonadGen m) => m Int8
int8 = bounded HG.int8

int16 :: (MonadGen m) => m Int16
int16 = bounded HG.int16

int32 :: (MonadGen m) => m Int32
int32 = bounded HG.int32

int64 :: (MonadGen m) => m Int64
int64 = bounded HG.int64

word :: (MonadGen m) => m Word
word = bounded HG.word

word8 :: (MonadGen m) => m Word8
word8 = bounded HG.word8

word16 :: (MonadGen m) => m Word16
word16 = bounded HG.word16

word32 :: (MonadGen m) => m Word32
word32 = bounded HG.word32

word64 :: (MonadGen m) => m Word64
word64 = bounded HG.word64

rational :: (GenBase m ~ Identity, MonadGen m) => m (Ratio Integer)
rational = ratioNumDenom integer pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

rationalNat :: (GenBase m ~ Identity, MonadGen m) => m (Ratio Natural)
rationalNat = ratioNumDenom natural pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

ratioNumDenom :: (Eq a, GenBase m ~ Identity, MonadGen m, Num a) => m a -> m a -> m (Ratio a)
ratioNumDenom genNum genDenom = do
  n <- genNum
  d <- HG.filter (/= 0) genDenom
  pure (n :% d)

floatNZ :: (MonadGen m) => m Float
floatNZ = nzFloatingBounds HG.float minVal maxVal

doubleNZ :: (MonadGen m) => m Double
doubleNZ = nzFloatingBounds HG.double minVal maxVal

intNZ :: (MonadGen m) => m Int
intNZ = nzBounded HG.int

int8NZ :: (MonadGen m) => m Int8
int8NZ = nzBounded HG.int8

int16NZ :: (MonadGen m) => m Int16
int16NZ = nzBounded HG.int16

int32NZ :: (MonadGen m) => m Int32
int32NZ = nzBounded HG.int32

int64NZ :: (MonadGen m) => m Int64
int64NZ = nzBounded HG.int64

integerNZ :: (MonadGen m) => m Integer
integerNZ = nzBounds HG.integral minVal maxVal

naturalNZ :: (MonadGen m) => m Natural
naturalNZ = HG.integral $ HR.exponential 1 maxVal

wordNZ :: (MonadGen m) => m Word
wordNZ = posBounded HG.word

word8NZ :: (MonadGen m) => m Word8
word8NZ = posBounded HG.word8

word16NZ :: (MonadGen m) => m Word16
word16NZ = posBounded HG.word16

word32NZ :: (MonadGen m) => m Word32
word32NZ = posBounded HG.word32

word64NZ :: (MonadGen m) => m Word64
word64NZ = posBounded HG.word64

rationalNZ :: (GenBase m ~ Identity, MonadGen m) => m (Ratio Integer)
rationalNZ = ratioNumDenom integerNZ pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

nzBounded :: (Integral a, MonadGen m, TestBounds a) => (Range a -> m a) -> m a
nzBounded gen = nzBounds gen minVal maxVal

nzBounds :: (Integral a, MonadGen m) => (Range a -> m a) -> a -> a -> m a
nzBounds gen lower upper =
  HG.choice
    [ gen (HR.exponential lower -1),
      gen (HR.exponential 1 upper)
    ]

nzFloatingBounds :: (Floating a, MonadGen m, Ord a) => (Range a -> m a) -> a -> a -> m a
nzFloatingBounds gen lower upper =
  HG.choice
    [ gen (HR.exponentialFloat lower -1),
      gen (HR.exponentialFloat 1 upper)
    ]

posBounded ::
  (Integral a, TestBounds a) =>
  (Range a -> m g) ->
  m g
posBounded gen = gen $ HR.exponential 1 maxVal

bounded :: (Integral a, TestBounds a) => (Range a -> m a) -> m a
bounded gen = gen $ HR.exponentialFrom minVal 0 maxVal
