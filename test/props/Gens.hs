-- | Exports generators.
--
-- @since 0.1.0.0
module Gens
  ( -- * Basic
    float,
    double,
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

    -- * NonZero

    -- ** Combinators
    nzBounded,
    nzBounds,
    nzFloatingBounds,
    nonzeroBounded,
    nonzeroBounds,
    nonzeroFloatingBounds,
    posBounded,

    -- ** Specializations
    floatNonZero,
    doubleNonZero,
    intNonZero,
    int8NonZero,
    int16NonZero,
    int32NonZero,
    int64NonZero,
    integerNonZero,
    naturalNonZero,
    wordNonZero,
    word8NonZero,
    word16NonZero,
    word32NonZero,
    word64NonZero,
    rationalNonZero,
  )
where

import Algebra.Additive.AMonoid (AMonoid)
import Algebra.Multiplicative.MGroup (NonZero (..))
import Algebra.Multiplicative.MGroup qualified as MGroup
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Real (Ratio ((:%)))
import Hedgehog (MonadGen (GenBase))
import Hedgehog.Gen qualified as HG
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as HR

-- | Common interface for defining what bounds we should use during testing.
--
-- @since 0.1.0.0
class TestBounds a where
  minVal :: a
  maxVal :: a

-- | @since 0.1.0.0
instance TestBounds Float where
  minVal = -3.402823e+38
  maxVal = 3.402823e+38

-- | @since 0.1.0.0
instance TestBounds Double where
  minVal = -1.8e308
  maxVal = 1.8e308

-- | @since 0.1.0.0
instance TestBounds Integer where
  minVal = floor @Double -2e40
  maxVal = floor @Double 2e40

-- | @since 0.1.0.0
instance TestBounds Natural where
  minVal = 0
  maxVal = floor @Double 2e40

-- | @since 0.1.0.0
instance TestBounds Int where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1.0.0
instance TestBounds Int8 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1.0.0
instance TestBounds Int16 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1.0.0
instance TestBounds Int32 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1.0.0
instance TestBounds Int64 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1.0.0
instance TestBounds Word where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1.0.0
instance TestBounds Word8 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1.0.0
instance TestBounds Word16 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1.0.0
instance TestBounds Word32 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1.0.0
instance TestBounds Word64 where
  minVal = minBound
  maxVal = maxBound

float :: MonadGen m => m Float
float = HG.float $ HR.exponentialFloatFrom minVal 0 maxVal

floatNonZero :: MonadGen m => m (NonZero Float)
floatNonZero = nonzeroFloatingBounds HG.float minVal maxVal

double :: MonadGen m => m Double
double = HG.double $ HR.exponentialFloatFrom minVal 0 maxVal

doubleNonZero :: MonadGen m => m (NonZero Double)
doubleNonZero = nonzeroFloatingBounds HG.double minVal maxVal

integer :: MonadGen m => m Integer
integer = HG.integral $ HR.exponentialFrom minVal 0 maxVal

integerNZ :: MonadGen m => m Integer
integerNZ = nzBounds HG.integral minVal maxVal

integerNonZero :: MonadGen m => m (NonZero Integer)
integerNonZero = nonzeroBounds HG.integral minVal maxVal

natural :: MonadGen m => m Natural
natural = HG.integral $ HR.exponential minVal maxVal

naturalNZ :: MonadGen m => m Natural
naturalNZ = HG.integral $ HR.exponential 1 maxVal

naturalNonZero :: MonadGen m => m (NonZero Natural)
naturalNonZero = MGroup.unsafeAMonoidNonZero <$> naturalNZ

int :: MonadGen m => m Int
int = bounded HG.int

intNonZero :: MonadGen m => m (NonZero Int)
intNonZero = nonzeroBounded HG.int

int8 :: MonadGen m => m Int8
int8 = bounded HG.int8

int8NonZero :: MonadGen m => m (NonZero Int8)
int8NonZero = nonzeroBounded HG.int8

int16 :: MonadGen m => m Int16
int16 = bounded HG.int16

int16NonZero :: MonadGen m => m (NonZero Int16)
int16NonZero = nonzeroBounded HG.int16

int32 :: MonadGen m => m Int32
int32 = bounded HG.int32

int32NonZero :: MonadGen m => m (NonZero Int32)
int32NonZero = nonzeroBounded HG.int32

int64 :: MonadGen m => m Int64
int64 = bounded HG.int64

int64NonZero :: MonadGen m => m (NonZero Int64)
int64NonZero = nonzeroBounded HG.int64

word :: MonadGen m => m Word
word = bounded HG.word

wordNonZero :: MonadGen m => m (NonZero Word)
wordNonZero = posBounded HG.word

word8 :: MonadGen m => m Word8
word8 = bounded HG.word8

word8NonZero :: MonadGen m => m (NonZero Word8)
word8NonZero = posBounded HG.word8

word16 :: MonadGen m => m Word16
word16 = bounded HG.word16

word16NonZero :: MonadGen m => m (NonZero Word16)
word16NonZero = posBounded HG.word16

word32 :: MonadGen m => m Word32
word32 = bounded HG.word32

word32NonZero :: MonadGen m => m (NonZero Word32)
word32NonZero = posBounded HG.word32

word64 :: MonadGen m => m Word64
word64 = bounded HG.word64

word64NonZero :: MonadGen m => m (NonZero Word64)
word64NonZero = posBounded HG.word64

rational :: (GenBase m ~ Identity, MonadGen m) => m (Ratio Integer)
rational = ratioNumDenom integer pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

rationalNonZero :: (GenBase m ~ Identity, MonadGen m) => m (NonZero (Ratio Integer))
rationalNonZero = MGroup.unsafeAMonoidNonZero <$> ratioNumDenom integerNZ pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

ratioNumDenom :: (Eq a, GenBase m ~ Identity, MonadGen m, Num a) => m a -> m a -> m (Ratio a)
ratioNumDenom genNum genDenom = do
  n <- genNum
  d <- HG.filter (/= 0) genDenom
  pure (n :% d)

bounded :: (Bounded a, Integral a) => (Range a -> m a) -> m a
bounded gen = gen $ HR.exponentialFrom minBound 0 maxBound

nonzeroBounded ::
  (AMonoid a, Integral a, MonadGen m, TestBounds a) =>
  (Range a -> m a) ->
  m (NonZero a)
nonzeroBounded = fmap MGroup.unsafeAMonoidNonZero . nzBounded

nzBounded :: (Integral a, MonadGen m, TestBounds a) => (Range a -> m a) -> m a
nzBounded gen = nzBounds gen minVal maxVal

nonzeroBounds ::
  (AMonoid a, Integral a, MonadGen m) =>
  (Range a -> m a) ->
  a ->
  a ->
  m (NonZero a)
nonzeroBounds gen lower = fmap MGroup.unsafeAMonoidNonZero . nzBounds gen lower

nonzeroFloatingBounds ::
  (AMonoid a, Floating a, MonadGen m, Ord a) =>
  (Range a -> m a) ->
  a ->
  a ->
  m (NonZero a)
nonzeroFloatingBounds gen lower =
  fmap MGroup.unsafeAMonoidNonZero
    . nzFloatingBounds gen lower

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

posBounded :: (AMonoid g, Integral a, MonadGen m, TestBounds a) => (Range a -> m g) -> m (NonZero g)
posBounded gen = fmap MGroup.unsafeAMonoidNonZero <$> gen $ HR.exponential 1 maxVal