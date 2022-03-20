-- | Exports generators.
--
-- @since 0.1.0.0
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
    fraction,
    modN,
    modP,
    nonNegative,
    nonZero,
    positive,

    -- * NonZero

    -- ** Specializations
    floatNonZero,
    doubleNonZero,
    intNonZero,
    int8NonZero,
    int16NonZero,
    int32NonZero,
    int64NonZero,
    integerNZ,
    integerNonZero,
    naturalNonZero,
    wordNonZero,
    word8NonZero,
    word16NonZero,
    word32NonZero,
    word64NonZero,
    rationalNonZero,
    fractionNonZero,
    modPNonZero,
    nonNegativeNonZero,

    -- ** Combinators
    nzBounded,
    nzBounds,
    nzFloatingBounds,
    nonzeroBounded,
    nonzeroBounds,
    nonzeroFloatingBounds,
    posBounded,
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
import Numeric.Algebra.Additive.AMonoid (AMonoid)
import Numeric.Algebra.Multiplicative.MGroup qualified as MGroup
import Numeric.Data.Fraction (Fraction (..))
import Numeric.Data.ModN (ModN (..), mkModN)
import Numeric.Data.ModP (ModP (..), unsafeModP)
import Numeric.Data.NonNegative (NonNegative (..), unsafeNonNegative)
import Numeric.Data.NonZero (NonZero (..), unsafeNonZero)
import Numeric.Data.Positive (Positive (..), unsafePositive)
import Test.TestBounds (TestBounds (..))

float :: MonadGen m => m Float
float = HG.float $ HR.exponentialFloatFrom minVal 0 maxVal

double :: MonadGen m => m Double
double = HG.double $ HR.exponentialFloatFrom minVal 0 maxVal

integer :: MonadGen m => m Integer
integer = HG.integral $ HR.exponentialFrom minVal 0 maxVal

natural :: MonadGen m => m Natural
natural = HG.integral $ HR.exponential minVal maxVal

int :: MonadGen m => m Int
int = bounded HG.int

int8 :: MonadGen m => m Int8
int8 = bounded HG.int8

int16 :: MonadGen m => m Int16
int16 = bounded HG.int16

int32 :: MonadGen m => m Int32
int32 = bounded HG.int32

int64 :: MonadGen m => m Int64
int64 = bounded HG.int64

word :: MonadGen m => m Word
word = bounded HG.word

word8 :: MonadGen m => m Word8
word8 = bounded HG.word8

word16 :: MonadGen m => m Word16
word16 = bounded HG.word16

word32 :: MonadGen m => m Word32
word32 = bounded HG.word32

word64 :: MonadGen m => m Word64
word64 = bounded HG.word64

rational :: (GenBase m ~ Identity, MonadGen m) => m (Ratio Integer)
rational = ratioNumDenom integer pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

ratioNumDenom :: (Eq a, GenBase m ~ Identity, MonadGen m, Num a) => m a -> m a -> m (Ratio a)
ratioNumDenom genNum genDenom = do
  n <- genNum
  d <- HG.filter (/= 0) genDenom
  pure (n :% d)

fraction :: MonadGen m => m (Fraction Integer)
fraction = (:%:) <$> integer <*> integerNZ

modN :: MonadGen m => m (ModN 10 Natural)
modN = mkModN <$> natural

modP :: MonadGen m => m (ModP 17 Natural)
modP = unsafeModP <$> natural

nonNegative :: MonadGen m => m (NonNegative Natural)
nonNegative = unsafeNonNegative <$> natural

nonZero :: MonadGen m => m (NonZero Integer)
nonZero = unsafeNonZero <$> integerNZ

positive :: MonadGen m => m (Positive Integer)
positive = unsafePositive <$> pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

floatNonZero :: MonadGen m => m (NonZero Float)
floatNonZero = nonzeroFloatingBounds HG.float minVal maxVal

doubleNonZero :: MonadGen m => m (NonZero Double)
doubleNonZero = nonzeroFloatingBounds HG.double minVal maxVal

intNonZero :: MonadGen m => m (NonZero Int)
intNonZero = nonzeroBounded HG.int

int8NonZero :: MonadGen m => m (NonZero Int8)
int8NonZero = nonzeroBounded HG.int8

int16NonZero :: MonadGen m => m (NonZero Int16)
int16NonZero = nonzeroBounded HG.int16

int32NonZero :: MonadGen m => m (NonZero Int32)
int32NonZero = nonzeroBounded HG.int32

int64NonZero :: MonadGen m => m (NonZero Int64)
int64NonZero = nonzeroBounded HG.int64

integerNZ :: MonadGen m => m Integer
integerNZ = nzBounds HG.integral minVal maxVal

integerNonZero :: MonadGen m => m (NonZero Integer)
integerNonZero = nonzeroBounds HG.integral minVal maxVal

naturalNZ :: MonadGen m => m Natural
naturalNZ = HG.integral $ HR.exponential 1 maxVal

naturalNonZero :: MonadGen m => m (NonZero Natural)
naturalNonZero = MGroup.unsafeAMonoidNonZero <$> naturalNZ

wordNonZero :: MonadGen m => m (NonZero Word)
wordNonZero = posBounded HG.word

word8NonZero :: MonadGen m => m (NonZero Word8)
word8NonZero = posBounded HG.word8

word16NonZero :: MonadGen m => m (NonZero Word16)
word16NonZero = posBounded HG.word16

word32NonZero :: MonadGen m => m (NonZero Word32)
word32NonZero = posBounded HG.word32

word64NonZero :: MonadGen m => m (NonZero Word64)
word64NonZero = posBounded HG.word64

rationalNonZero :: (GenBase m ~ Identity, MonadGen m) => m (NonZero (Ratio Integer))
rationalNonZero = MGroup.unsafeAMonoidNonZero <$> ratioNumDenom integerNZ pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

fractionNonZero :: MonadGen m => m (NonZero (Fraction Integer))
fractionNonZero = fmap MGroup.unsafeAMonoidNonZero $ (:%:) <$> integerNZ <*> integerNZ

modPNonZero :: (GenBase m ~ Identity, MonadGen m) => m (NonZero (ModP 17 Natural))
modPNonZero = MGroup.unsafeAMonoidNonZero . unsafeModP <$> pos
  where
    pos = HG.filter (\x -> x `mod` 17 /= 0) $ HG.integral $ HR.exponential 1 maxVal

nonNegativeNonZero :: MonadGen m => m (NonZero (NonNegative Natural))
nonNegativeNonZero = MGroup.unsafeAMonoidNonZero . unsafeNonNegative <$> naturalNZ

nzBounded :: (Integral a, MonadGen m, TestBounds a) => (Range a -> m a) -> m a
nzBounded gen = nzBounds gen minVal maxVal

nzBounds :: (Integral a, MonadGen m) => (Range a -> m a) -> a -> a -> m a
nzBounds gen lower upper =
  HG.choice
    [ gen (HR.exponential lower -1),
      gen (HR.exponential 1 upper)
    ]

nonzeroBounded ::
  (AMonoid a, Integral a, MonadGen m, TestBounds a) =>
  (Range a -> m a) ->
  m (NonZero a)
nonzeroBounded = fmap MGroup.unsafeAMonoidNonZero . nzBounded

nzFloatingBounds :: (Floating a, MonadGen m, Ord a) => (Range a -> m a) -> a -> a -> m a
nzFloatingBounds gen lower upper =
  HG.choice
    [ gen (HR.exponentialFloat lower -1),
      gen (HR.exponentialFloat 1 upper)
    ]

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

posBounded :: (AMonoid g, Integral a, MonadGen m, TestBounds a) => (Range a -> m g) -> m (NonZero g)
posBounded gen = fmap MGroup.unsafeAMonoidNonZero <$> gen $ HR.exponential 1 maxVal

bounded :: (Integral a, TestBounds a) => (Range a -> m a) -> m a
bounded gen = gen $ HR.exponentialFrom minVal 0 maxVal
