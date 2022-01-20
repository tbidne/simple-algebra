module Test.Fraction (props) where

import Algebra.Fraction (Fraction (..))
import Algebra.Fraction qualified as Frac
import Gens qualified
import Hedgehog ((===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

props :: TestTree
props =
  T.testGroup
    "Fraction"
    [ eqProps,
      reduceProps,
      numProps,
      showRoundTrip,
      numeratorProp,
      denominatorProp
    ]

eqProps :: TestTree
eqProps =
  T.testGroup
    "Equality"
    [ eqMult,
      eqZero
    ]

eqMult :: TestTree
eqMult = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "n :%: d === n * k :%: d * k" $
    H.withTests limit $
      H.property $ do
        x@(n :%: d) <- H.forAll Gens.fraction
        k <- H.forAll Gens.integerNZ
        x === (n * k :%: d * k)

eqZero :: TestTree
eqZero = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "0 :%: d1 === 0 :%: d2 " $
    H.withTests limit $
      H.property $ do
        d1 <- H.forAll Gens.integerNZ
        d2 <- H.forAll Gens.integerNZ
        (0 :%: d1) === (0 :%: d2)

reduceProps :: TestTree
reduceProps =
  T.testGroup
    "Reduction"
    [ reduceIdempotent,
      reducePosDenom,
      reduceGCD1
    ]

reduceIdempotent :: TestTree
reduceIdempotent = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "reduce x = reduce (reduce x)" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        Frac.reduce x === Frac.reduce (Frac.reduce x)

reducePosDenom :: TestTree
reducePosDenom = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "denom (reduce x) > 0" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        H.diff x ((>) . Frac.denominator) 0

reduceGCD1 :: TestTree
reduceGCD1 = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "gcd n d <= 1" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        H.diff x ((<=) . fracGcd) 1

numProps :: TestTree
numProps =
  T.testGroup
    "Num"
    [ numAddReduces,
      numSubReduces,
      numMultReduces,
      absGtZero,
      signumProp
    ]

numAddReduces :: TestTree
numAddReduces = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(+) is reduced" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        y <- H.forAll Gens.fraction
        let s = x + y
        H.annotateShow s
        H.assert $ isReduced s

numSubReduces :: TestTree
numSubReduces = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(-) is reduced" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        y <- H.forAll Gens.fraction
        let s = x - y
        H.annotateShow s
        H.assert $ isReduced s

numMultReduces :: TestTree
numMultReduces = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "(*) is reduced" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        y <- H.forAll Gens.fraction
        let s = x * y
        H.annotateShow s
        H.assert $ isReduced s

absGtZero :: TestTree
absGtZero = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "negate . negate === id" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        y <- H.forAll Gens.fraction

        -- idempotence: |x| = ||x||
        abs x === abs (abs x)

        -- non-negative: |x| >= 0
        H.diff (abs x) (>=) 0

        -- triangle equality: |x + y| <= |x| + |y|
        H.diff (abs (x + y)) (<=) (abs x + abs y)

signumProp :: TestTree
signumProp = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "negate . negate === id" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        if
            | x > 0 -> 1 === signum x
            | x == 0 -> 0 === signum x
            | otherwise -> -1 === signum x

showRoundTrip :: TestTree
showRoundTrip = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "read . show === id" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        x === read (show x)

numeratorProp :: TestTree
numeratorProp = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "numerator x@(n :%: _) === n" $
    H.withTests limit $
      H.property $ do
        x@(n :%: _) <- H.forAll Gens.fraction
        n === Frac.numerator x

denominatorProp :: TestTree
denominatorProp = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "denominator x@(_ :%: d) === d" $
    H.withTests limit $
      H.property $ do
        x@(_ :%: d) <- H.forAll Gens.fraction
        d === Frac.denominator x

isReduced :: Integral a => Fraction a -> Bool
isReduced (0 :%: d) = d == 1
isReduced x@(_ :%: d)
  | d < 0 = False
  | otherwise = fracGcd x == 1

fracGcd :: Integral a => Fraction a -> a
fracGcd (n :%: d) = gcd n d
