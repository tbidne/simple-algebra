module Main (main) where

import Criterion (Benchmark)
import Criterion qualified as C
import Criterion.Main qualified as C
import GHC.Int (Int16, Int32, Int64, Int8)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Data.Fraction (Fraction (..))

main :: IO ()
main =
  C.defaultMain
    [ grp
    ]

plus :: Num a => a -> a -> a
plus = (+)

plus' :: ASemigroup a => a -> a -> a
plus' = (.+.)

grp :: Benchmark
grp =
  C.bgroup
    "Addition"
    [ C.bench "Int (+)" $ C.nf (plus @Int 10_000_000) 10_000_000,
      C.bench "Int (.+.)" $ C.nf (plus' @Int 10_000_000) 10_000_000,
      C.bench "Int8 (+)" $ C.nf (plus @Int8 10) 10,
      C.bench "Int8 (.+.)" $ C.nf (plus' @Int8 10) 10,
      C.bench "Int16 (+)" $ C.nf (plus @Int16 1_000) 1_000,
      C.bench "Int16 (.+.)" $ C.nf (plus' @Int16 1_000) 1_000,
      C.bench "Int32 (+)" $ C.nf (plus @Int32 10_000) 10_000,
      C.bench "Int32 (.+.)" $ C.nf (plus' @Int32 10_000) 10_000,
      C.bench "Int64 (+)" $ C.nf (plus @Int64 100_000_000) 100_000_000,
      C.bench "Int64 (.+.)" $ C.nf (plus' @Int64 100_000_000) 100_000_000,
      C.bench "Integer (+)" $ C.nf (plus @Integer 100_000_000_000) 100_000_000_000,
      C.bench "Integer (.+.)" $ C.nf (plus' @Integer 100_000_000_000) 100_000_000_000,
      C.bench "Rational (.+.)" $ C.nf (plus' @Rational 100_000_000_000) 100_000_000_000,
      C.bench "Fraction (.+.)" $ C.nf (plus' @(Fraction Integer) 100_000_000_000) 100_000_000_000
    ]
