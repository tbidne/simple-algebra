module Main (main) where

import System.Environment.Guard
  ( ExpectEnv (..),
    guardOrElse',
  )
import Test.DocTest qualified as DT

main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    (DT.doctest args)
    (putStrLn "*** Doctests Disabled ***")
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Numeric/Data/NonZero.hs",
    "src/Numeric/Algebra/Multiplicative/MGroup.hs"
  ]

exts :: [String]
exts =
  [ "-XNoStarIsType",
    "-XBangPatterns",
    "-XDataKinds",
    "-XDeriveDataTypeable",
    "-XDeriveGeneric",
    "-XDeriveLift",
    "-XDerivingStrategies",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XGeneralizedNewtypeDeriving",
    "-XFunctionalDependencies",
    "-XImportQualifiedPost",
    "-XMultiParamTypeClasses",
    "-XPatternSynonyms",
    "-XRankNTypes",
    "-XScopedTypeVariables",
    "-XStandaloneKindSignatures",
    "-XTypeApplications",
    "-XTypeFamilies",
    "-XTypeOperators"
  ]
