-- | Entrypoint for property tests.
--
-- @since 0.1.0.0
module Main (main) where

import Control.Monad qualified as M
import Data.Proxy (Proxy (..))
import MaxRuns (MaxRuns (..))
import System.Environment qualified as Env
import System.Exit qualified as SysEx
import Test.Additive.AGroup qualified
import Test.Additive.AMonoid qualified
import Test.Additive.ASemigroup qualified
import Test.Multiplicative.MGroup qualified
import Test.Multiplicative.MMonoid qualified
import Test.Multiplicative.MSemigroup qualified
import Test.Tasty qualified as Tasty
import Test.Tasty.Options (OptionDescription (..))
import Text.Read qualified as TR

-- | Runs property tests. The environment variable @MAX_RUNS@ controls
-- how many test runs we do (default 100).
--
-- @since 0.1.0.0
main :: IO ()
main = do
  maxRuns <-
    Env.lookupEnv "MAX_RUNS" >>= \case
      Nothing -> pure 100
      Just mr -> case parseMaxRuns mr of
        Nothing -> SysEx.die $ "*** MAX_RUNS is not a non-negative integer: " <> mr
        Just x -> pure $ fromIntegral @Int x

  let maxRunProps =
        Tasty.localOption (MkMaxRuns maxRuns)
          <$> [ Test.Additive.ASemigroup.props,
                Test.Additive.AMonoid.props,
                Test.Additive.AGroup.props,
                Test.Multiplicative.MSemigroup.props,
                Test.Multiplicative.MMonoid.props,
                Test.Multiplicative.MGroup.props
              ]

  Tasty.defaultMainWithIngredients ingredients $
    Tasty.testGroup
      "Property tests"
      maxRunProps
  where
    parseMaxRuns = M.mfilter (> 0) . TR.readMaybe
    ingredients = Tasty.includingOptions [Option @MaxRuns Proxy] : Tasty.defaultIngredients
