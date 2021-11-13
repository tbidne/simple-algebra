-- | Utility module for defining types following the "smart constructor"
-- pattern.
--
-- @since 0.1.0.0
module Simple.Algebra.Data.Utils
  ( mkX,
    unsafeX,
    readX,
  )
where

import Control.Monad ((>=>))
import Data.Maybe qualified as M
import Text.Read qualified as TR

-- | Potentially creates a type, conditioned on the the type passing
-- the predicate.
--
-- @since 0.1.0.0
mkX :: (a -> Bool) -> (a -> b) -> a -> Maybe b
mkX p cons x
  | p x = Just $ cons x
  | otherwise = Nothing

-- | Like 'mkX', except dies with 'error' at runtime. Intended to
-- be used with known constants. Exercise restraint!
--
-- @since 0.1.0.0
unsafeX :: String -> (a -> Bool) -> (a -> b) -> a -> b
unsafeX errMsg p cons = M.fromMaybe (error errMsg) . mkX p cons

-- | Combination of 'TR.readMaybe' and 'mkX'.
--
-- @since 0.1.0.0
readX :: (Read a) => (a -> Bool) -> (a -> b) -> String -> Maybe b
readX p cons = TR.readMaybe >=> mkX p cons
