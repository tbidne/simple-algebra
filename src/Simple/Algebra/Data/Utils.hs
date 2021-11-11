-- | Utility module for defining types following the "smart constructor"
-- pattern.
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
mkX :: (a -> Bool) -> (a -> b) -> a -> Maybe b
mkX p cons x
  | p x = Just $ cons x
  | otherwise = Nothing

-- | Like 'mkX', except dies with 'error' at runtime. Intended to
-- be used with known constants. Exercise restraint!
unsafeX :: String -> (a -> Bool) -> (a -> b) -> a -> b
unsafeX errMsg p cons = M.fromMaybe (error errMsg) . mkX p cons

-- | Combination of 'TR.readMaybe' and 'mkX'.
readX :: (Read a) => (a -> Bool) -> (a -> b) -> String -> Maybe b
readX p cons = TR.readMaybe >=> mkX p cons
