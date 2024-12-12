-- | Provides the 'FromReal' and 'ToReal' typeclasses.
--
-- @since 0.1
module Numeric.Convert.Real
  ( Internal.FromReal (..),
    fromℝ,
    Internal.ToReal (..),
    toℝ,
  )
where

import GHC.Stack (HasCallStack)
import Numeric.Convert.Internal qualified as Internal

-- | Unicode alias for 'Internal.fromR', with U+211D.
--
-- @since 0.1
fromℝ :: (Internal.FromReal a, HasCallStack) => Double -> a
fromℝ = Internal.fromR

-- | Unicode alias for 'Internal.toR', with U+211D.
--
-- @since 0.1
toℝ :: (HasCallStack, Internal.ToReal a) => a -> Double
toℝ = Internal.toR
