-- | Provides the 'FromInteger' and 'ToInteger' typeclasses.
--
-- @since 0.1
module Numeric.Convert.Integer
  ( Internal.FromInteger (..),
    fromℤ,
    Internal.ToInteger (..),
    toℤ,
  )
where

import GHC.Stack (HasCallStack)
import Numeric.Convert.Internal qualified as Internal

-- | Unicode alias for 'Internal.fromZ', with U+2114.
--
-- @since 0.1
fromℤ :: (Internal.FromInteger a, HasCallStack) => Integer -> a
fromℤ = Internal.fromZ

-- | Unicode alias for 'Internal.toZ', with U+2114.
--
-- @since 0.1
toℤ :: (HasCallStack, Internal.ToInteger a) => a -> Integer
toℤ = Internal.toZ
