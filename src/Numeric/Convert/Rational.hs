-- | Provides the 'FromRational' and 'ToRational typeclasses.
--
-- @since 0.1
module Numeric.Convert.Rational
  ( Internal.FromRational (..),
    fromℚ,
    Internal.ToRational (..),
    toℚ,
  )
where

import GHC.Stack (HasCallStack)
import Numeric.Convert.Internal qualified as Internal

-- | Unicode alias for 'Internal.fromQ', with U+211A.
--
-- @since 0.1
fromℚ :: (Internal.FromRational a, HasCallStack) => Rational -> a
fromℚ = Internal.fromQ

-- | Unicode alias for 'Internal.toQ', with U+211A.
--
-- @since 0.1
toℚ :: (HasCallStack, Internal.ToRational a) => a -> Rational
toℚ = Internal.toQ
