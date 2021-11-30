-- | @algebra-simple@ endeavors to provide a simple, reasonably principled
-- interface to typical mathematical operations.
--
-- @since 0.1.0.0
module Algebra
  ( -- * Motivation
    -- $motivation

    -- * Solution
    -- $solution

    -- * Algebraic Typeclasses
    module Algebra.Additive,
    module Algebra.AdditiveMonoid,
    module Algebra.Group,
    module Algebra.Multiplicative,
    module Algebra.MultiplicativeMonoid,
    module Algebra.Semiring,
    module Algebra.Ring,
    module Algebra.Module,
    module Algebra.Field,
    module Algebra.VectorSpace,

    -- * Misc Typeclasses
    module Algebra.Literal,
  )
where

import Algebra.Additive
import Algebra.AdditiveMonoid
import Algebra.Field
import Algebra.Group
import Algebra.Literal
import Algebra.Module
import Algebra.Multiplicative
import Algebra.MultiplicativeMonoid
import Algebra.Ring
import Algebra.Semiring
import Algebra.VectorSpace

-- $motivation
-- The primary interface to numerical operations in Haskell is 'Num'.
-- Unfortunately, 'Num' has a key limitation: it is "too large". For example,
-- if we want to opt-in to addition, we must also opt-in to subtraction,
-- multiplication, and integer literal conversions. These may not make sense
-- for the type at hand (e.g. naturals), so we are stuck either providing an
-- invariant-breaking dangerous implementation (e.g. defining subtraction for
-- arbitrary naturals) or throwing runtime errors.

-- $solution
-- @algebra-simple@'s approach is to split this functionality into multiple
-- typeclasses, so types can opt-in to exactly as much functionality as they
-- want. The typeclasses are inspired by abstract algebra. The following table
-- lists the classes along with the num functionality they are intended to
-- replace:
--
-- +------------------------+-------------------------+----------+--------+-----------------------+
-- | Typeclass              | Description             | New      | 'Num'  | Example               |
-- +========================+=========================+==========+========+=======================+
-- | 'Additive'             | Types that              | '(.+.)'  | '(+)'  | 'Refined.Negative'    |
-- |                        | support "addition".     |          |        |                       |
-- +------------------------+-------------------------+----------+--------+-----------------------+
-- | 'AdditiveMonoid'       | 'Additive's that        | 'zero'   |        | 'Refined.NonPositive' |
-- |                        | have an identity.       |          |        |                       |
-- +------------------------+-------------------------+----------+--------+-----------------------+
-- | 'Multiplicative'       | Types that support      | '(.*.)'  | '(*)'  | 'Refined.Positive'    |
-- |                        | "multiplication".       |          |        |                       |
-- +------------------------+-------------------------+----------+--------+-----------------------+
-- | 'MultiplicativeMonoid' | 'Multiplicative's       | 'one'    |        | 'Refined.Positive'    |
-- |                        | that have an identity.  |          |        |                       |
-- +------------------------+-------------------------+----------+--------+-----------------------+
-- | 'Semiring'             | 'AdditiveMonoid' and    |          |        | 'Refined.NonNegative' |
-- |                        | 'MultiplicativeMonoid'. |          |        |                       |
-- +------------------------+-------------------------+----------+--------+-----------------------+
-- | 'Group'                | 'AdditiveMonoid's       | '(.-.)', | '(-)', | 'Refined.Integer'     |
-- |                        | that support            | 'ginv',  | 'abs'  |                       |
-- |                        | "subtraction".          | 'gabs'   |        |                       |
-- +------------------------+-------------------------+----------+--------+-----------------------+
-- | 'Ring'                 | 'Group' and             |          |        | 'Refined.Integer'     |
-- |                        | 'MultiplicativeMonoid'. |          |        |                       |
-- +------------------------+-------------------------+----------+--------+-----------------------+
-- | 'Field'                | 'Ring's that support    | '(.%.)'  | 'div', | 'Refined.Integer'     |
-- |                        | "division".             |          | '(/)'  |                       |
-- +------------------------+-------------------------+----------+--------+-----------------------+
-- | 'Module'               | 'Group's that supports  | '(.*)',  |        | @(,)@                 |
-- |                        | "scalar                 | '(*.)'   |        |                       |
-- |                        | multiplication".        |          |        |                       |
-- +------------------------+-------------------------+----------+--------+-----------------------+
-- | 'VectorSpace'          | 'Module's that supports | '(.%)',  |        | @(,)@                 |
-- |                        | "scalar division".      |          |        |                       |
-- +------------------------+-------------------------+----------+--------+-----------------------+
--
-- We have the following guiding principles:
--
-- 1. Simplicity
--
--     This is not a comprehensive implementation of abstract algebra, merely
--     the classes needed to replace the usual 'Num'-like functionality. For
--     the former, see [algebra](https://hackage.haskell.org/package/algebra).
--
-- 2. Practicality
--
--     When there is tension between practicality and theoretical "purity", we
--     favor the former. To wit:
--
--     * We provide two semigroup/monoid typeclasses:
--        'Additive'\/'AdditiveMonoid' and
--        'Multiplicative'\/'MultiplicativeMonoid'. Formally this is clunky,
--        but it allows us to:
--
--         * Reuse the same operator for ring multiplication and types that
--           have sensible multiplication but cannot be rings (e.g. naturals).
--
--         * Provide both addition and multiplication without an explosion of
--           newtype wrappers.
--
--     * Leniency vis-à-vis laws
--
--         For instance, integers cannot satisfy the field laws, and floats do
--         not satisfy anything, as their equality is nonsense. Nevertheless,
--         we provide instances for them. Working with technically unlawful
--         numerical instances is extremely common, so we take the stance that
--         it is better to provide such instances (albeit with known
--         limitations) than to forgo them completely (read: "integer division
--         is useful"). The only instances we disallow are those likely to
--         cause runtime errors (e.g. natural subtraction) or break expected
--         invariants.
--
--         One may wonder why e.g. we provide bounded integral addition
--         but disallow natural subtraction. The reason is due to practicality.
--         Generally, we do not want instances that can cause runtime
--         exceptions or nonsense via over/underflow. Removing bounded integral
--         addition would make this library near-useless, as these operations
--         are pervasive. By contrast, natural subtraction is less common
--         and -- in the eyes of the author -- more likely to go wrong.
--         Certainly this is debatable, but for now we consider this
--         a reasonable position.
--
--     * Division classes (i.e. 'Field', 'VectorSpace') have their own division
--       function that must be implemented. Theoretically this is unnecessary,
--       as we need only a function @inv :: NonZero a -> NonZero a@ and we
--       can then define division as @x .%. d = d .*. inv d@. But this will
--       not work for many types (e.g. integers), so we force users to define a
--       (presumably sensible) '(.%.)', so there is no chance of accidentally
--       using a nonsensical @inv@.
--
-- 3. Ergonomics
--
--      We choose new operators that do not clash with prelude.
--
-- We provide instances for built-in numeric types where it makes sense.
-- Furthermore, we define several "smart constructor" newtypes in "Algebra.Data"
-- that can be used in conjunction with these typeclasses for providing better
-- APIs.