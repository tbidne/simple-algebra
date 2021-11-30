<div align="center">

# Algebra Simple

![cabal](https://github.com/tbidne/algebra-simple/workflows/cabal/badge.svg?branch=main)
![stack](https://github.com/tbidne/algebra-simple/workflows/stack/badge.svg?branch=main)
![nix](https://github.com/tbidne/algebra-simple/workflows/nix/badge.svg?branch=main)
![haddock](https://github.com/tbidne/algebra-simple/workflows/haddock/badge.svg?branch=main)
![style](https://github.com/tbidne/algebra-simple/workflows/style/badge.svg?branch=main)

</div>


# Motivation

The primary interface to numerical operations in Haskell is `Num`. Unfortunately, `Num` has a key limitation: it is "too large". For example, if we want to opt-in to addition, we must also opt-in to subtraction, multiplication, and integer literal conversions. These may not make sense for the type at hand (e.g. naturals), so we are stuck either providing an
invariant-breaking dangerous implementation (e.g. defining subtraction for arbitrary naturals) or throwing runtime errors.

# Solution

`algebra-simple`'s approach is to split this functionality into multiple typeclasses, so types can opt-in to exactly as much functionality as they want. The typeclasses are inspired by abstract algebra. The following table lists the classes along with the num functionality they are intended to replace:

<table>
  <thead>
    <th>Typeclass</th>
    <th>Description</th>
    <th>New</th>
    <th>Num</th>
    <th>Example</th>
  </thead>
  <tr>
    <td><code>Additive</code></td>
    <td>Types that support "addition".</td>
    <td><code>(.+.)</code></td>
    <td><code>(+)</code></td>
    <td><code>Negative</code></td>
  </tr>
  <tr>
    <td><code>AdditiveMonoid</code></td>
    <td><code>Additive</code>s that have an identity.</td>
    <td><code>zero</code></td>
    <td></td>
    <td><code>NonPositive</code></td>
  </tr>
  <tr>
    <td><code>Multiplicative</code></td>
    <td>Types that support "multiplication".</td>
    <td><code>(.*.)</code></td>
    <td><code>(*)</code></td>
    <td><code>Positive</code></td>
  </tr>
  <tr>
    <td><code>MultiplicativeMonoid</code></td>
    <td><code>Multiplicative</code>s that have an identity.</td>
    <td><code>one</code></td>
    <td></td>
    <td><code>Positive</code></td>
  </tr>
  <tr>
    <td><code>Semiring</code></td>
    <td><code>AdditiveMonoid</code> and <code>MultiplicativeMonoid</code></td>
    <td></td>
    <td></td>
    <td><code>NonNegative</code></td>
  </tr>
  <tr>
    <td><code>Group</code></td>
    <td><code>AdditiveMonoid</code>s that support "subtraction".</td>
    <td><code>(.-.)</code>, <code>ginv</code>, <code>gabs</code></td>
    <td><code>(-)</code>, <code>abs</code></td>
    <td><code>Integer</code></td>
  </tr>
  <tr>
    <td><code>Ring</code></td>
    <td><code>Group</code> and <code>MultiplicativeMonoid</code></td>
    <td></td>
    <td></td>
    <td><code>Integer</code></td>
  </tr>
  <tr>
    <td><code>Field</code></td>
    <td><code>Ring</code>s that support "division".</td>
    <td><code>(.%.)</code></td>
    <td><code>div</code>, <code>(/)</code></td>
    <td><code>Integer</code></td>
  </tr>
  <tr>
    <td><code>Module</code></td>
    <td><code>Group</code>s that support "scalar multiplication".</td>
    <td><code>(.*)</code>, <code>(*.)</code></td>
    <td></td>
    <td><code>(,)</code></td>
  </tr>
  <tr>
    <td><code>VectorSpace</code></td>
    <td><code>Module</code>s that support "scalar division".</td>
    <td><code>(.%)</code></td>
    <td></td>
    <td><code>(,)</code></td>
  </tr>
</table>

We have the following guiding principles:

1. Simplicity

    This is not a comprehensive implementation of abstract algebra, merely the classes needed to replace the usual `Num`-like functionality. For the former, see [algebra](https://hackage.haskell.org/package/algebra).

2. Practicality

    When there is tension between practicality and theoretical "purity", we favor the former. To wit:

    * We provide two semigroup/monoid typeclasses:
       `Additive`/`AdditiveMonoid` and
       `Multiplicative`/`MultiplicativeMonoid`. Formally this is clunky, but it allows us to:

        * Reuse the same operator for ring multiplication and types that have sensible multiplication but cannot be rings (e.g. naturals).

        * Provide both addition and multiplication without an explosion of newtype wrappers.

    * Leniency vis-à-vis laws

        For instance, integers cannot satisfy the field laws, and floats do not satisfy anything, as their equality is nonsense. Nevertheless, we provide instances for them. Working with technically unlawful numerical instances is extremely common, so we take the stance that it is better to provide such instances (albeit with known limitations) than to forgo them completely (read: integer division is useful). The only instances we disallow are those likely to cause runtime errors (e.g. natural subtraction) or break expected invariants.

        One may wonder why e.g. we provide bounded integral addition but disallow natural subtraction. The reason is due to practicality. Generally, we do not want instances that can cause runtime exceptions or nonsense via over/underflow. Removing bounded integral addition would make this library near-useless, as these operations are pervasive. By contrast, natural subtraction is less common and in the eyes of the author more likely to go wrong. Certainly this is debatable, but for now we consider this a reasonable position.

    * Division classes (i.e. `Field`, `VectorSpace`) have their own division function that must be implemented. Theoretically this is unnecessary, as we need only a function `inv :: NonZero a -> NonZero a` and we can then define division as `x .%. d = x .*. inv d`. But this will not work for many types (e.g. integers), so we force users to define a (presumably sensible) `(.%.)`, so there is no chance of accidentally using a nonsensical `inv`.

3. Ergonomics

     We choose new operators that do not clash with prelude.

We provide instances for built-in numeric types where it makes sense. Furthermore, we define instances for `refined-simple` types.