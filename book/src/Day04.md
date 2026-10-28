# Day 4 -- Printing Department

**Problem:** <https://adventofcode.com/2025/day/4>

## Part 1 -- Counting Accessible Paper Rolls

### Problem Statement

The input to this problem is a 2D grid of cells which can be either occupied (by
a paper roll) or empty. In the input text, the empty cells are represented by
dots (`.`) and the occupied ones by "at" symbols (`@`):

```text
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
```

Our first task is to count the number of paper rolls that are accessible, i.e.
occupied cells with fewer than four surrounding paper rolls (including
diagonals). The outer edge is considered unoccupied.

### Types

To represent this grid in haskell, we will use the `massiv` package:

```haskell
import Data.Massiv.Array
```

> [!WARNING]
>
> Since I am my-self very unfamiliar with high-performance haskell in general
> and `massiv` in particular, take the code presented here with a grain of salt!
>
> This day is more of an excuse for me to learn about these aspects of haskell.

In principle, we would love to use a sum type to represent the state of the
cells (e.g. something along the lines of `type State = Empty | Occupied`).
However, it turns out that `massiv` has better performance when using `Prim`
types (which are types with specific representation in memory). Examples of
`Prim` types include `Int`, `Float` or `Char`. In our case we will use `Word8`,
which is the haskell type for unsigned 8-bit integers.

However, we can actually keep a very similar interface to our ideal sum type by
using
[pattern synonyms](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html)
(this is inspired by
[this tutorial](https://www.mlabs.city/blog/our-performance-is-massiv)):

```haskell
{{#include ../../code/Day04.hs:cellState}}
```

> [!NOTE] Language Extensions Required
>
> The above code requires two language extensions:
>
> - `DerivingVia` to derive `Eq` and `Prim` directly from their `Word8`
>   implementation (see
>   [docs](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html))
> - `PatternSynonyms` to pattern-match with `Empty` and `Full` (see
>   [docs](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html))
>
> We also use the `COMPLETE` pragma to indicate that any pattern matching that
> covers `Full` and `Empty` has no missing case (the other cases, e.g.
> `CellState 2` are invalid and should never be produced in the first place).

This allows us to express the state of the automaton as a `massiv` array of type
`P` (only available to `Prim` types, which our cell state is):

```haskell
{{#include ../../code/Day04.hs:board}}
```

This type means that a board (i.e. the state of the cellular automaton) is a 2D
array of type `P` containing `CellState` (which are treated internally like
`Word8`).

Reading the input is not much harder than it has been for the previous days:

```haskell
{{#include ../../code/Day04.hs:readInput}}
```

> [!TODO]
>
> This code is rather slow since it is manipulating haskell strings, and it
> would be possible to make it faster via `Data.Text`. I think I might have a
> day when parsing can be a bit nicer to introduce. For now this is probably
> good enough.

The only subtlety in the above code is the creation of the array via the
`makeArray` function. This function takes three arguments:

- the computational strategy, commonly `Seq` (sequential) or `Par` (parallel)
- the size, which has almost the same type as an index but wrapped by `Sz`
- a map from index to value

Note that the type for index (here `Ix2 Int Int = Int :. Int`, similar to
`(Int, Int)`) also informs the dimension of the array (here a 2D array).

### Stencils

Stencils are one of the very powerful features of `massiv`. They allow us to
build functions that modify arrays entry by entry based on their surrounding.

A stencil is created via the `makeStencil` function which takes three arguments
([docs](https://hackage-content.haskell.org/package/massiv-1.0.5.0/docs/Data-Massiv-Array-Stencil.html)):

- a size argument (the size of the neighborhood array, here we always want the
  [Moore neighborhood](https://en.wikipedia.org/wiki/Moore_neighborhood) so we
  use `Sz (3 :. 3)`)
- a center argument (which index in the neighborhood array is the target, so
  here `(1 :. 1)`)
- a function that computes the updated value of the target via a `get` function
  which allows it to read the values of the neighborhood.

Here we create two different stencils, one that produces boolean values based on
if the cell is full and freeable and another that frees freeable cells:

```haskell
{{#include ../../code/Day04.hs:stencils}}
```

### Solution

To apply these stencils we can either use `applyStencil` or `mapStencil`. The
main difference between these two methods is the way the border is treated
([docs](https://hackage-content.haskell.org/package/massiv-1.0.5.0/docs/Data-Massiv-Array-Stencil.html)).
Here we only need `mapStencil`, which forces the new array to be of the exact
same shape as the original one. We also need to specify how cells outside of the
grid need to be handled, which we do here by setting them all to `Empty`. Form
this new array we can then obtain the result by counting all the `True` cells
via a fold operation.

```haskell
{{#include ../../code/Day04.hs:freeable}}

{{#include ../../code/Day04.hs:solution1}}
```

> [!NOTE] Delayed Arrays
>
> Note that the array produced by `mapStencil` is 'delayed', meaning it is not
> yet computed. This allows `massiv` to stack multiple operations and only loop
> over the array once. To transform a delayed array into a manifest one, we need
> to use the `compute` function and specify a desired array type (here
> `Array U Ix2 Bool`, which is an unboxed two-dimensional array containing
> booleans).

## Part 2 -- Counting _All_ Accessible Paper Rolls

For the second part of this day, we need to count not just the first round of
freeable cells but also the cells that are freeable after these are removed,
until no freeable cells remain. We do this via a simple recursion:

```haskell
{{#include ../../code/Day04.hs:allFreeable}}

{{#include ../../code/Day04.hs:solution2}}
```
