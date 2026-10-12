# Day 1 -- Secret Entrance

**Problem:** <https://adventofcode.com/2025/day/1>

## Part 1 -- Counting Zeros

### Problem Statement

For the first day of this advent of code we are given a list of rotations of a
safe dial:

```text
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
```

Our (first) task is to count the number of times the dial is at the position "0"
(it starts at the position 50).

### Data Structures and Input Parsing

Haskell shines brightest when we give it good (expressive and descriptive)
types, so let's start with that:

```haskell
{{#include ../../code/Day01.hs:types}}
```

To read the input file we use some quick and dirty `read` magic. (This can crash
quite badly if the text is not formatted as expected, but for our purpose this
is ok.)

```haskell
{{#include ../../code/Day01.hs:read}}
```

### Solution

Naturally, the rotations can act on the positions by either adding or
subtracting the corresponding angle to the position:

```haskell
{{#include ../../code/Day01.hs:actions}}
```

> [!NOTE]
>
> The dial goes from 0 to 100, looping back to 0 if we exceed 100 (or go below
> 0). "Valid" positions should hence only go from 0 to 100. Therefore, we also
> add a `rotateMod` function to get the correct position after removing these
> cycles.

From these functions we can turn a list of rotations into a list of positions
(provided we have a starting position, here always 50) via a scan operation and
by simply counting the number of times that the list contains 0 we have the
solution!

```haskell
{{#include ../../code/Day01.hs:getpos}}
```

> [!NOTE]
>
> Since the scan expects a step function of the form `a -> b -> a`, but our
> function is of the form `b -> a -> a`, we use a `flip` which permutes the two
> inputs of the function.

Composing everything together:

```haskell
{{#include ../../code/Day01.hs:out1}}
```

## Part 2 -- Counting Clicks

For the second part of this problem, we don't just want to get the number of
times the rotation stops at 0 but the total number of times the dial crosses 0.

To solve this problem, we need a slightly different step function than the above
`rotate` and `rotateMod` that also increments a counter for the number of times
we have passed 0 during the rotation.

```haskell
{{#include ../../code/Day01.hs:step}}
```

> [!NOTE]
>
> Here we need to take some care into the fact that we can have rotations of
> more the 100. Furthermore, negative rotations (i.e. left rotations) are a bit
> tricky since if the number becomes negative, we have already crossed 0 once,
> but if we start at 0 we do not cross it again.

Finally, we fold this step across the list of rotation with initial value
`(50,0)` (position 50, no clicks):

```haskell
{{#include ../../code/Day01.hs:countClicks}}
```

Using the new method to count clicks, getting the final answer is simple:

```haskell
{{#include ../../code/Day01.hs:out2}}
```

## Extra -- Groups

TODO
