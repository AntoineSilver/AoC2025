# Day 5 -- Cafeteria

**Problem:** <https://adventofcode.com/2025/day/5>

## Part 1 -- Counting Elements in Ranges

### Problem Statement

For the first part of this day, we are given a text file with two parts
(separated by a blank line, i.e. two `\n` characters).

The first part is a list of ranges:

```text
3-5
10-14
16-20
12-18
```

And the second part is a list of integers:

```text
1
5
8
11
17
32
```

Or task is to count the number of integers (from the second list) that falls
into at least one of the ranges.

### Input Parsing

Since this day's input is _slightly_ more complex than the previous days, we can
use it as an opportunity to talk (briefly) about parsing in Haskell by building
a simple parser using the `Megaparsec` library.

> [!WARNING]
>
> Similarly to the previous day, I will not give a complete introduction to this
> library but instead demonstrate it on a simple example. To fully understand
> how monadic parsers such as `Megaparsec` work I'd highly recommend following a
> tutorial building one from the ground up.

#### Strings vs Text

In Haskell, strings are simply normal (linked) list objects who's entries are of
type `Char` (i.e. characters). While this is convenient since any function
defined on lists therefore also can be used for strings, it leads to poor
performance for many common text operations. We therefore will use the
`Data.Text` library instead.

Furthermore, to be able to still write strings with the familiar `"abc"` syntax,
we can use the `OverloadedStrings` language extension, which allows us to treat
`Text` (almost) like `String` objects.

#### Building a Range Parser

For the range, we can re-use the range type from [day 2](./Day02.md) but adding
an empty range for the second part of this exercise:

```haskell
{{#include ../../code/Day05.hs:range}}
```

> [!NOTE] Weak Head Normal Form
>
> Haskell is a lazy language, that means that objects are only evaluated when
> absolutely necessary. While this unlocks many super-powers (such as infinite
> lists), it can impact performance (see
> [docs](https://wiki.haskell.org/Performance/Strictness)).
>
> Here we impose the values inside the range to be at least partially evaluated
> (up to
> [weak head normal form](https://wiki.haskell.org/index.php?title=Weak_head_normal_form))
> via the exclamation mark.

In parsing libraries such as `Megaparsec`, it is common to have a `Parser a`
type, which is basically an object that is capable of processing text into an
object of type `a`. Here we use:

```haskell
{{#include ../../code/Day05.hs:parserType}}
```

To build the parser for the range type, we use
[do notation](https://en.wikibooks.org/wiki/Haskell/do_notation) to use an
imperative-ish style.

```haskell
{{#include ../../code/Day05.hs:rangeParser}}
```

> [!NOTE] Do Notation
>
> > [!TODO]
> >
> > This explanation might be a bit more complex..

#### Building the Full Input Parser

To build the full parser we then simply need to combine the range parser with
some other built-in parsers:

```haskell
{{#include ../../code/Day05.hs:inputParser}}
```

Which we then use to read the input file:

```haskell
{{#include ../../code/Day05.hs:readInput}}
```

Note that only the definition of the range parser and the input parser where
specific to this day, the rest is simply type declaration and some simple piping
code to make the parser read the file.

### Simple Solution

To solve the first part of this day, we first build some helper functions to
check if a given number is in a given range or in any range:

```haskell
{{#include ../../code/Day05.hs:isIn}}
```

> [!NOTE] Short-circuiting Evaluation and Folding
>
> In Haskell (and many other programming languages), certain operations (such as
> the logical operations, such as "or") are _short-circuiting_, i.e. for some
> values of the first argument (here for example `True`) the result of the
> operation is already fixed (for the case of `True || x` it is always `True`).
> In this case the second argument is not even computed.
>
> Here we use a `foldr` to allow the folding operation to short-circuit. To show
> how `foldr` is able to use short-circuiting but `foldl` is not, compare the
> following model implementations:
>
> ```haskell
> foldr f init (x:xs) = f init (foldr f x xs)
> foldl f init (x:xs) = foldl f (f init x) xs
> ```
>
> If `f` is short-circuiting for the current value of `init`, then `foldr` can
> already return without computing further terms but `foldl` still has to be
> called even if the result of `f init x` is again short-circuiting.
>
> Note that we could have also simply written:
>
> ```haskell
> isInAny i rs = any (isIn i) rs -- or: (i `isIn`) `any` rs
> ```
>
> But then we would not have had this discussion.

To obtain the result we then simply apply `isInAny` on all items and count the
number of `True`:

```haskell
{{#include ../../code/Day05.hs:solution1}}
```

> [!NOTE]
>
> Note that here `foldl'` is the better choice since it allows us to accumulate
> the result while we are traversing the list.

### Faster Solution

> [!TODO]
>
> I want to look into how feasible it is to pre-process the ranges for this
> task.

## Part 2 -- Counting All Elements in Ranges

### Problem Statement

For the second part of this problem, we only consider the range part of the
input and our goal is to count the total number of items in any range across all
ranges (i.e. the total size of all the ranges).

The tricky part about this is that the ranges we are given are intersecting. It
is therefore not enough to simply sum the size of each range, and we need to
make sure that we don't double count any elements.

### Dealing with Intersecting Ranges

When two ranges $[x_1, x_2]$ and $[y_1, y_2]$ intersect they can do so in four
different ways:

- $x$ is inside $y$ (i.e. $y_1 \leq x_1$ and $x_2 \leq y_2$)
- $y$ is inside $x$ (i.e. $x_1 \leq y_1$ and $y_2 \leq x_2$)
- the end of $x$ overlaps with the start of $y$ (i.e. $x_1 \leq y_1$ and $y_1
  \leq x_2 \leq y_2$)
- the end of $y$ overlaps with the start of $x$ (i.e. $y_1 \leq x_1$ and $x_1
  \leq y_2 \leq x_2$)

Our strategy will be based on cutting the elements form $x$ that are in $y$ so
that we can simply count the elements that remain in $x$ and then the elements
in $y$, for three of the above cases this results in $x$ remaining a range, but
if $y$ is strictly inside $x$, then $x$ is cut into two ranges.

We can avoid this by first ordering the ranges by increasing size, which ensures
that $y$ is larger than (or equal to) $x$ and therefore cannot be strictly
inside $x$.

The remaining cases lead to the following new ranges for $x$ (note that all are
smaller than the original $x$):

- if $x$ is inside $y$: $x' = []$ (empty)
- if the start of $x$ overlaps with $y$: $x' = [y_2 + 1, x_2]$
- if the end of $x$ overlaps with $y$: $x' = [x_1, y_1 - 1]$

In Haskell:

```haskell
{{#include ../../code/Day05.hs:cutRange}}
```

### Solution

We can apply this simplification on one element with respect to all the others
(assuming this first element is the smallest). Doing this from a sorted list of
ranges and always comparing one element to the following ones we get the desired
simplification and therefore also the total size:

```haskell
{{#include ../../code/Day05.hs:solution2}}
```
