# Day 3 -- Lobby

**Problem:** <https://adventofcode.com/2025/day/3>

## Part 1 -- Largest 2 Sub-Number

> [!TODO]
>
> This day is very simple (at least compared to the previous one). I might use
> this as an opportunity to explain more of haskell it-self (recursion?).

### Problem Statement

We are given a list of numbers:

```text
987654321111111
811111111111119
234234234234278
818181911112111
```

Our task today is to form the largest digit number in each line by picking two
digits from the given number, e.g. in the first line here we could pick 9 and 8
to form 98. Note that we can not change the order of the digits! In the last
line from this test input we hence can not pick 8 and 9 to form 98.

> [!NOTE]
>
> Here we will always number the digits left to right i.e. in the order 12345...

### Solution

It is always optimal to choose the largest digit (excluding the last) for the
first digit. If there are multiple equally large digits, then choosing the
left-most one allows for a strictly larger set of options for the second digit.

The second digit is then simply the largest digit from all the digits to the
right of the first.

```haskell
{{#include ../../code/Day03.hs:max2subnumber}}
```

> [!NOTE]
>
> Here the input to `max2subnumber` is the list of digits in a given row.

### In- and Out-put

The IO stuff is pretty much identical to the previous days:

```haskell
{{#include ../../code/Day03.hs:in1}}

{{#include ../../code/Day03.hs:out1}}
```

## Part 2 - Largest 12 Sub-Number

For the second part of this problem, we want to again find the largest number,
this time using 12 digits.

It turns out that the above logic generalizes to any number of digits, we can
hence give a general solution which recursively picks the largest digit while
ensuring that enough digits remain:

```haskell
{{#include ../../code/Day03.hs:maxNsubnumber}}

{{#include ../../code/Day03.hs:out2}}
```
