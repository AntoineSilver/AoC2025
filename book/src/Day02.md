# Day 2 -- Gift Shop

**Problem:** <https://adventofcode.com/2025/day/2>

## Part 1 -- Finding Doubled Sequences of Digits

### Problem Statement

We are given a list of ranges (separated by `,`, line breaks here only for
convenience):

```text
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124
```

Our task is to find the all the numbers in these ranges made by a digit pattern
of the form `abc..abc..` (e.g. 1010, 123123 or even 111111), which we call a
2-repetition. The final result is the sum of all these numbers.

### Data Structures and Input Parsing

Just as we did for [day 1](./Day01.md), we start by forming a fitting type to
represent ranges:

```haskell
{{#include ../../code/Day02.hs:types}}
```

And we also use a similar code to read the input file (again, this can fail
ugly, but we assume that the input is well formatted):

```haskell
{{#include ../../code/Day02.hs:read}}
```

### A Bit of Math

Before brute forcing this problem, let us spend some time thinking a bit of the
math behind this problem.

If we have a number of the form $ abc\cdots abc\cdots$ with $2k$ digits (note
that the number of digits has to be even), then we can also write it as
$(10^{k} + 1) \times n$ where $n = abc\cdots$. In other words, **we want to find
numbers with $2k$ digits which are multiples of $10^k + 1$**.

Let us now assume we are given a range $⟦a,b⟧$ where $a$ and $b$ have $2k$
digits. We can perform the integer division of both $a$ and $b$:

$$
  a = (10^k + 1) \times n_1 + r_1
  \qquad\qquad
  b = (10^k + 1) \times n_2 + r_2
$$

The desired numbers between $a$ and $b$ hence take the form $ (10^k + 1) \times
n $ with $n$ smaller or equal to $n_2$ and larger (or equal if $r_1 = 0$) than
$n_1$. Hence the sum is:

$$
  S(⟦a,b⟧) = \sum_{n = n_1'}^{n_2} (10^k + 1) n = \frac{1}{2}  (10^k + 1) (n_2 - n_1' + 1)(n_2 + n_1')
$$

where $n_1' = n_1$ if $r_1 = 0$ and $n_1' = n_1 + 1$ otherwise.

If $a$ and $b$ have different numbers of digits ($i$ and $j$ respectively), then
we can decompose the interval into sections which have the same number of
digits:

$$
  ⟦a,b⟧ = ⟦a, 10^{i}-1⟧ \cup ⟦10^{i}, 10^{i+1}-1⟧ \cup \cdots \cup ⟦10^{j-1}, b⟧
$$

### Solution

Let us first build the logic to decompose ranges:

```haskell
{{#include ../../code/Day02.hs:decomposeRange}}
```

Then, on each of these ranges we apply the formula:

```haskell
{{#include ../../code/Day02.hs:sum2Repetitions}}
```

Note that we already also made a method to sum all 2-repetitions from a list of
ranges which we can use to obtain the solution to the problem:

```haskell
{{#include ../../code/Day02.hs:out1}}
```

> [!NOTE]
>
> The method `sum2Repetitions_fromList` will count numbers twice if they appear
> in different ranges (e.g.
> `sum2Repetitions_fromList [Range 10 12, Range 10 12]` will return `22`). In
> this exercise this is not a problem since the input data is made of disjoint
> ranges.

## Part 2 -- Finding n-Times Repetitions

### Problem Statement and Some More Math

We now want to sum all the numbers with $n$-times repetitions, i.e. patterns
like $ababab_{10}$.

> [!DEFINITION]
>
> An $(r,k)$-repetition is a number formed by a digit pattern of $r$ blocks of
> $k$ digits. E.g. $123123$ is a $(2,3)$-repetition. The set of all
> $(r,k)$-repetitions is written $\mathcal{R}(r,k)$.

Naturally, a $(r,k)$-repetition has $r \times k$ digits, hence if the number of
digits is fixed, we may omit $k$ from our notation.

By adapting the above formula for $(2,k)$-repetitions, we can sum all
$(r,k)$-repetitions between $a$ and $b$ (with $rk$ digits):

$$
  S_{r}(\llbracket a, b \rrbracket)
  = \sum_{n=n_1'}^{n_2} \left( \sum_{i=0}^{r-1} 10^{k i} \right) n
  = \frac{10^{kr} -1}{10^k - 1} \frac{(n_2 - n_1' + 1) (n_1' + n_2)}{2}
$$

In haskell:

```haskell
{{#include ../../code/Day02.hs:sumRRepetitions}}
```

We may now be tempted to simply consider all allowed values of $r$ and sum them
together (e.g. for six digit numbers, we would consider $r=2$, $r=3$ and $r=6$).
This would however be incorrect as we would count multiple numbers twice! For
example $222222$ is both a $r=2$ and $r=3$ repetition (and even an $r=6$
repetition)!

### Avoiding Double Counting

The problem comes (in part) from the fact that a $(r,k)$-repetition is also a
$(r/d, d k)$-repetition (where $d$ divides $r$):

$$
  \mathcal{R}(r,k) \subseteq \mathcal{R}(r/d, d k)
$$

We can fix this by only considering $(r,k)$-repetitions where $r$ is prime
(repetitions with $r=1$ are trivial and not counted here). However, this still
double count some numbers, such as $222222$, which is both a $(2,3)$ and a
$(3,2)$ repetition. To solve this we can use the following theorem:

> [!THEOREM]
>
> Let $r_1$ and $r_2$ be coprime, then:
>
> $$
>   \mathcal{R}(r_1 r_2, k) = \mathcal{R}(r_1, r_2 k) \cap \mathcal{R}(r_2, r_1
>   k)
> $$

> [!PROOF]
>
> - $\mathcal{R}(r_1 r_2, k) \subseteq \mathcal{R}(r_1, r_2 k)$ and
>   $\mathcal{R}(r_1 r_2, k) \subseteq \mathcal{R}(r_2, r_1 k)$ by the above
>   property.
> - If a number $x$ is both a $(r_1, r_2 k)$-repetition and a $(r_2, r_1
>   k)$-repetition, then its digits can be decomposed into $r_1$ and $r_2$
>   blocks. This is only possible if it can be decomposed into $r'$ blocks where
>   $r'$ is a multiple of both $r_1$ and $r_2$. Since $r_1$ and $r_2$ are
>   coprime, $r'$ is a multiple of $r_1 r_2$.

For six digits, this gives us the following formula:

$$
  \text{result} = S_2 + S_3 - S_6
$$

where we used the theorem to remove the double counting of $(6,1)$-repetitions.

For the general case of $n$ digits, we consider all the prime numbers $p$
dividing $n$ and use the
[inclusion-exclusion principle](https://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle)
(short IEP) to remove the double counted numbers. I.e.:

$$
  S =
  \sum_{p_1 \in P} S_{p_1}
  - \sum_{p_1, p_2 \in P} S_{p_1 p_2}
  + \sum_{p_1, p_2, p_3 \in P} S_{p_1 p_2 p_3}
  - \sum_{p_1, p_2, p_3, p_4 \in P} S_{p_1 p_2 p_3 p_4}
  + \cdots
$$

where $P$ is the set of prime factors of $n$ and $p_i$ and $p_j$ are assumed to
be different if $i \neq j$.

### Implementation

We first need two helper functions:

- `primeFactors` to get a list of all the prime factors of the number of digits
- `bitString` to get all the bit-strings of a given length for the IEP

```haskell
{{#include ../../code/Day02.hs:helper}}
```

With these helpers, we use the IEP on $r$ prime:

```haskell
{{#include ../../code/Day02.hs:sumAllRepetitions}}
```

Which we can then simply output:

```haskell
{{#include ../../code/Day02.hs:out1}}
```
