# Advent of Code 2025

This repo contains my solutions to the advent of code 2025 [see
here](https://adventofcode.com/2025/).

The code is written in a literate programming style using Emacs `.org` files
which tangle (in a reversible manner) into code files in the `/code` folder
(these should be run from a REPL such as GHCi).

Furthermore, I added a small bash script that downloads the input files
automatically with a session cookie given in the `.env` file (shhh, it's
secret). You can also add your own inputs by adding them directly into the
`/input` folder, the format is `Day$n.txt` where `$n` is the number of the day
(with leading zeros if `$n` is less then 10).

I think I will write most of this using Haskell, since I rarely have an
opportunity to use it otherwise.


