# aoc2020

Code for the advent of code 2020 event:

- <https://adventofcode.com/2020>

## Setup

How setup was done:

```
$ stack new aoc2020
$ cd aoc2020
$ git init
$ git add .
$ git commit
```

## Running

```
$ echo 1 2 3 | stack runhaskell -- src/Day01
[[1,2,3]]
```

## Tests

```
$ stack runhaskell -- src/Day01Spec
```
