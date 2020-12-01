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
$ echo -e "1\n2\n3" | stack runhaskell -- src/DayXX
1
```

## Tests

```
$ stack runhaskell -- src/DayXXSpec
```
