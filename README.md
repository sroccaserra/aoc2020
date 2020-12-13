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
$ echo -e "1: 2,3\n2: 3,4\n3: 5,6" | stack runhaskell -- src/DayXX
[1,2,3]
```

## Tests

Run a single test:

```
$ stack runhaskell -- -isrc test/DayXXSpec
```

Run all tests:

```
$ stack test
```

## Building

```
$ stack ghc -- -O2 -main-is DayXX src/DayXX.hs
```

## Repl

```
$ stack ghci src/DayXX.hs
```

## Learnings

- I should read and learn [this theorem][cr], it was apparently useful for day 13
- Reading from `stdin` allows to input lines manually (or paste short examples from the puzzle text) and end by Ctrl+D
- `Data.Complex` is handy for 2D operations (regular addition translates, `(* (0+:1))` rotates 90 °, `(* -(0:+1))` rotates -90 °)
- `Data.Maybe.catMaybes` turns a list of `Maybe a` into a list of `a`
- If I declare both a `main = hspec spec` and a `spec` functions in a test file, I can run them either individually or all.
- For Int indexed values, [Vector][ve] seems more useful than [Array][ar]
- A Text is not a List, like a String is: you can't use Data.List functions on a Text.
- How to print & debug: <https://wiki.haskell.org/Debugging>
- Use [nub][nu] to remove duplicates in a list (no need to sort, but beware n²)
- Use [concat][cc] instead of `foldl1 (++)`
- Instead of `foldl max 0 xs` I can use `foldl1 max xs` and even better, `maximum xs`.
- If I have a list of `0`s and `1`s, I can convert them to decimal numbers with `foldl1' $ (+) . (*2)`
- With [span][sp] or [break][br] I can split a list with a predicate (see also [takeWhile][tw] and [dropWhile][dw])
- With [lookup][lu] I can lookup in a dumb association structure (`a -> [(a, b)] -> Maybe b`), not unlike Lisp's [association lists][al]
- With [mapM_][mm] I can apply print to a list of results:

```haskell
main = do
  xs <- fmap lines getContents
  mapM_ print $ map parseLine xs
```

- The [interact][in] function passes the input from stdin to a `String -> String` function and prints the result to stdout
- The [cycle][cy] function turns a finite list to a cycling infinite one
- The [mapMaybe][ma] function (`(a -> Maybe b) -> [a] -> [b]`)
- The [fromEnum][fe] function, used to convert `Bool` to `Int` in day 03
- The [fromMaybe][fm] function (`a -> Maybe a -> a`)
- The [ReadP][rp] module & how it works (see refs about parser combinators below)
- The [Regex.TDFA][re] module to use regexes in Haskell (see commit [f38935c][rrc])
- The [&][&] operator, works like a pipe operator
- The `do` notation can be used as a [list comprehension][lc]

[&]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Function.html#v:-38-
[al]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html
[ar]: https://hackage.haskell.org/package/array
[br]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:break
[cc]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:concat
[cr]: https://en.wikipedia.org/wiki/Chinese_remainder_theorem
[cy]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:cycle
[dw]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:dropWhile
[fe]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:fromEnum
[fm]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Maybe.html#v:fromMaybe
[in]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:interact
[lc]: https://wiki.haskell.org/List_comprehension
[lu]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:lookup
[ma]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Maybe.html#v:mapMaybe
[mm]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:mapM_
[nu]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:nub
[re]: https://hackage.haskell.org/package/regex-tdfa-1.3.1.0/docs/Text-Regex-TDFA.html
[rp]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Text-ParserCombinators-ReadP.html
[rrc]: https://github.com/sroccaserra/aoc2020/commit/f38935c
[sp]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:span
[tw]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:takeWhile
[ve]: https://hackage.haskell.org/package/vector-0.12.1.2

## References

- <https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners>
- <https://github.com/Morendil/AdventOfCode2019>
- <https://ravichugh.gitbooks.io/a-quarter-of-haskell/content/parsing/more.html>
