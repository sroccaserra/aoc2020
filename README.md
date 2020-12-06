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

```
$ stack test --file-watch
```

## Repl

```
$ stack ghci src/DayXX.hs
```

## Learnings

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
[br]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:break
[cc]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:concat
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

## References

- <https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners>
- <https://github.com/Morendil/AdventOfCode2019>
- <https://ravichugh.gitbooks.io/a-quarter-of-haskell/content/parsing/more.html>
