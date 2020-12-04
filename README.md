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

- With [mapM_][mm] I can apply print to a list of results:

```haskell
main = do
  xs <- fmap lines getContents
  mapM_ print $ map parseLine xs
```

- The [interact][in] function passes the input from stdin to a `String -> String` function and prints the result to stdout
- The [cycle][cy] function turns a finite list to a cycling infinite one
- The [mapMaybe][mm] function (`(a -> Maybe b) -> [a] -> [b]`)
- The [fromEnum][fe] function, used to convert `Bool` to `Int` in day 03
- The [fromMaybe][fm] function (`a -> Maybe a -> a`)
- The [ReadP][rp] module & how it works (see refs about parser combinators below)
- The [Regex.TDFA][re] module to use regexes in Haskell (see commit [f38935c][rrc])
- The [&][&] operator, works like a pipe operator
- The `do` notation can be used as a [list comprehension][lc]

[mm]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:mapM_
[in]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:interact
[cy]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:cycle
[mm]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Maybe.html#v:mapMaybe
[fe]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:fromEnum
[fm]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Maybe.html#v:fromMaybe
[rp]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Text-ParserCombinators-ReadP.html
[re]: https://hackage.haskell.org/package/regex-tdfa-1.3.1.0/docs/Text-Regex-TDFA.html
[lc]: https://wiki.haskell.org/List_comprehension
[&]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Function.html#v:-38-
[rrc]: https://github.com/sroccaserra/aoc2020/commit/f38935c

## References

- <https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners>
- <https://github.com/Morendil/AdventOfCode2019>
- <https://ravichugh.gitbooks.io/a-quarter-of-haskell/content/parsing/more.html>
