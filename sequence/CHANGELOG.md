# Changelog

## 0.5

- conversion with `klist`
- add monadic, choice and applicative infix operators and `>|=`
- add several functions:
  * `product2`
  * `find`, `mem`
  * `doubleton`, `cons`, `snoc`
  * `drop_while`, `take_while`...
  * `concat_str`
- aliases to existing functions
- use `delimcc` in a new module, `SequenceInvert`, in order to reverse the
  control flow (here with conversion to Gen)
- fix examples, tests and doc (about `product`)
- reading benchmark for persistent sequences.
- replace `Bench` with `Benchmark`

## 0.4.1

- `persistent_lazy`
- use bin_annot

## 0.4

- API change for `persistent`
- more efficient implementation for `persistent`
- remove `TypeClass`
- API change for `min`/`max` (in case the sequence is empty)
- conversion with `Gen`
- use Oasis

## 0.3.7

- decreasing int range
- printing functions

## 0.3.6.1

- documentation
- bugfixes

## 0.3.6

- `fmap`
- functors to adapt `Set` and `Map`

## 0.3.5

- tests and benchmarks
- `join` combinator
- optimization for `Sequence.persistent`

## 0.3.4

- `sort`, `uniq`, `group` and `sort_uniq` combinators implemented
- some conversion functions that use `Sequence.t2`
- infix operators in `Sequence.Infix`
- `Sequence.t2` type for efficient iteration on pairs of elements
- some combinators are adapted to `Sequence.t2`
- `zip`, `unzip` and `zip_i` to convert between `t` and `t2`
- added `scan` combinator

note: git log --no-merges previous_version..HEAD --pretty=%s
