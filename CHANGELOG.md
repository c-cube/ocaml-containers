# Changelog

## 0.3.3

- readme: add ci hook (to http://ci.cedeela.fr)
- `CCIO`: monad for IO actions-as-values
    - explicit finalizer system, to use a `>>>=` operator rather than callbacks
    - `File` for basic filenames manipulations
    - `Seq` for streams
- `CCMultiMap`: functor for bidirectional mappings
- `CCMultiSet`: sequence
- renamed threads/future to threads/CCFuture
- big upgrade of `RAL` (random access lists)
- `CCList.Ref` to help use references on lists
- `CCKList`: group,uniq,sort,sort_uniq,repeat and cycle, infix ops, applicative,product
- `CCTrie.above/below`: ranges of items
- more functions in `CCPair`
- `CCCat`: funny (though useless) definitions inspired from Haskell
- `CCList`: applicative instance
- `CCString.init`
- `CCError.fail_printf`

## 0.3.2

- small change in makefile
- conversions for `CCString`
- `CCHashtbl`: open-addressing table (Robin-Hood hashing)
- registered printers for `CCError`.guard,wrap1,etc.
- monadic operator in `CCList`: map_m_par
- simple interface to `PrintBox` now more powerful
- constructors for 1 or 2 elements fqueues
- bugfixes in BTree (insertion should work now)
- `CCFQueue`: logarithmic access by index
- add BTree partial implementation (not working yet)
- fix bug in `CCPrint.to_file`
- `CCArray.lookup` for divide-and-conquer search
- `CCList.sort_uniq`
- `CCError`: retry and choose combinators
- stub for monadic IO in `CCPrint`
- `CCopt.pure`
- updated `CCPersistentHashtbl` with new functions; updated doc, simplified code
- move `CCString` into core/, since it deals with a basic type; also add some features to `CCString` (Sub and Split modules to deal with slices and splitting by a string)
- `CCArray.blit`, .Sub.to_slice; some bugfixes
- applicative and lifting operators for `CCError`
- `CCError.map2`
- more combinators in `CCError`

## 0.3.1

- test for `CCArray.shuffle`
- bugfix in `CCArray.shuffle`
- `CCOpt.get_exn`
- `CCOpt.sequence_l`
- mplus instance for `CCOpt`
- monad instance for `CCFun`
- updated description in _oasis
- `CCTrie`, a compressed functorial persistent trie structure
- fix `CCPrint.unit`, add `CCPrint.silent`
- fix type mismatch

note: git log --no-merges previous_version..HEAD --pretty=%s
