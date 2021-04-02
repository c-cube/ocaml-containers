# Changelog

## 3.3

- feat: add code-generator for optimal bitfields; add tests
- new Canonical sexpr module with printer and parser

- CCSeq: Add `for_all` and `exists`
- feat(sexp): expose last location in decoder
- feat(CCChar): add CCChar.Infix
- feat(CCString): add CCString.foldi
- feat(CCFormat): add `string_lines` combinator
- feat(CCList): update with regards to `partition_map`
- add `CCList.cons'`
- implement {of,add}_*_with family of function in `CCMap` with update (#352)
- add `CCMap.of_{list,iter,seq}_with` functions
- add `CCHashtbl.{of,add}_{list,seq,iter}_with`

- Fix integer overflow warning on jsoo (#346)
- updated fuzzer scripts

### Containers-thread

- refactor(pool): less locking, fix deadlock, more parallelism
- feat(pool): keep one idle thread
- small optim in `Pool.sequence_a`

## 3.2

- add CCEither module
- add `CCList.chunks`
- add iter/seq functions to `CCString`
- add `CCList.reduce` (resolves #305)
- fix: in `CCInt` pick popcount at runtime on 64 bits
- fix: in shims, use configurator properly to determine int size
- in `CCFormat`, add `append`, `append_l`, infix `++` for sequencing,
  `space`, `break`, `cut`
- fix: in `CCSexp`, handle non-ascii escapes in strings
- `CCUtf8_string`: add and expose `uchar_to_bytes`

- enable auto deploy of doc
- improve CI: test core on non ubuntu platform, test all on ubuntu
- update readme
- CCImmutArray: add tests (#344)
- add fuzzing (#339)
- add stronger test to compare with uutf in ccutf8string

## 3.1

- add `List.combine_chop` and corresponding `(and&)` synchronized product
- chore: remove travis to use github CI instead
- add `CCList.mguard` function for list comprehensions
- add some basic tests to CCMutHeap
- un-specify order of elements in `CCMap.to_list`
- Move definition of `CCMap.update` so that it is shadowed by Stdlib.Map.update
- fix(intmap): order of arguments for the HO param should be stable

- feat(containers-data): add `CCMutHeap` mutable heap with increase/decrease

## 3.0.1

- fix build on 32 bits architectures

## 3.0

### Breaking changes

see https://github.com/c-cube/ocaml-containers/issues/290 for a summary of
a subset of these changes.

packaging:

- split the library into separate packages
  `containers`, `containers-data`, and `containers-thread`.
- delete `containers.iter` and merge parts of it into `containers-data`;
- move `CCSexp` into the core library, remove `containers.sexp`.

api:

- remove slice APIs in string and array.
- change pp functions to take unit printer for sep/stop/start (#295)
- CCPair: use more standard name for some map functions (#316)
- remove `CCKlist` from everywhere
- CCGraph: remove deprecated module and function
- rename `<op>_std_seq` to `<op>_seq`, making `Seq.t` the standard everywhere;
  remove the old `<op>_seq` that were previously
  deprecated in favor of `<op>_iter`.
- CCVector: rename `shrink` into `truncate`
- CCVector: rename `remove to CCVector.remove_unordered`
- CCList: make mem compatible with the Stdlib by making `?eq` optional
- CCVector: rename `filter'` into `filter_in_place`

### Other changes

- CI: add github actions in addition to travis
- feat: add infix operators to `String`
- feat: add opt.bind
- CCResult: add `<$>` operator
- CCResult: add `get_lazy`
- put infix operators in `Infix` module, then include it
- ccnativeint: complete CCNativeint with regards to CCInt
- Int64: complete CCInt64 with regards to CCInt
- CCInt32: complete CCInt32 with regards to CCInt
- implement `CCInt.sign` using `CCInt.compare`
- CCInt: include module Int for ocaml >= 4.08
- CCInt: add `of_float`
- CCInt: add `of_string_exn`
- add `CCResult.get_lazy`
- add `Int.popcount` operator
- CCFloat: add `pi`
- CCFloat: add `of_string_opt`
- fix: expose `always_eq`/`never_eq` in `CCEqual`
- string: add optional `cutoff` arg on `String.edit_distance`
- CCVector: add `remove_and_shift`
- CCArray: add optional argument eq to mem
- CCSexp: Escape empty atoms
- substitute 'Pervasives' with 'Stdlib'
- CCFormat: add `exn` combinator
- IO: add `copy_into` for transferring data between channels

- Extend benchmark: `to_array`, cons and `cons_fold`
- Extend benchmark: Sek, iter and pop
- benchmark for memory usage of data structures

And many, many bugfixes.

## 2.8.1

- add missing `CCVector.of_iter`

## 2.8

### Breaking:

- bump minimum version of OCaml to 4.03, drop deps `{result,uchar}`
- deprecate `{of,to}_seq` a bit everywhere
- deprecate `CCKList` as it's subsumed by `Seq`

- feat: on `>= 4.08`, support let+ and let* operators
- feat(list): add indexed functions and `fold_on_map`
- refactor: also port `CCGraph` to iter
- feat: add `{to,of,add}_{iter,std_seq}` where relevant
- feat(unix): add `ensure_session_leader` and add some docs
- feat(pool): add infix operators on futures
- fix(pp): improve printing of hashtables
- feat: add `monoid_product` to Array and Vector
- improved gc behavior for `CCvector`
- deprecate `CCVector.fill_empty_slots_with`
- `CCVector.shrink_to_fit` to limit memory usage
- add `CCVector.clear_and_reset`
- feat(sexp): expose `parse_string_list` and the list decoder
- add `CCUnix.with_temp_dir` function
- deprecate `CCOpt.to_seq`, provide `to_iter` instead
- add `CCOpt.value` to improve compat with `Stdlib.Option`
- add `CCVector.mapi`

- fix: restore `CCSexp.atom` and `list` which was lost in 2.7
- fix(sexp): set location properly when parsing a file
- fix: properly alias to `CCChar` in containers.ml

- use older dune dialect
- remove unlabel, remove all traces of Result
- require dune configurator explicitly in opam
- Re-enable mdx tests
- fix benchs so they don't depend on clarity and they compile again

## 2.7

- deprecate CCKList in favor of the standard Seq
- CCIO: add `_gen` suffixes to some functions
- ccsexp: provide ability to annotate parsed S-exprs with their position
- ccsexp: functorize the parser/printer
- ccsexp: support `#;` for commenting a sexp
- fix: remove dep from vec to list
- add `to_string` to many modules (#270)
- add `CCDeque.{remove_*;update_*}`,` CCDeque.{*_opt}`
- add `CCDeque.{filter,filter_map}`
- add `CCDeque.filter_in_place`
- add `CCBool.{to,of}_int`
- add `Result.flatten_l` to turn a list of results into a result of list
- refactor: remove stdlib's code, simple reimplementation of `Stdlib.Fun`
- add `CCArray.Infix`
- Document behaviour of `Fun.finally` when finaliser raises

- travis: test on OCaml 4.09, too.
- more docs for IO

## 2.6.1

bugfix release:

- fix(parse): error in `many`
- chore: add 4.08 to travis
- fix `Containers.Stdlib` on OCaml 4.07

## 2.6

- introduce shim modules for 4.08 compat
- remove reference to sequence, use `iter` instead for tests
- add `remove` function to het map/tbl
- missing type annotation for specializing int.compare

- doc: fix bad example in CCIO
- use `iter`, not `sequence`, in tests
- fix: use same evaluation order as stdlib for `CCList.init`
- fix: make `Array.random_choose` fail on empty array at creation time
- breaking: make `Array.random_choose` raise invalid_arg instead of not_found
- migrate readme to .md, using mdx to test it

## 2.5

- perf: annotate types in monomorphic/float/int to help specialize builtins
- use GADT to discard impossible case on `CCFQueue` (@dinosaure).
- fix(funvec): expose `pop`, fix off by one error

## 2.4.1

- revert some compatibility-breaking changes in label modules

## 2.4

### breaking:

- rename `Random.sample_without_{replacement,duplicates}`

### Features

- add `CCResult.iter_err`
- add `CCEqual.{always,never}_eq`
- add `containersLabels.ml`, generate unlabelled interfaces from labelled ones
- add `CCEqualLabels`
- add `CCArray_sliceLabels`
- add `CCStringLabels`
- add `CCResult.get_or_failwith`
- add `CCInt.( ** )` for integer exponentiation
- add `List.counts`, related to `List.count` (#230)

- migrate to dune
- migrate to opam2
- add CODE_OF_CONDUCT.md

### Fixes

- #235: release memory in vector/ringbuffer (thanks to @copy)
- remove spurious `Labels` module
- doc: fix small inaccuracy in comments and API
- test: improve perf by changing random gens

## 2.3

- feat(vector): add `Vector.{filter,filter_map}_in_place`
- perf(hashtrie): use int64 for 64-bits branching factor and popcount
- feat(intmap): add `CCIntMap.{filter,filter_map,merge,is_empty}`
- Add `CCHeap.Make_from_compare` (#225)
- add relational ops `CCList.{group_by,join,join_by,join_all_by,group_join_by}`

- fix(float): make `Float.{min,max}` compliant with revised IEEE754
- fix(build): remove `[@inline]` attributes since they break on 4.02.3
- Fix Int32 and Int64 operators that are not visible (#224)

- some performance tweaks in Vector
- test(float): add some tests for FP min/max

## 2.2

- Improving comments presentation
- Add `CCOpt.return_if`
- Add `CCOpt.flatten`
- Add `CCString.{,r}drop_while`
- add many missing functions to `CCListLabels`
- test: consistency `CCList{,Labels}`

- fix(arrayLabels): compatibility with 4.07
- fix: compatibility for CCArrayLabels
- test: add compatibility checks between `CCArray{,Labels}`

## 2.1

- make `CCInt64` compatible with `Int64` (breaking!) (closes #192)

- Add `CCBijection` in containers.data
- feat(mono): add dotted comparison operators for floats
- add `?margin` parameter to `CCFormat.ksprintf`
- add `CCUtf8_string` with basic encoding and decoding functionalities
- Add `CCLazy_list.<|>`
- Adding `CCNativeint`
- enrich `CCInt.Infix` to get a uniform interface with `CCInt{32,64}`
- add `CCInt{32,64}.Infix`
- Adding CCInt32 module
- add `CCHash.combine{5,6}`
- Add infix operators to CCFloat
- feat(list): add `{interleave,intersperse}` (closes #191)
- add missing signatures of `CCArrayLabels` (closes #193)
- Add CCFun.iterate
- add experimental `CCFun_vec` data structure for fast functional vectors

- fix: strong type aliases in Random (closes #210)
- use standard `List.sort_uniq`
- remove explicit dep on `bytes` in jbuild files
- update printers names in containers.top (closes #201)
- Enable support for Travis CI and Appveyor
- test deps are required when we run tests
- point to JST's blog post on poly compare

## 2.0

### breaking

- move to jbuilder (closes #165), requiring at least OCaml 4.02
- become defensive w.r.t polymorphic operators:
  * Internally shadow polymorphic operators and functions from Pervasives
    by `include CCMonomorphic` in `Containers` module
  * Shadow the physical equality operator
  * Shadow polymorphic functions in `CCList`
- rename `print` to `pp` for Format printers (closes #153, #181)
- remove `CCFlatHashtbl`

### others

- many typos and style fixes  (from Fourchaux)
- Add `CCList.iteri2` and `CCList.foldi2`
- remove `PARAM.min_size` in `CCPool`
- Add `CCEqual.physical`
- Avoid uses of the polymorphic operators
- Add a `CCMonomorphic` module shipped into a `containers.monomorphic` library
- make complexity of `Array.lookup` explicit (closes #174)
- add `CCFormat.lazy_{or,force}` for printing thunks
- now that ocaml >= 4.02 is required, use `Format.pp_print_text` directly
- add `CCHeap.delete_{one,all}`
- add `CCList.tail_opt`


- remove qtest makefile and use a script instead
- add many tests
- fix bug in `CCRAL.drop` (see #184)
- `CCFormat`: fix support of unrecognized styles
- fix bug: don't reverse twice in `CCList.repeat`

## 1.5.1, 1.5.2

- re-export `Format` types and functions in `CCFormat`

## 1.5

- have `CCList.{get,insert,set}_at_idx` work with negative indices
- Add CCCache.add
- missing function in `CCListLabels`
- Allow negative indexes in CCList.remove_at_idx
- add an optional `drop` parameter to string-splitting functions
- add `Hash.const0` for trivial hash function that ignores its input
- improve compatibility with the stdlib
- Add List.count
- Add String.is_empty
- add missing compatibility functions: `{assoc_opt,assq_opt}`
- backport some functions added in 4.05 in `CCList`
- add functions from 4.05 into `CC{Map,Set}`
- Implement `CCImmutArray.sub`
- bugfix in `CCTrie.Make`: Remove polymorphic comparison

- remove dependency on cppo
- add travis support
- update doc of `CCList.cartesian_product`, which returns results in unspecified order (close #154)
- fix containers.top (closes #155)

## 1.4

- add `CCMap.union`
- add `CCRef.swap`
- add `CCArray.swap`
- change signature of `CCWBTree.get_rank`
- add `CCWBTree.get_rank{,_exn}`

- more efficient `List.map` Using efficient chunking algorithm
- Fix `CCVector.append_array` (empty vector case)
- `CCFQueue.take_back_exn` raised InvalidArg instead of Empty on an empty queue
- faster `CCString.{prefix,suffix}`
- speed improvements and benchmarks for `CCString.{prefix,suffix}`

- add ocp-indent file
- fix `CCFun.tap` example in doc
- specify behavior of `CCFQueue.take_{front,back}_l` in some corner cases
- More tests for CCVector.append and CCVector.append_array
- assertions and cleanup in `CCPool`

## 1.3

- deprecate `CCBool.negate`
- add `CCString.compare_natural` (closes #146)
- add callbacks in `CCCache.with_cache{,_rec}` (closes #140)
- tail-rec `CCList.split` (by @bikalgurung, see #138)
- change `CCRingBuffer.peek_{front,back}` to return options (closes #127)
- add `CCRingBuffer.is_full`
- add `CCArray.find_map{,_i}`, deprecated older names (closes #129)
- add `CCList.{keep,all}_{some,ok}` (closes #124)
- large refactor of `CCSimple_queue` (close #125)
- add `CCSimple_queue` to containers.data
- small change for consistency in `CCIntMap`

- bugfix in `CCRingBuffer.skip`, and corresponding tests
- cleanup and refactor of `CCRingBuffer` (see #126). Add strong tests.
- add rich testsuite to `CCIntMap`, based on @jmid's work

## 1.2

- make many modules extensions of stdlib (close #109)
  the modules are: `String List ListLabels Array ArrayLabels Char Random`
- add `CCString.{l,r}trim` (close #121)
- add `CCInt.floor_div` and `CCInt.rem`
- add test and bugfix for `CCBV`
- add `CCList.take_drop_while` (close #120)
- add `CCstring.equal_caseless` (close #112)
- add alias `CCString.split` (close #115)
- add `CCFormat.text` (close #111)
- add `CCFormat.{newline,substring}`
- add `CCList.combine_gen` (close #110)
- add module `CCEqual`
- add `CCResult.fold_ok` (closes #107)
- add `CCFormat.with_color_ksf` for colored printing
- add `CCInt.range{,',by}` for iterating on integer ranges
- add `CCString.Sub.get`
- add `CCResult.add_ctx{,f}` for replacing stack traces
- add `CCString.split_on_char`
- add `CCArray.{fold_map,scan_left}` (close #101)
- add `CCList.scan_left`
- add `CCList.{cartesian_product,map_product_l}`
- add `CCUnix.with_file_lock` for locking whole files
- add `CCFormat.of_chan`
- add `CCFormat.flush`
- Add `{map_lazy, or_, or_lazy, to_result, to_result_lazy, of_result}` to `CCOpt`

- annotations in `CCEqual`, for optimization
- Add a tail-recursive implementation of `List.combine`
- fix too restrictive type in `CCResult`
- build unix support by default
- bugfix and test for `CCZipper.is_focused` (closes #102)
- use boxes in `CCFormat.Dump` for tuples
- update header, and use more `(##)` in `CCIntMap`

## 1.1

**bugfixes**:

- fix bug in `CCGraph` (in DFS traversal)
- fix bug in `CCOpt.filter` (close #100)

**new features**:

- add `CCHeap.to_seq_sorted`
- add `CCHeap.to_list_sorted`
- add `CCIO.File.walk_l`

**cleanup and doc**:

- remove dead code
- new test for `CCPool`
- new test and small readme section on `CCParse`
- remove CCError from tutorial
- merge tutorial into readme, cleanup

## 1.0

See https://github.com/c-cube/ocaml-containers/issues/84 for an overview.

**Breaking and Removals**:

- simplify and cleanup of CCGraph
- remove poly-variant based errors, use `result` everywhere
- remove deprecated functions and modules
- remove `CCVHashconsedSet`
- remove `CCAllocCache`
- remove `CCBloom`
- update benchmarks (ignoring hamt); remove useless old script
- simplify `CCHash`, changing the type to `'a -> int`, relying on
  `Hashtbl.seeded_hash` for combining hashes
- split `CCList.Zipper` into its own module, `CCZipper` in containers.data
- change argument ordering in `CCList.Assoc`
- remove `CCList.Idx`, rename its functions to toplevel
- remove `CCList.Set`, move functions to toplevel and rename them
- rewrite `CCBitField` with a much simpler interface
- split `CCArray.Sub` into `CCArray_slice`
- remove containers.string
  * remove CCParse and CCKMP (will be replaced in core)
- `CCFormat`:
  * remove `start/stop` args, make `sep` a `unit printer`
  * many other improvements and additions
  * add `CCFormat.tee`
  * add `CCFormat.Dump.{result,to_string}`
- replace `or_` by `default` in labelled functions
- remove trailing `_` in `CCOrd` primitives
- remove `containers.io` (deprecated for a while)
- remove `containers.bigarray`
- remove `CCSexpM`, use ocamllex for a much simpler `CCSexp` using ocamllex
- add `CCParse` into core, a simple, lightweight version of parser combs
- remove `CCPrint`, use `CCFormat` instead (also, update tests relying on it)
- remove containers.advanced
- change type of `CCUnix.escape_str`

**Additions**:

- `CCHashtbl`:
  * `CCHash.{list,array}_comm`
  * `CCHashtbl.Poly` and fix issue in Containers (close #46)
  * `CCHashtbl.get_or_add`
- `CCList.sublists_of_len` (close #97)
- `Char.{of_int{,_exn},to_int}` (close #95)
- Add `CCResult.{is_ok,is_error}`
- improve `CCUnix` a bit
- update `containers.ml` so as to include all core containers
- add `CCOrd.Infix`
- use `Labels` versions of `CCList` and `CCArray`
- add `CCString.edit_distance`
- expose `CCString.Find` for efficient sub-string searching

**Bugfixes**:

- `CCIO`: deal properly with broken symlinks and permission errors
- test for #94 (using Thread.yield to trigger segfault)
  Fix `CCSemaphore.with_acquire`: release a non locked mutex is UB
- containers.top: remove printers on structural types (#71)
- add doc for `of_list` in relevant modules (close #85)
- bugfix: do not use `Sequence.flatMap` (close #90)

## 0.22

- threads/CCLock: add `try_with_lock` to wrap `Mutex.try_lock`
- Add `CCMultiSet.remove_all`
- document errors in `CCIO` (close #86)
- use the new qtest/qcheck

## 0.21

- (breaking) make default `start`/`stop` arguments empty in printers (#82)

- add `CCFormat.{with_color_sf,fprintf_dyn_color,sprintf_dyn_color}`
- add `CCFormat.Dump` for easy debugging (see #82)
- add `CCArray.Sub.to_list`
- add `CCArray.{sorted,sort_indices,sort_ranking}` (closes #81)

- handle `\r` in CCSexpM (fixes #83)
- add alias `Containers.IO`
- bugfixes in `CCArray.Sub`
- bugfix + tests for `CCArray.Sub.sub`
- disable parallel build to support cygwin

## 0.20

- bugfix in `CCArray.equal`
- fix `CCString.*_ascii`; add `CCChar.{upper,lower}case_ascii`

- add functions in `CCArray`: fold2,iter2,map2
- add `CCArray.rev`
- add `CCFloat.round`
- add `CCVector.append_gen`
- add `CCList.{head_opt,last_opt}`
- add `CCInt.{print_binary,to_string_binary}` + tests (thanks @gsg)
- more general types for `CCArray.{for_all2,exists2}`
- more general type for `CCResult.map_or`

## 0.19

- add regression test for #75
- Fix `CCString.Split.{left,right}` (#75)
- additional functions in `CCMultiSet`
- show ocaml array type concretely in `CCRingBuffer.Make` sig
- cleanup and more tests in `CCHeap`
- fix bugs in `CCFlatHashtbl`, add some tests
- add more generic printers for `CCError` and `CCResult` (close #73)
- add `CCstring.of_char`
- update headers

## 0.18

- update implem of `CCVector.equal`
- add `CCOpt.get_or` with label, deprecates `get`
- add `CCArray.get_safe` (close #70)
- add `CCGraph.is_dag`
- add aliases to deprecated  functions from `String`, add `Fun.opaque_identity`
- add `CCLazy_list.take`
- add `Lazy_list.filter`
- add `CCList.range_by`

## 0.17

### potentially breaking

- change the semantics of `CCString.find_all` (allow overlaps)

### Additions

- add `CCString.pad` for more webscale
- add `(--^)` to CCRAl, CCFQueue, CCKlist (closes #56); add `CCKList.Infix`
- add monomorphic signatures in `CCInt` and `CCFloat`
- add `CCList.{sorted_insert,is_sorted}`
- add `CCLazy_list` in containers.iter (with a few functions)
- add `CCTrie.longest_prefix`
- provide additional ordering properties in `CCTrie.{above,below}`
- add `CCOpt.if_`
- have
  * `CCRandom.split_list` fail on `len=0`
  * `CCRandom.sample_without_replacement` fail if `n<=0`
- add `CCOpt.{for_all, exists}`
- add `CCRef.{get_then_incr,incr_then_get}`
- add `Result.{to,of}_err`
- add `CCFormat.within`
- add `map/mapi` to some of the map types.
- add `CCString.{drop,take,chop_prefix,chop_suffix,filter,filter_map}`
- add `CCList.fold_filter_map`
- add `CCIO.File.with_temp` for creating temporary files
- add `{CCArray,CCVector,CCList}.(--^)` for right-open ranges
- add `Containers.{Char,Result}`
- modify `CCPersistentHashtbl.merge` and add `CCMap.merge_safe`
- add `CCHet`, heterogeneous containers (table/map) indexed by keys
- add `CCString.rev`
- add `CCImmutArray` into containers.data
- add `CCList.Assoc.remove`

### Fixes, misc

- Make `CCPersistentHashtbl.S.merge` more general.
- optimize KMP search in `CCString.Find` (hand-specialize code)
- bugfix in `CCFormat.to_file` (fd was closed too early)

- add a special case for pattern of length 1 in `CCString.find`
- more tests, bugfixes, and benchs for KMP in CCString
- in CCString, use KMP for faster sub-string search; add `find_all{,_l}`

others:

- `watch` target should build all
- add version constraint on sequence
- migrate to new qtest
- add an `IO` section to the tutorial
- enable `-j 0` for ocamlbuild

## 0.16

### breaking

- change the signature of `CCHeap.{of_gen,of_seq,of_klist}`
- change the API of `CCMixmap`
- make type `CCHash.state` abstract (used to be `int64`)
- optional argument `~eq` to `CCGraph.Dot.pp`
- rename `CCFuture` into `CCPool`

### deprecations

- deprecate `containers.bigarray`
- deprecate `CCHashtbl.{Counter,Default}` tables
- deprecate `CCLinq` in favor of standalone `OLinq` (to be released)

### bugfixes

- fix wrong signature of `CCHashtbl.Make.{keys,values}_list`
- missing constraint in `CCSexpM.ID_MONAD`

### new features

- add a tutorial file
- add a printer into CCHeap
- add `{CCList,CCOpt}.Infix` modules
- add `CCOpt.map_or`, deprecating `CCopt.maybe`
- add `CCFormat.sprintf_no_color`
- add `CCFormat.{h,v,hov,hv}box` printer combinators
- add `CCFormat.{with_color, with_colorf}`
- add `CCList.hd_tl`
- add `CCResult.{map_or,get_or}`
- add `CCGraph.make` and utils
- add `CCHashtbl.add_list`
- add counter function in `CCHashtbl`, to replace `CCHashtbl.Counter`
- add `CCPair.make`
- add `CCString.Split.{left,right}_exn`
- add `CCIO.File.{read,write,append}` for quickly handling files
- add `CCRandom.pick_{list,array}`
- add `CCList.Assoc.update`
- add `CCList.Assoc.mem`
- add `{CCMap,CCHashtbl}.get_or` for lookup with default value
- add `CCLock.{decr_then_get, get_then_{decr,set,clear}}`
- rename `CCFuture` into `CCPool`, expose the thread pool
- split `CCTimer` out of `CCFuture`, a standalone 1-thread timer
- move `CCThread.Queue` into `CCBlockingQueue`
- add `CCResult`, with dependency on `result` for retrocompat
- add `CCThread.spawn{1,2}`
- add many helpers in `CCUnix` (for sockets, files, and processes)
- add `CCFun.finally{1,2}`, convenience around `finally`
- add `CCLock.update_map`
- add `CCLock.{incr_then_get,get_then_incr}`
- add breaking space in `CCFormat.{pair,triple,quad}`
- update `examples/id_sexp` so it can read on stdin
- add `CCList.fold_map2`

## 0.15

### breaking changes

- remove deprecated `CCFloat.sign`
- remove deprecated `CCSexpStream`

### other changes

- basic color handling in `CCFormat`, using tags and ANSI codes
- add `CCVector.ro_vector` as a convenience alias
- add `CCOrd.option`
- add `CCMap.{keys,values}`
- add wip `CCAllocCache`, an allocation cache for short-lived arrays
- add `CCError.{join,both}` applicative functions for CCError
- opam: depend on ecamlbuild
- work on `CCRandom` by octachron:
  * add an uniformity test
  * Make `split_list` uniform
  * Add sample_without_replacement

- bugfix: forgot to export `{Set.Map}.OrderedType` in `Containers`

## 0.14

### breaking changes

- change the type `'a CCParse.t` with continuations
- add labels on `CCParse.parse_*` functions
- change semantics of `CCList.Zipper.is_empty`

### other changes

- deprecate `CCVector.rev'`, renamed into `CCVector.rev_in_place`
- deprecate `CCVector.flat_map'`, renamed `flat_map_seq`

- add `CCMap.add_{list,seqe`
- add `CCSet.add_{list,seq}`
- fix small ugliness in `Map.print` and `Set.print`
- add `CCFormat.{ksprintf,string_quoted}`
- add `CCArray.sort_generic` for sorting over array-like structures in place
- add `CCHashtbl.add` mimicking the stdlib `Hashtbl.add`
- add `CCString.replace` and tests
- add `CCPersistentHashtbl.stats`
- reimplementation of `CCPersistentHashtbl`
- add `make watch` target
- add `CCVector.rev_iter`
- add `CCVector.append_list`
- add `CCVector.ensure_with`
- add `CCVector.return`
- add `CCVector.find_map`
- add `CCVector.flat_map_list`
- add `Containers.Hashtbl` with most combinators of `CCHashtbl`
- many more functions in `CCList.Zipper`
- large update of `CCList.Zipper`
- add `CCHashtbl.update`
- improve `CCHashtbl.MakeCounter`
- add `CCList.fold_flat_map`
- add module `CCChar`
- add functions in `CCFormat`
- add `CCPrint.char`
- add `CCVector.to_seq_rev`
- doc and tests for `CCLevenshtein`
- expose blocking decoder in `CCSexpM`
- add `CCList.fold_map`
- add `CCError.guard_str_trace`
- add `CCError.of_exn_trace`
- add `CCKlist.memoize` for costly computations
- add `CCLevenshtein.Index.{of,to}_{gen,seq}` and `cardinal`

- small bugfix in `CCSexpM.print`
- fix broken link to changelog (fix #51)
- fix doc generation for `containers.string`
- bugfix in `CCString.find`
- raise exception in `CCString.replace` if `sub=""`
- bugfix in hashtable printing
- bugfix in `CCKList.take`, it was slightly too eager

## 0.13

### Breaking changes

- big refactoring of `CCLinq` (now simpler and cleaner)
- changed the types `input` and `ParseError`  in `CCParse`
- move `containers.misc` and `containers.lwt` into their own repo
- change the exceptions in `CCVector`
- change signature of `CCDeque.of_seq`

### Other changes

- add module `CCWBTree`, a weight-balanced tree, in `containers.data`.
- add module `CCBloom` in `containers.data`, a bloom filter
- new module `CCHashTrie` in `containers.data`, HAMT-like associative map
- add module `CCBitField` in `containers.data`, a safe abstraction for bitfields of `< 62 bits`
- add module `CCHashSet` into `containers.data`, a mutable set
- add module `CCInt64`
- move module `RAL` into `containers.data` as `CCRAL`
- new module `CCThread` in `containers.thread`, utils for threading (+ blocking queue)
- new module `CCSemaphore` in `containers.thread`, with simple semaphore
- add `containers.top`, a small library that installs printers

- add `CCParse.memo` for memoization (changes `CCParse.input`)
- add `CCString.compare_versions`
- update `CCHash` with a functor and module type for generic hashing
- add `CCList.{take,drop}_while`; improve map performance
- add `CCList.cons_maybe`
- add `CCArray.bsearch` (back from batteries)
- add fair functions to `CCKList`
- deprecate `CCList.split`, introduce `CCList.take_drop` instead.
- add `CCKtree.force`
- add tests to `CCIntMap`; now flagged "stable" (for the API)
- add `CCOpt.choice_seq`
- add `CCOpt.print`
- add `CCIntMap.{equal,compare,{of,to,add}_{gen,klist}}`
- add `CCThread.Barrier` for simple synchronization
- add `CCPersistentArray.{append,flatten,flat_map,of_gen,to_gen}`
- add `CCDeque.clear`
- add `CCDeque.{fold,append_{front,back},{of,to}_{gen,list}}` and others
- add `CCKList.{zip, unzip}`
- add `CCKList.{of_array,to_array}`
- add `CCKList.{head,tail,mapi,iteri}`
- add `CCKList.{unfold,of_gen}`
- add `CCParse.{input_of_chan,parse_file,parse_file_exn}`
- modify `CCParse.U.list` to skip newlines
- add `CCDeque.print`
- add `CCBV.print`
- add printer to `CCHashtbl`

- bugfix in `CCSexpM`
- new tests in `CCTrie`; bugfix in `CCTrie.below`
- lots of new tests
- more benchmarks; cleanup of benchmarks
- migration of tests to 100% qtest
- migration markdown to asciidoc for doc (readme, etc.)
- add tests to `CCIntMap`, add type safety, and fix various bugs in `{union,inter}`
- more efficient `CCThread.Queue.{push,take}_list`
- slightly different implem for `CCThread.Queue.{take,push}`
- new implementation for `CCDeque`, more efficient
- update makefile (target devel)

## 0.12

### breaking

- change type of `CCString.blit` so it writes into `Bytes.t`
- better default opening flags for `CCIO.with_{in, out}`

### non-breaking

NOTE: use of `containers.io` is deprecated (its only module has moved to `containers`)

- add `CCString.mem`
- add `CCString.set` for updating immutable strings
- add `CCList.cons` function
- enable `-safe-string` on the project; fix `-safe-string` issues
- move `CCIO` from `containers.io` to `containers`, add dummy module in `containers.io`
- add `CCIO.read_all_bytes`, reading a whole file into a `Bytes.t`
- add `CCIO.with_in_out` to read and write a file
- add `CCArray1` in containers.bigarray, a module on 1-dim bigarrays (experimental)
- add module `CCGraph` in `containers.data`, a simple graph abstraction similar to `LazyGraph`
- add a lot of string functions in `CCString`
- add `CCError.catch`, in prevision of the future standard `Result.t` type
- add `CCError.Infix` module
- add `CCHashconsedSet` in `containers.data` (set with maximal struct sharing)

- fix: use the proper array module in `CCRingBuffer`
- bugfix: `CCRandom.float_range`

## 0.11

- add `CCList.{remove,is_empty}`
- add `CCOpt.is_none`
- remove packs for `containers_string` and `containers_advanced`
- add `Containers_string.Parse`, very simple monadic parser combinators
- add `CCList.{find_pred,find_pred_exn}`
- bugfix in `CCUnix.escape_str`
- add methods and accessors to `CCUnix`
- in `CCUnix`, use `Unix.environment` as the default environment
- add `CCList.partition_map`
- `RingBuffer.{of_array, to_array}` convenience functions
- `containers.misc.RAL`: more efficient in memory (unfold list)
- add `CCInt.pow` (thanks to bernardofpc)
- add `CCList.group_succ`
- `containers.data.CCMixset`, set of values indexed by poly keys
- disable warning 32 (unused val) in .merlin
- some infix operators for `CCUnix`
- add `CCUnix.async_call` for spawning and communicating with subprocess
- add `CCList.Set.{add,remove}`
- fix doc of `CCstring.Split.list_`

## 0.10

- add `containers.misc.Puf.iter`
- add `CCString.{lines,unlines,concat_gen}`
- `CCUnix` (with a small subprocess API)
- add `CCList.{sorted_merge_uniq, uniq_succ}`
- breaking: fix documentation of `CCList.sorted_merge` (different semantics)
- `CCPersistentArray` (credit to @gbury and Jean-Christophe Filliâtre)
- `CCIntMap` (big-endian patricia trees) in containers.data
- bugfix in `CCFQueue.add_seq_front`
- add `CCFQueue.{rev, --}`
- add `App_parse` in `containers.string`, experimental applicative parser combinators
- remove `containers.pervasives`, add the module `Containers` to core
- bugfix in `CCFormat.to_file`

## 0.9

- add `Float`, `Ref`, `Set`, `Format` to `CCPervasives`
- `CCRingBuffer.append` (simple implementation)
- `containers.data` now depends on bytes
- new `CCRingBuffer` module, imperative deque with batch (blit) operations,
  mostly done by Carmelo Piccione
- new `Lwt_pipe` and `Lwt_klist` streams for Lwt, respectively (un)bounded
  synchronized queues and lazy lists
- `CCKTree.print`, a simple S-expressions printer for generic trees
- Add `CCMixmap` in containers.data (close #40), functional alternative to `CCMixtbl`
- remove old META file
- simplified `CCTrie` implementation
- use "compiledObject: best" in `_oasis` for binaries
- document some invariants in `CCCache` (see #38)
- tests for `CCCache.lru`
- fix `CCFormat.seq` combinator
- add `CCSet` module in core/
- add `CCRef` module in core/

## 0.8

- add `@Emm` to authors
- refactored heavily `CCFuture` (much simpler, cleaner, basic API and thread pool)
- add `CCLock` in containers.thread
- merged `test_levenshtein` with other tests
- Add experimental rose tree in `Containers_misc.RoseTree`.
- remove a lot of stuff from `containers.misc` (see `_oasis` for details)
- `make devel` command, activating most flags, for developers (see #27)
- use benchmark 1.4, with the upstreamed tree system
- test `ccvector.iteri`
- add `CCFormat` into core/
- infix map operators for `CCArray`
- `fold_while` impl for `CCList` and `CCArray`
- Added `CCBigstring.length` for more consistency with the `CCString` module.
- Added name and dev fields in the OPAM file for local pinning.
- Fix `CCIO.remove*` functions.
- Added `CCIO.remove_safe`.
- only build doc if all the required flags are enabled
- `CCHashtbl.{keys,values}_list` in the functor as well. Better doc.
- `CCHashtbl.{keys,values}_list`
- more accurate type for `CCHashtbl.Make`

## 0.7

### breaking

- remove `cgi`/
- removed useless Lwt-related module
- remove `CCGen` and `CCsequence` (use the separate libraries)
- split the library into smaller pieces (with `containers.io`, `containers.iter`,
    `containers.sexp`, `containers.data`)

### other changes

- cleanup: move sub-libraries to their own subdir each; mv everything into `src/`
- `sexp`:
    * `CCSexp` now split into `CCSexp` (manipulating expressions) and `CCSexpStream`
    * add `CCSexpM` for a simpler, monadic parser of S-expressions (deprecating `CCSexpStream`)
- `core`:
    * `CCString.fold`
    * `CCstring.suffix`
    * more efficient `CCString.init`
    * fix errors in documentation of `CCString` (slightly over-reaching sed)
    * add `CCFloat.{fsign, sign_exn}` (thanks @bernardofpc)
- new `containers.bigarray`, with `CCBigstring`
- `CCHashtbl.map_list`
- `io`:
    * `CCIO.read_all` now with ?size parameter
    * use `Bytes.extend` (praise modernity!)
    * bugfix in `CCIO.read_all` and `CCIO.read_chunks`
- use `-no-alias-deps`

## 0.6.1

- use subtree `gen/` for `CCGen` (symlink) rather than a copy.
- Add benchmarks for the function `iter` of iterators.
- `CCKTree`: more printers (to files), `Format` printer
- `CCOpt.get_lazy` convenience function
- introduce `CCFloat`, add float functions to `CCRandom` (thanks to @struktured)

## 0.6

### breaking changes

- new `CCIO` module, much simpler, but incompatible interface
- renamed `CCIO` to `advanced.CCMonadIO`

### other changes

- `CCMultiSet.{add_mult,remove_mult,update}`
- `CCVector.{top,top_exn}`
- `CCFun.compose_binop` (binary composition)
- `CCList.init`
- `CCError.map2` has a more general type (thanks to @hcarty)
- new module `CCCache`
    * moved from `misc`
    * add `CCache`.{size,iter}
    * incompatible interface (functor -> values), much simpler to use
- `lwt/Lwt_actor` stub, for erlang-style concurrency (albeit much much more naive)
- `misc/Mixtbl` added from its old repository
- more benchmarks, with a more general system to select/run them
- more efficient versions of `CCList.{flatten,append,flat_map}`, some functions
  are now tailrec


## 0.5

### breaking changes

- dependency on `cppo` (thanks to @whitequark, see `AUTHORS.md`) and `bytes`
- `CCError`:
    * now polymorphic on the error type
    * some retro-incompatibilies (wrap,guard)
- `CCPervasives.Opt` -> `CCPervasives.Option`
- `Levenshtein.Index.remove` changed signature (useless param removed)

### other changes

- stronger inlining for `CCVector` (so that e.g. push is inline)
- more tests for `CCVector`
- removed many warnings
- `CCSequence` now provides some bytes-dependent operations
- `CCList.(>|=)` map operator
- `CCOpt.filter`
- `CCInt.neg`
- `CCMap` wrapper to the standard `Map` module
- make some functions in `CCFun` and `CCString` depend on ocaml version
- thanks to @whitequark, could use cppo for preprocessing files
- add Format printers to `CCString`
- `AUTHORS.md`

## 0.4.1

- `CCOpt.get`
- new functions in `CCSexp.Traverse`
- comments in `CCMultiSet.mli`, to explain meet/intersection/union
- `CCMultiset`: Add meet
- update of readme
- generate doc for `containers.advanced`

## 0.4

- `core/CCSexp` for fast and lightweight S-expressions parsing/printing
- moved `CCLinq`, `CCBatch` and `CCat` from core/ to advanced/
- ensure compatibility with ocaml 4.00
- get rid of deprecated `Array.create`
- move benchmarks to benchs/ so they are separate from tests
- `CCError.{iter,get_exn}`
- `CCPair.print`
- some small improvements to `CCRandom`
- moved `CCHashtbl` to `CCFlatHashtbl`; new module `CCHashtbl` that
  wraps and extends the standard hashtable
- `CCPervasives` module, replacing modules of the standard library
- removed type alias `CCString.t` (duplicate of String.t which already exists)

## 0.3.4

- subtree for `sequence` repo
- `CCSequence` is now a copy of `sequence`
- `CCOpt.wrap{1,2}`
- `CCList.findi`, `CCArray.findi` and `CCArray.find_idx`
- better `Format` printers (using break hints)
- specialize some comparison functions
- `CCOrd.map`

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
- `CCKList`: `group,uniq,sort,sort_uniq,repeat` and `cycle`, infix ops, applicative,product
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
- monadic operator in `CCList`: `map_m_par`
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
- `CCArray.blit`, `.Sub.to_slice`; some bugfixes
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
- updated description in `_oasis`
- `CCTrie`, a compressed functorial persistent trie structure
- fix `CCPrint.unit`, add `CCPrint.silent`
- fix type mismatch

NOTE: `git log --no-merges previous_version..HEAD --pretty=%s`
