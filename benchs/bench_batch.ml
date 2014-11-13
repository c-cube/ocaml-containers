(** benchmark CCBatch *)

open Containers_advanced

module type COLL = sig
  val name : string
  include CCBatch.COLLECTION
  val doubleton : 'a -> 'a -> 'a t
  val (--) : int -> int -> int t
  val equal : int t -> int t -> bool
end

module Make(C : COLL) = struct
  let f1 x = x mod 2 = 0
  let f2 x = -x
  let f3 x = C.doubleton x (x+1)
  let f4 x = -x
  let collect a = C.fold (+) 0 a

  let naive a =
    let a = C.filter f1 a in
    let a = C.flat_map f3 a in
    let a = C.filter f1 a in
    let a = C.map f2 a in
    let a = C.flat_map f3 a in
    let a = C.map f4 a in
    ignore (collect a);
    a

  module BA = CCBatch.Make(C)

  let ops =
    BA.(filter f1 >>> flat_map f3 >>> filter f1 >>> map f2 >>> flat_map f3 >>> map f4)

  let batch a =
    let a = BA.apply ops a in
    ignore (collect a);
    a

  let bench_for ~time n =
    Printf.printf "\n\nbenchmark for %s of len %d\n" C.name n;
    flush stdout;
    let a = C.(0 -- n) in
    (* debug
    CCPrint.printf "naive: %a\n" (CCArray.pp CCInt.pp) (naive a);
    CCPrint.printf "simple: %a\n" (CCArray.pp CCInt.pp) (batch_simple a);
    CCPrint.printf "batch: %a\n" (CCArray.pp CCInt.pp) (batch a);
    *)
    assert (C.equal (batch a) (naive a));
    let res = Benchmark.throughputN time
      [ C.name ^ "_naive", naive, a
      ; C.name ^ "_batch", batch, a
      ]
    in
    Benchmark.tabulate res

  let bench () =
    bench_for ~time:1 100;
    bench_for ~time:4 100_000;
    bench_for ~time:4 1_000_000;
    ()
end

module BenchArray = Make(struct
  include CCArray
  let name = "array"
  let equal a b = a=b
  let doubleton x y = [| x; y |]
  let fold = Array.fold_left
end)

module BenchList = Make(struct
  include CCList
  let name = "list"
  let equal a b = a=b
  let doubleton x y = [ x; y ]
  let fold = List.fold_left
end)

module BenchKList = Make(struct
  include CCKList
  let name = "klist"
  let equal a b = equal (=) a b
  let doubleton x y = CCKList.of_list [ x; y ]
end)

let () =
  BenchArray.bench();
  BenchList.bench();
  BenchKList.bench ();
  ()
