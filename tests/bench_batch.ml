(** benchmark CCBatch *)

module type COLL = sig
  val name : string
  include CCBatch.COLLECTION
  val doubleton : 'a -> 'a -> 'a t
  val (--) : int -> int -> int t
  val equal : int t -> int t -> bool
  val fold : (int -> int -> int) -> int -> int t -> int
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

  let batch_simple a =
    let a = BA.apply ~level:BA.OptimNone ops a in
    ignore (collect a);
    a

  let batch a =
    let a = BA.apply ~level:BA.OptimBase ops a in
    ignore (collect a);
    a

  let batch2 a =
    let a = BA.apply ~level:BA.OptimMergeFlatMap ops a in
    ignore (collect a);
    a

  let bench_for ~time n =
    Printf.printf "\n\nbenchmark for %s of len %d\n" C.name n;
    Printf.printf "optimization: from %d to %d\n"
      (BA.length ops) (BA.length (BA.optimize ops));
    flush stdout;
    let a = C.(0 -- n) in
    (* debug
    CCPrint.printf "naive: %a\n" (CCArray.pp CCInt.pp) (naive a);
    CCPrint.printf "simple: %a\n" (CCArray.pp CCInt.pp) (batch_simple a);
    CCPrint.printf "batch: %a\n" (CCArray.pp CCInt.pp) (batch a);
    *)
    assert (C.equal (batch_simple a) (naive a));
    assert (C.equal (batch_simple a) (batch a));
    let res = Benchmark.throughputN time
      [ C.name ^ "_naive", naive, a
      ; C.name ^ "_batch_simple", batch_simple, a
      ; C.name ^ "_batch", batch, a
      ; C.name ^ "_batch_merge", batch2, a
      ]
    in
    Benchmark.tabulate res

  let bench () =
    bench_for 1 100;
    bench_for 2 100_000;
    bench_for 2 1_000_000;
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
  let equal a b = equal a b
  let doubleton x y = CCKList.of_list [ x; y ]
end)

let () =
  BenchArray.bench();
  BenchList.bench();
  BenchKList.bench ();
  ()
