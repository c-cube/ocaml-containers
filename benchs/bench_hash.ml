(** Benchmarks for CCHash primitives.
*)

[@@@warning "-5"]

module B = Benchmark

let repeat = 3
let n_ints = 1_000
let ints = Array.init n_ints (fun i -> i * 2654435761)

let bench_int_hash ~time () =
  B.throughputN time ~repeat
    [
      ( "CCHash.int",
        (fun () ->
          Array.iter
            (fun x -> ignore @@ Sys.opaque_identity (CCHash.int x))
            ints),
        () );
      ( "Hashtbl.hash (poly)",
        (fun () ->
          Array.iter
            (fun x -> ignore @@ Sys.opaque_identity (Hashtbl.hash x))
            ints),
        () );
      ( "CCHash.int64",
        (fun () ->
          Array.iter
            (fun x ->
              ignore @@ Sys.opaque_identity (CCHash.int64 (Int64.of_int x)))
            ints),
        () );
    ]

let bench_combine64 ~time () =
  B.throughputN time ~repeat
    [
      ( "combine64 chain x5",
        (fun () ->
          Array.iter
            (fun x ->
              ignore
              @@ Sys.opaque_identity
                   CCHash64.(
                     combine2
                       (combine2
                          (combine2
                             (combine2 (Int64.of_int x)
                                (Int64.of_int (x lxor 0xaaaa)))
                             (Int64.of_int (x + 1)))
                          (Int64.of_int (x * 3)))
                       (Int64.of_int (x lxor (x lsr 7)))))
            ints),
        () );
      ( "CCHash.list int [1..5]",
        (fun () ->
          Array.iter
            (fun x ->
              ignore
              @@ Sys.opaque_identity
                   (Int64.of_int
                      CCHash.(list int [ x + 1; x + 2; x + 3; x + 4; x + 5 ])))
            ints),
        () );
    ]

(* --- tree for run_global ------------------------------------------------- *)

let () =
  B.Tree.(
    register @@ "hash"
    @>>> [
           "int" @> lazy (bench_int_hash ~time:2 ());
           "combine64" @> lazy (bench_combine64 ~time:2 ());
         ])

let () = try B.Tree.run_global () with Arg.Help msg -> print_endline msg
