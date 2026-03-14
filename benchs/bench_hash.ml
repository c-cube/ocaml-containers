(** Benchmarks for CCHash primitives.

    Run with: dune exec benchs/bench_hash.exe -- [options]
    See: dune exec benchs/bench_hash.exe -- --help
*)

[@@@warning "-5"]

module B = Benchmark

let repeat = 3

(* --- data setup ---------------------------------------------------------- *)

let n_ints = 1_000

let ints =
  Array.init n_ints (fun i -> i * 2654435761 (* knuth multiplicative *))

let short_str = String.make 16 'x'
let medium_str = String.make 64 'x'
let long_str = String.make 512 'x'

(* Strings of various lengths with distinct content *)
let strings_short = Array.init n_ints (fun i -> Printf.sprintf "%016d" i)
let strings_medium = Array.init n_ints (fun i -> Printf.sprintf "%064d" i)

(* --- benchmarks ---------------------------------------------------------- *)

let bench_int_hash ~time () =
  let r = ref 0 in
  B.throughputN time ~repeat
    [
      "CCHash.int", (fun () -> Array.iter (fun x -> r := CCHash.int x) ints), ();
      ( "Hashtbl.hash (poly)",
        (fun () -> Array.iter (fun x -> r := Hashtbl.hash x) ints),
        () );
      ( "CCHash.int64",
        (fun () ->
          Array.iter (fun x -> r := CCHash.int64 (Int64.of_int x)) ints),
        () );
    ];
  ignore !r

let bench_string_hash ~time () =
  let r = ref 0 in
  B.throughputN time ~repeat
    [
      ( "CCHash.string/16",
        (fun () -> Array.iter (fun s -> r := CCHash.string s) strings_short),
        () );
      ( "CCHash.string/64",
        (fun () -> Array.iter (fun s -> r := CCHash.string s) strings_medium),
        () );
      "CCHash.string literal/16", (fun () -> r := CCHash.string short_str), ();
      "CCHash.string literal/64", (fun () -> r := CCHash.string medium_str), ();
      "CCHash.string literal/512", (fun () -> r := CCHash.string long_str), ();
      ( "Hashtbl.hash/16",
        (fun () -> Array.iter (fun s -> r := Hashtbl.hash s) strings_short),
        () );
    ];
  ignore !r

let bench_combine64 ~time () =
  let r = ref 0L in
  B.throughputN time ~repeat
    [
      ( "combine64 chain x5",
        (fun () ->
          Array.iter
            (fun x ->
              r :=
                CCHash.(
                  combine64
                    (combine64
                       (combine64
                          (combine64
                             (combine64 seed (Int64.of_int x))
                             (Int64.of_int (x lxor 0xaaaa)))
                          (Int64.of_int (x + 1)))
                       (Int64.of_int (x * 3)))
                    (Int64.of_int (x lxor (x lsr 7)))))
            ints),
        () );
      ( "CCHash.list int [1..5]",
        (fun () -> r := Int64.of_int CCHash.(list int [ 1; 2; 3; 4; 5 ])),
        () );
    ];
  ignore !r

(* --- tree for run_global ------------------------------------------------- *)

let () =
  B.Tree.add_global "hash"
    B.Tree.(
      "int"
      @>> (fun () -> bench_int_hash ~time:2 ())
      @> "string"
      @>> (fun () -> bench_string_hash ~time:2 ())
      @> "combine64"
      @>> (fun () -> bench_combine64 ~time:2 ())
      @> nil)

let () = try B.Tree.run_global () with Arg.Help msg -> print_endline msg
