let verbose = try Sys.getenv "VERBOSE" = "1" with _ -> false

module J = Yojson.Safe
module Fmt = CCFormat
module Cbor = Containers_cbor

type json = Yojson.Safe.t

module Test = struct
  type expect =
    | Diagnostic of string
    | Decoded of json

  type t = {
    hex: string;
    raw: string;
    expect: expect;
    roundtrip: bool;
  }
end

let list_assoc_opt x l = try Some (List.assoc x l) with _ -> None

let extract_tests (j : json) : Test.t list =
  let l = J.Util.to_list j in
  List.map
    (fun o ->
      let o = J.Util.to_assoc o in
      let hex = J.Util.to_string @@ List.assoc "hex" o in
      let raw = CCString.of_hex_exn @@ hex in
      let roundtrip = J.Util.to_bool @@ List.assoc "roundtrip" o in
      let expect =
        match list_assoc_opt "decoded" o, list_assoc_opt "diagnostic" o with
        | None, Some (`String s) -> Test.Diagnostic s
        | Some o, _ -> Test.Decoded o
        | _ -> failwith "cannot find expected result"
      in
      { Test.hex; raw; expect; roundtrip })
    l

(* a few tests we need to skip *)
let skip =
  [
    "c249010000000000000000", "(bigint)";
    "1BFFFFFFFFFFFFFFFF", "(requires int64, loss of precision)";
    "3bffffffffffffffff", "(requires int64, loss of precision)";
    "1bffffffffffffffff", "(requires int64 loss of precision)";
    "5f42010243030405ff", "(requires representation of indefinite length)";
  ]

type count = {
  mutable n_ok: int;
  mutable n_err: int;
  mutable n_skip: int;
}

let run_test (c : count) (t : Test.t) : unit =
  try
    match Cbor.decode_exn t.raw with
    | exception e ->
      c.n_err <- c.n_err + 1;
      Fmt.printf "error when decoding %S: %s@." t.hex (Printexc.to_string e)
    | cbor ->
      if verbose then Fmt.printf "  decoded into %a@." Cbor.pp_diagnostic cbor;

      (* do we skip the rest of the test? *)
      (match List.assoc_opt t.hex skip with
      | Some reason ->
        c.n_skip <- 1 + c.n_skip;
        if verbose then
          Fmt.printf "> @{<Yellow>SKIP@} %S (reason: %s)@." t.hex reason
      | None ->
        if verbose then Fmt.printf "> RUN test %S@." t.hex;

        (* check roundtrip, except on floats because we always use float64 *)
        if
          t.roundtrip
          &&
          match cbor with
          | `Float _ -> false
          | _ -> true
        then (
          let hex' = Cbor.encode cbor |> CCString.to_hex in
          if hex' <> t.hex then (
            Fmt.printf
              "  @[<v>@{<Red>mismatch@} on roundtrip:@ from %S@ to %S@]@." t.hex
              hex';
            c.n_err <- c.n_err + 1;
            raise Exit
          ) else if verbose then
            Fmt.printf "  roundtrip ok@."
        );

        (match t.expect with
        | Test.Diagnostic s ->
          let s' = Cbor.to_string_diagnostic cbor in
          (* adjust display *)
          let s' =
            match s' with
            | "inf" -> "Infinity"
            | "-inf" -> "-Infinity"
            | "nan" -> "NaN"
            | _ -> s'
          in
          if s = s' then (
            c.n_ok <- c.n_ok + 1;
            if verbose then Fmt.printf "  @{<Green>OK@}@."
          ) else (
            Fmt.printf "  @{<Red>ERR@}: expected diagnostic %S, got %S@." s s';
            c.n_err <- c.n_err + 1
          )
        | Test.Decoded j ->
          let rec compare_cj (cbor : Cbor.t) (j : json) =
            match cbor, j with
            | `Null, `Null -> true
            | `Float f1, `Float f2 -> Float.equal f1 f2
            | `Bool b1, `Bool b2 -> b1 = b2
            | `Map l, `Assoc l2 ->
              List.for_all
                (fun (k, v) ->
                  try compare_cj (List.assoc (`Text k) l) v
                  with Not_found -> false)
                l2
            | `Int i, `Int j -> i = Int64.of_int j
            | `Text s1, `String s2 -> s1 = s2
            | `Array l1, `List l2 ->
              List.length l1 = List.length l2 && List.for_all2 compare_cj l1 l2
            | `Int i, `Intlit s -> Int64.to_string i = s
            | _, `Intlit "-18446744073709551617" ->
              (* skip bigint test*)
              true
            | _ ->
              Fmt.printf "  TODO: compare %a with %a@." Cbor.pp_diagnostic cbor
                J.pp j;
              true
          in

          let ok = compare_cj cbor j in

          if ok then (
            c.n_ok <- 1 + c.n_ok;
            if verbose then Fmt.printf "  expect: @{<Green>OK@}@."
          ) else (
            c.n_err <- 1 + c.n_err;
            Fmt.printf "  expect: @{<Red>ERROR@} (got %a, expected %a)@."
              Cbor.pp_diagnostic cbor J.pp j
          )))
  with Exit -> ()

let run_tests (l : Test.t list) =
  let c = { n_err = 0; n_ok = 0; n_skip = 0 } in
  List.iter (run_test c) l;
  let has_err = c.n_err <> 0 in

  let total = c.n_err + c.n_ok + c.n_skip in
  if (verbose || has_err) && total <> List.length l then
    Fmt.printf "@{<Blue>warning@}: ran %d tests, for list of %d tests@." total
      (List.length l);

  if has_err then (
    Fmt.printf "@.@.#####@.@{<Red>FAIL@}: %d errors, %d ok, %d skip@." c.n_err
      c.n_ok c.n_skip;
    exit 1
  ) else
    Fmt.printf "@.@.#####@.@{<Green>OK@}: %d ok, %d skip@." c.n_ok c.n_skip

let () =
  let color = try Sys.getenv "COLOR" = "1" with _ -> false in
  if color then CCFormat.set_color_default true;
  let content = CCIO.File.read_exn Sys.argv.(1) in
  let j = Yojson.Safe.from_string content in
  let tests = extract_tests j in
  (*Format.printf "tests: %a@." (Fmt.Dump.list Test.pp) tests;*)
  run_tests tests;
  ()
