include (val Containers_testlib.make ~__FILE__ ())
module Cbor = Containers_cbor

let gen_c : Cbor.t Q.Gen.t =
  let open Q.Gen in
  sized @@ fix
  @@ fun self size ->
  let recurse = self (size - 1) in
  let base =
    [
      1, return `Null;
      1, return `Undefined;
      ( 3,
        let+ x = int >|= Int64.of_int in
        `Int x );
      ( 1,
        let+ b = bool in
        `Bool b );
      ( 1,
        let+ x = 0 -- 19 in
        `Simple x );
      ( 1,
        let+ x = 26 -- 127 in
        `Simple x );
      ( 1,
        let+ f = float in
        `Float f );
      ( 2,
        let* n = oneof_weighted [ 20, 0 -- 150; 1, 151 -- 100_000 ] in
        let+ s = string_size ~gen:printable (return n) in
        `Text s );
      ( 2,
        let* n = oneof_weighted [ 20, 0 -- 150; 1, 151 -- 100_000 ] in
        let+ s = string_size ~gen:char (return n) in
        `Bytes s );
    ]
  in
  let g_base = oneof_weighted base in
  let rec_ =
    [
      ( 2,
        let+ l =
          if size > 10 then
            list_size (0 -- 1024) g_base
          else
            list_size (0 -- 10) recurse
        in
        `Array l );
      ( 2,
        let+ l =
          if size > 10 then
            list_size (0 -- 1024) (pair g_base g_base)
          else
            list_size (0 -- 5) (pair g_base recurse)
        in
        `Map l );
      ( 1,
        let+ i = 0 -- 1024 and+ sub = self (size - 1) in
        `Tag (i, sub) );
    ]
  in
  oneof_weighted
    (if size > 0 then
       base @ rec_
     else
       base)

let rec shrink (c : Cbor.t) : Cbor.t Q.Iter.t =
  let open Q.Iter in
  match c with
  | `Null | `Undefined | (`Bool false) -> empty
  | (`Bool true) -> return ((`Bool false))
  | `Simple i ->
    let+ i = Q.Shrink.int i in
    `Simple i
  | `Int i ->
    let+ i = Q.Shrink.int (Int64.to_int i) in
    `Int (Int64.of_int i)
  | `Tag (t, i) ->
    let+ i = shrink i in
    `Tag (t, i)
  | `Float _ -> empty
  | `Array l ->
    let+ l = Q.Shrink.list ~shrink l in
    `Array l
  | `Map l ->
    let shrink_pair (a, b) =
      (let+ a = shrink a in
       a, b)
      <+> let+ b = shrink b in
          a, b
    in
    let+ l = Q.Shrink.list ~shrink:shrink_pair l in
    `Map l
  | `Text s ->
    let+ s = Q.Shrink.string s in
    `Text s
  | `Bytes s ->
    let+ s = Q.Shrink.string s in
    `Bytes s

let arb = Q.make ~shrink ~print:Cbor.to_string_diagnostic gen_c

let rec eq_c c c' =
  match c, c' with
  | `Null, `Null | `Undefined, `Undefined -> true
  | `Simple i, `Simple i' -> Int.equal i i'
  | `Bool b, `Bool b' -> Bool.equal b b'
  | `Int i, `Int i' -> Int64.equal i i'
  | `Float f, `Float f' -> Float.equal f f'
  | `Bytes s, `Bytes s' -> String.equal s s'
  | `Text t, `Text t' -> String.equal t t'
  | `Array a, `Array a' -> CCList.equal eq_c a a'
  | `Map m, `Map m' ->
    CCList.equal (fun (t0, t1) (t0', t1') -> eq_c t0 t0' && eq_c t1 t1') m m'
  | `Tag (i, t), `Tag (i', t') -> Int.equal i i' && eq_c t t'
  | _ -> false
;;

q ~count:1_000 ~long_factor:10 arb @@ fun c ->
let s = Cbor.encode c in
let c' = Cbor.decode_exn s in
if not (eq_c c c') then
  Q.Test.fail_reportf "@[<hv2>roundtrip failed:@ from %a@ to %a@]"
    Cbor.pp_diagnostic c Cbor.pp_diagnostic c';
true;;

(* Additional edge case and error handling tests *)

(* Test basic encoding/decoding *)
t @@ fun () -> Cbor.decode_exn (Cbor.encode `Null) = `Null;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode `Undefined) = `Undefined;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Bool true)) = (`Bool true);;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Bool false)) = (`Bool false);;

(* Test integer edge cases *)
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int 0L)) = `Int 0L;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int 23L)) = `Int 23L;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int 24L)) = `Int 24L;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int 255L)) = `Int 255L;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int 256L)) = `Int 256L;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int 65535L)) = `Int 65535L;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int 65536L)) = `Int 65536L;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int Int64.max_int)) = `Int Int64.max_int;;

(* Test negative integers *)
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int (-1L))) = `Int (-1L);;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int (-23L))) = `Int (-23L);;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int (-24L))) = `Int (-24L);;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int (-256L))) = `Int (-256L);;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Int Int64.min_int)) = `Int Int64.min_int;;

(* Test floats *)
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Float 0.0)) = `Float 0.0;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Float 1.5)) = `Float 1.5;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Float (-1.5))) = `Float (-1.5);;
t @@ fun () ->
  let result = Cbor.decode_exn (Cbor.encode (`Float infinity)) in
  match result with
  | `Float f -> classify_float f = FP_infinite && f > 0.0
  | _ -> false
;;
t @@ fun () ->
  let result = Cbor.decode_exn (Cbor.encode (`Float neg_infinity)) in
  match result with
  | `Float f -> classify_float f = FP_infinite && f < 0.0
  | _ -> false
;;
t @@ fun () ->
  let result = Cbor.decode_exn (Cbor.encode (`Float nan)) in
  match result with
  | `Float f -> classify_float f = FP_nan
  | _ -> false
;;

(* Test strings *)
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Text "")) = `Text "";;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Text "hello")) = `Text "hello";;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Text "a")) = `Text "a";;
t @@ fun () ->
  let long = String.make 1000 'x' in
  Cbor.decode_exn (Cbor.encode (`Text long)) = `Text long
;;

(* Test UTF-8 strings *)
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Text "hello 世界")) = `Text "hello 世界";;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Text "émoji 🎉")) = `Text "émoji 🎉";;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Text "Здравствуй")) = `Text "Здравствуй";;

(* Test bytes *)
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Bytes "")) = `Bytes "";;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Bytes "\x00\x01\x02")) = `Bytes "\x00\x01\x02";;
t @@ fun () ->
  let bytes = String.init 256 char_of_int in
  Cbor.decode_exn (Cbor.encode (`Bytes bytes)) = `Bytes bytes
;;

(* Test arrays *)
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Array [])) = `Array [];;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Array [`Int 1L])) = `Array [`Int 1L];;
t @@ fun () ->
  Cbor.decode_exn (Cbor.encode (`Array [`Int 1L; `Int 2L; `Int 3L]))
  = `Array [`Int 1L; `Int 2L; `Int 3L]
;;
t @@ fun () ->
  Cbor.decode_exn (Cbor.encode (`Array [(`Bool true); `Text "a"; `Int 42L]))
  = `Array [(`Bool true); `Text "a"; `Int 42L]
;;

(* Test nested arrays *)
t @@ fun () ->
  let nested = `Array [`Array [`Int 1L; `Int 2L]; `Array [`Int 3L]] in
  Cbor.decode_exn (Cbor.encode nested) = nested
;;

(* Test maps *)
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Map [])) = `Map [];;
t @@ fun () ->
  Cbor.decode_exn (Cbor.encode (`Map [(`Text "key", `Int 42L)]))
  = `Map [(`Text "key", `Int 42L)]
;;
t @@ fun () ->
  let map = `Map [
    (`Text "a", `Int 1L);
    (`Text "b", `Int 2L);
    (`Text "c", `Int 3L)
  ] in
  Cbor.decode_exn (Cbor.encode map) = map
;;

(* Test maps with various key types *)
t @@ fun () ->
  let map = `Map [
    (`Int 0L, `Text "zero");
    (`Int 1L, `Text "one");
  ] in
  Cbor.decode_exn (Cbor.encode map) = map
;;

(* Test tags *)
t @@ fun () ->
  Cbor.decode_exn (Cbor.encode (`Tag (0, `Text "2013-03-21")))
  = `Tag (0, `Text "2013-03-21")
;;
t @@ fun () ->
  Cbor.decode_exn (Cbor.encode (`Tag (1, `Int 1363896240L)))
  = `Tag (1, `Int 1363896240L)
;;
t @@ fun () ->
  Cbor.decode_exn (Cbor.encode (`Tag (32, `Text "http://example.com")))
  = `Tag (32, `Text "http://example.com")
;;

(* Test simple values *)
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Simple 0)) = `Simple 0;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Simple 19)) = `Simple 19;;
t @@ fun () -> Cbor.decode_exn (Cbor.encode (`Simple 255)) = `Simple 255;;

(* Test error cases *)
t @@ fun () ->
  match Cbor.decode "" with
  | Error _ -> true
  | Ok _ -> false
;;

t @@ fun () ->
  match Cbor.decode "\x1f" with  (* invalid additional info *)
  | Error _ -> true
  | Ok _ -> false
;;

t @@ fun () ->
  match Cbor.decode "\x1c" with  (* reserved additional info *)
  | Error _ -> true
  | Ok _ -> false
;;

t @@ fun () ->
  match Cbor.decode "\x5f\x42\x01\x02\x43\x03\x04\x05" with  (* incomplete indefinite *)
  | Error _ -> true
  | Ok _ -> false
;;

(* Test that decode_exn raises on invalid input *)
t @@ fun () ->
  try
    ignore (Cbor.decode_exn "");
    false
  with Failure _ -> true
;;

t @@ fun () ->
  try
    ignore (Cbor.decode_exn "\x1c");
    false
  with Failure _ -> true
;;

(* Test diagnostic string output *)
t @@ fun () -> Cbor.to_string_diagnostic `Null = "null";;
t @@ fun () -> Cbor.to_string_diagnostic `Undefined = "undefined";;
t @@ fun () -> Cbor.to_string_diagnostic ((`Bool true)) = "true";;
t @@ fun () -> Cbor.to_string_diagnostic ((`Bool false)) = "false";;
t @@ fun () -> Cbor.to_string_diagnostic (`Int 42L) = "42";;
t @@ fun () -> Cbor.to_string_diagnostic (`Int (-42L)) = "-42";;
t @@ fun () -> Cbor.to_string_diagnostic (`Float 1.5) = "1.5";;
t @@ fun () -> Cbor.to_string_diagnostic (`Text "hello") = "\"hello\"";;
t @@ fun () -> Cbor.to_string_diagnostic (`Array [`Int 1L; `Int 2L]) = "[1, 2]";;
t @@ fun () ->
  Cbor.to_string_diagnostic (`Map [(`Text "a", `Int 1L)])
  |> String.contains_s ~sub:"\"a\""
;;

(* Test deeply nested structures *)
t @@ fun () ->
  let rec make_nested n =
    if n = 0 then `Int 0L
    else `Array [make_nested (n - 1)]
  in
  let nested = make_nested 100 in
  Cbor.decode_exn (Cbor.encode nested) = nested
;;

(* Test large collections *)
t @@ fun () ->
  let large_array = `Array (List.init 1000 (fun i -> `Int (Int64.of_int i))) in
  Cbor.decode_exn (Cbor.encode large_array) = large_array
;;

t @@ fun () ->
  let large_map = `Map (List.init 500 (fun i ->
    (`Int (Int64.of_int i), `Text (string_of_int i))
  )) in
  Cbor.decode_exn (Cbor.encode large_map) = large_map
;;

(* Test mixed nested structures *)
t @@ fun () ->
  let complex = `Map [
    (`Text "array", `Array [`Int 1L; `Int 2L; `Int 3L]);
    (`Text "map", `Map [(`Text "nested", (`Bool true))]);
    (`Text "tagged", `Tag (42, `Text "value"));
    (`Text "null", `Null);
  ] in
  Cbor.decode_exn (Cbor.encode complex) = complex
;;

(* Test that encoding is consistent *)
t @@ fun () ->
  let c = `Map [(`Text "a", `Int 1L); (`Text "b", `Int 2L)] in
  let e1 = Cbor.encode c in
  let e2 = Cbor.encode c in
  e1 = e2
;;

(* Test buffer reuse *)
t @@ fun () ->
  let buf = Buffer.create 16 in
  let _ = Cbor.encode ~buf (`Int 1L) in
  let s1 = Buffer.contents buf in
  Buffer.clear buf;
  let _ = Cbor.encode ~buf (`Int 1L) in
  let s2 = Buffer.contents buf in
  s1 = s2
;;

(* Property: encoding then decoding gives original value *)
q ~count:5000 arb @@ fun c ->
  match Cbor.decode (Cbor.encode c) with
  | Ok c' -> eq_c c c'
  | Error e ->
    Q.Test.fail_reportf "decode failed: %s" e;
    false
;;

(* Property: decode result equality *)
q ~count:2000 arb @@ fun c ->
  let s = Cbor.encode c in
  match Cbor.decode s with
  | Error e ->
    Q.Test.fail_reportf "decode failed on encoded value: %s" e;
    false
  | Ok c1 ->
    match Cbor.decode s with
    | Error _ -> false
    | Ok c2 -> eq_c c1 c2
;;

(* Property: diagnostic string doesn't crash *)
q ~count:1000 arb @@ fun c ->
  let _ = Cbor.to_string_diagnostic c in
  true
;;

(* Property: encoding size is reasonable *)
q ~count:1000 arb @@ fun c ->
  let s = Cbor.encode c in
  String.length s < 1_000_000  (* Sanity check *)
;;
