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
        let* n = frequency [ 20, 0 -- 150; 1, 151 -- 100_000 ] in
        let+ s = string_size ~gen:printable (return n) in
        `Text s );
      ( 2,
        let* n = frequency [ 20, 0 -- 150; 1, 151 -- 100_000 ] in
        let+ s = string_size ~gen:char (return n) in
        `Bytes s );
    ]
  in
  let g_base = frequency base in
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
  frequency
    (if size > 0 then
       base @ rec_
     else
       base)

let rec shrink (c : Cbor.t) : Cbor.t Q.Iter.t =
  let open Q.Iter in
  match c with
  | `Null | `Undefined | `Bool false -> empty
  | `Bool true -> return (`Bool false)
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

let arb = Q.make ~shrink ~print:Cbor.to_string_diagnostic gen_c;;

let rec eq_c c c' = match c,c' with
  | `Null, `Null
  | `Undefined, `Undefined -> true
  | `Simple i, `Simple i' -> Int.equal i i'
  | `Bool b, `Bool b' -> Bool.equal b b'
  | `Int i, `Int i' -> Int64.equal i i'
  | `Float f, `Float f' -> Float.equal f f'
  | `Bytes s, `Bytes s' -> String.equal s s'
  | `Text t, `Text t' -> String.equal t t'
  | `Array a, `Array a' -> List.equal eq_c a a'
  | `Map m, `Map m' ->
    List.equal (fun (t0,t1) (t0',t1') -> eq_c t0 t0' && eq_c t1 t1') m m'
  | `Tag (i,t), `Tag (i',t') -> Int.equal i i' && eq_c t t'
  | _ -> false;;

q ~count:1_000 ~long_factor:10 arb @@ fun c ->
let s = Cbor.encode c in
let c' = Cbor.decode_exn s in
if not (eq_c c c') then
  Q.Test.fail_reportf "@[<hv2>roundtrip failed:@ from %a@ to %a@]"
    Cbor.pp_diagnostic c Cbor.pp_diagnostic c';
true
