
include (val Containers_testlib.make ~__FILE__ ())
module Cbor = Containers_cbor;;

[@@@ifge 4.08];;

let gen_c : Cbor.t Q.Gen.t =
  let open Q.Gen in
  sized @@ fix @@ fun self size ->
    let recurse = self (size-1) in
    let base = [
      (1, return `Null);
      (1, return `Undefined);
      (3, let+ x = int in `Int x);
      (1, let+ b = bool in `Bool b);
      (1, let+ x = 0 -- 19 in `Simple x);
      (1, let+ x = 26 -- 127 in `Simple x);
      (1, let+ f = float in `Float f);
      (2, let+ s = string_size ~gen:printable (0--150) in `Text s);
      (2, let+ s = string_size ~gen:char (0--150) in `Bytes s);
    ] in
    let g_base = frequency base in
    let rec_ = [
      (2, let+ l =
          if size>10 then list_size (0--1024) g_base
          else list_size (0--10) recurse
      in `Array l);
      (2, let+ l =
          if size>10 then list_size (0--1024) (pair g_base g_base)
          else list_size (0--5) (pair g_base recurse)
      in `Map l);
    ]in
    frequency (if size>0  then base @ rec_ else base)

let rec shrink (c:Cbor.t) : Cbor.t Q.Iter.t =
  let open Q.Iter in
  match c with
  | `Null | `Undefined | `Bool false -> empty
  | `Bool true -> return (`Bool false)
  | `Simple i -> let+ i = Q.Shrink.int i in `Simple i
  | `Int i -> let+ i = Q.Shrink.int i in `Int i
  | `Tag(t,i) -> let+ i = shrink i in `Tag (t,i)
  | `Float _ -> empty
  | `Array l ->
    let+ l = Q.Shrink.list ~shrink l in `Array l
  | `Map l ->
    let shrink_pair (a,b) =
      (let+a = shrink a in a,b) <+> (let+b = shrink b in a,b)
    in
    let+ l = Q.Shrink.list ~shrink:shrink_pair l in `Map l
  | `Text s ->
    let+ s = Q.Shrink.string s in `Text s
  | `Bytes s ->
    let+ s = Q.Shrink.string s in `Bytes s

let arb = Q.make ~shrink ~print:Cbor.to_string_diagnostic gen_c;;

q ~count:10_000 arb @@ fun c ->
  let s = Cbor.encode c in
  let c' = Cbor.decode_exn s in
  if not (c = c') then
    Q.Test.fail_reportf "@[<hv2>roundtrip failed:@ from %a@ to %a@]"
    Cbor.pp_diagnostic c Cbor.pp_diagnostic c';
  true;;

[@@@endif]
