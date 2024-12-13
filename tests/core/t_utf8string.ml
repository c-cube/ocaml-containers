open CCUtf8_string
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

eq ~cmp:( = )
  ~printer:Q.Print.(list (fun c -> string_of_int @@ Uchar.to_int c))
  (to_list (of_string_exn "aébõ😀"))
  (to_seq (of_string_exn "aébõ😀") |> CCList.of_seq)
;;

(* make sure it's persisted correctly *)
t @@ fun () ->
let s = of_string_exn "aébõ😀" in
let seq = to_seq s in
let l = to_list s in
let testeq seq = assert_equal ~cmp:( = ) l (CCList.of_seq seq) in
testeq seq;
testeq seq;
testeq seq;
true

let printer s = String.escaped (to_string s)
let pp_uchar (c : Uchar.t) = Printf.sprintf "0x%x" (Uchar.to_int c)

let arb_uchar =
  let rec gen =
    lazy
      (let open Q.Gen in
       Q.Gen.int_range Uchar.(to_int min) Uchar.(to_int max) >>= fun n ->
       try return (Uchar.of_int n) with _ -> Lazy.force gen)
  in
  Q.make
    ~print:(fun c -> Printf.sprintf "<uchar '%d'>" (Uchar.to_int c))
    (Lazy.force gen)

let uutf_is_valid s =
  try
    Uutf.String.fold_utf_8
      (fun () _ -> function
        | `Malformed _ -> raise Exit
        | `Uchar _ -> ())
      () s;
    true
  with Exit -> false

let uutf_to_iter s f =
  Uutf.String.fold_utf_8
    (fun () _ -> function
      | `Malformed _ -> f (Uchar.of_int 0xfffd)
      | `Uchar c -> f c)
    () s

let uutf_of_l l =
  let buf = Buffer.create 32 in
  List.iter (Uutf.Buffer.add_utf_8 buf) l;
  Buffer.contents buf
;;

t @@ fun () ->
let s = of_string_exn "このため、" in
let s' = to_iter s |> of_iter in
assert_equal ~cmp:equal ~printer s s';
true
;;

q Q.small_string (fun s ->
    Q.assume (CCString.for_all (fun c -> Char.code c < 128) s);
    is_valid s)
;;

q ~long_factor:10 Q.small_string (fun s ->
    Q.assume (CCString.for_all (fun c -> Char.code c < 128) s);
    s = (of_string_exn s |> to_iter |> of_iter |> to_string))
;;

q ~long_factor:10 Q.string (fun s ->
    Q.assume (CCString.for_all (fun c -> Char.code c < 128) s);
    String.length s = List.length (of_string_exn s |> to_list))
;;

q ~long_factor:10 ~count:20_000
  Q.(small_list arb_uchar)
  (fun l ->
    let s = of_list l in
    l = to_list s)
;;

q ~long_factor:10
  Q.(small_list arb_uchar)
  (fun l ->
    let s = of_list l in
    l = to_list @@ of_gen @@ to_gen s)
;;

q ~long_factor:10
  Q.(small_list arb_uchar)
  (fun l ->
    let s = of_list l in
    l = to_list @@ of_iter @@ to_iter s)
;;

t @@ fun () -> not (is_valid "\192\181");;
t @@ fun () -> not (is_valid "\193\143");;
t @@ fun () -> not (is_valid "\224\151\167");;
t @@ fun () -> not (is_valid "\224\137\165");;
t @@ fun () -> is_valid "\240\151\189\163";;

q ~long_factor:40 Q.string (fun s ->
    Q.assume (is_valid s);
    let s = of_string_exn s in
    let s2 = s |> to_iter |> of_iter in
    if s = s2 then
      true
    else
      Q.Test.fail_reportf "s=%S, s2=%S" (to_string s) (to_string s2))
;;

q ~long_factor:40 Q.string (fun s ->
    Q.assume (is_valid s);
    let s = of_string_exn s in
    let s2 = s |> to_gen |> of_gen in
    if s = s2 then
      true
    else
      Q.Test.fail_reportf "s=%S, s2=%S" (to_string s) (to_string s2))
;;

(* compare with uutf *)

q ~long_factor:40 ~count:50_000 Q.small_string (fun s ->
    let v1 = is_valid s in
    let v2 = uutf_is_valid s in
    if v1 = v2 then
      true
    else
      Q.Test.fail_reportf "s:%S, valid: %B, uutf_valid: %B" s v1 v2)
;;

q ~long_factor:40 ~count:50_000
  Q.(small_list arb_uchar)
  (fun l ->
    let pp s = Q.Print.(list pp_uchar) s in
    let uutf = uutf_of_l l in
    let s = (of_list l :> string) in
    if uutf = s then
      true
    else
      Q.Test.fail_reportf "l: '%s', uutf: '%s', containers: '%s'" (pp l) uutf s)
;;

q ~long_factor:40 ~count:50_000 Q.small_string (fun s ->
    Q.assume (is_valid s && uutf_is_valid s);
    let pp s = Q.Print.(list pp_uchar) s in
    let l_uutf = uutf_to_iter s |> Iter.to_list in
    let l_co = of_string_exn s |> to_iter |> Iter.to_list in
    if l_uutf = l_co then
      true
    else
      Q.Test.fail_reportf
        "uutf: '%s', containers: '%s', is_valid %B, uutf_is_valid %B"
        (pp l_uutf) (pp l_co) (is_valid s) (uutf_is_valid s))
;;

t @@ fun () ->
for i = 0 to 127 do
  let c = Uchar.of_int i in
  assert_equal 1 (n_bytes (of_list [ c ]))
done;
true
;;

q
  Q.(small_list arb_uchar)
  (fun l -> of_list l = concat empty (List.map of_uchar l))
;;

q
  Q.(pair small_nat arb_uchar)
  (fun (i, c) -> make i c = concat empty (CCList.init i (fun _ -> of_uchar c)))
