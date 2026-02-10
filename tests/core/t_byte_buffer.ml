module T = (val Containers_testlib.make ~__FILE__ ())
include T
open CCByte_buffer;;

t @@ fun () ->
let b = create () in
is_empty b
;;

t @@ fun () ->
let b = create ~cap:32 () in
is_empty b
;;

t @@ fun () ->
let b = create () in
length b = 0
;;

t @@ fun () ->
let b = create ~cap:32 () in
length b = 0

let test_count = 2_500

open QCheck

type op =
  | Add_char of char
  | Add_string of string
  | Get_contents
  | Get of int
  | Clear
  | Shrink_to of int
  | Set of int * char

let spf = Printf.sprintf

let str_op = function
  | Add_char c -> spf "add_char %C" c
  | Add_string s -> spf "add_string %S" s
  | Get_contents -> "contents"
  | Get i -> spf "get %d" i
  | Clear -> "clear"
  | Shrink_to n -> spf "shrink %d" n
  | Set (i, c) -> spf "set %d %C" i c

let gen_op size : (_ * _) Gen.t =
  let open Gen in
  let base =
    if size > 0 then
      [
        (1, 0 -- size >|= fun x -> Get x, size);
        ( 1,
          0 -- size >>= fun x ->
          printable >|= fun c -> Set (x, c), size );
        (1, 0 -- size >|= fun x -> Shrink_to x, x);
      ]
    else
      []
  in
  oneof_weighted
    (base
    @ [
        1, return (Get_contents, size);
        1, return (Clear, 0);
        (3, printable >|= fun c -> Add_char c, size + 1);
        ( 1,
          string_size (0 -- 100) ~gen:printable >|= fun s ->
          Add_string s, size + String.length s );
      ])

let rec gen_l acc sz n =
  let open Gen in
  if n = 0 then
    return (List.rev acc)
  else
    gen_op sz >>= fun (op, sz) -> gen_l (op :: acc) sz (n - 1)

let gen : op list Gen.t = Gen.sized (gen_l [] 0)

let is_valid ops =
  let rec loop sz = function
    | [] -> true
    | Add_char _ :: tl -> loop (sz + 1) tl
    | Clear :: tl -> loop 0 tl
    | Add_string s :: tl -> loop (sz + String.length s) tl
    | (Get n | Set (n, _)) :: tl -> n < sz && loop sz tl
    | Get_contents :: tl -> loop sz tl
    | Shrink_to x :: tl -> x <= sz && loop x tl
  in
  loop 0 ops

let shrink_op =
  Iter.(
    function
    | Get_contents | Clear -> empty
    | Get n -> Shrink.int n >|= fun n -> Get n
    | Add_char c -> Shrink.char c >|= fun c -> Add_char c
    | Add_string s -> Shrink.string s >|= fun s -> Add_string s
    | Shrink_to n -> Shrink.int n >|= fun n -> Shrink_to n
    | Set (n, c) ->
      Shrink.int n
      >|= (fun n -> Set (n, c))
      <+> (Shrink.char c >|= fun c -> Set (n, c)))

let arb =
  make gen ~print:(Print.list str_op)
    ~shrink:Shrink.(filter is_valid @@ list ~shrink:shrink_op)

exception Nope of string

let prop_consistent ops =
  let buf = ref "" in
  let b = create ~cap:32 () in

  let run_op op =
    match op with
    | Get i ->
      assert (String.length !buf = length b);
      let c1 = !buf.[i] in
      let c2 = get b i in
      if c1 <> c2 then raise (Nope (spf "c1=%C, c2=%C" c1 c2))
    | Get_contents ->
      let s1 = !buf in
      let s2 = contents b in
      if s1 <> s2 then raise (Nope (spf "s1=%S, s2=%S" s1 s2))
    | Add_char c ->
      buf := !buf ^ String.make 1 c;
      add_char b c
    | Add_string s ->
      buf := !buf ^ s;
      append_string b s
    | Clear ->
      buf := "";
      clear b
    | Shrink_to n ->
      buf := String.sub !buf 0 n;
      shrink_to b n
    | Set (n, c) ->
      (let b' = Bytes.of_string !buf in
       Bytes.set b' n c;
       buf := Bytes.unsafe_to_string b');
      set b n c
  in

  assume (is_valid ops);
  try
    List.iter run_op ops;
    true
  with Nope str -> Test.fail_reportf "consistent ops failed:\n%s" str
;;

q arb (fun ops -> prop_consistent ops)
