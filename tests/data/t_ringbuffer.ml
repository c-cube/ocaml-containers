module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCRingBuffer
open Q.Gen

let g_char = map Char.chr (Char.code 'A' -- Char.code 'z')
let g_str = string_size ~gen:g_char (0 -- 10)
let a_str = Q.set_gen g_str Q.string;;

t @@ fun () ->
let b = Byte.of_array (Bytes.of_string "abc") in
let b' = Byte.copy b in
Byte.clear b;
Byte.to_array b' = Bytes.of_string "abc" && Byte.to_array b = Bytes.empty
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    Byte.capacity b >= s_len)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    let b' = Byte.copy b in
    try
      Byte.iteri b ~f:(fun i c -> if Byte.get_front b' i <> c then raise Exit);
      true
    with Exit -> false)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    Byte.push_back b 'X';
    Byte.peek_back_exn b = 'X')
;;

q (Q.pair a_str a_str) (fun (s, s') ->
    let b = Byte.create (max (String.length s + String.length s') 64) in
    let s = Bytes.of_string s in
    let s' = Bytes.of_string s' in
    Byte.blit_from b s 0 (Bytes.length s);
    Byte.blit_from b s' 0 (Bytes.length s');
    Byte.length b = Bytes.length s + Bytes.length s')
;;

q (Q.pair a_str a_str) (fun (s, s') ->
    let s = Bytes.of_string s in
    let s' = Bytes.of_string s' in
    let b = Byte.create (max (Bytes.length s + Bytes.length s') 64) in
    Byte.blit_from b s 0 (Bytes.length s);
    Byte.blit_from b s' 0 (Bytes.length s');
    Byte.length b = Bytes.length s + Bytes.length s')
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let b = Byte.create (max 64 (Bytes.length s)) in
    Byte.blit_from b s 0 (Bytes.length s);
    let to_buf = Bytes.create (Bytes.length s) in
    let len = Byte.blit_into b to_buf 0 (Bytes.length s) in
    to_buf = s && len = Bytes.length s)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    Byte.skip b s_len;
    Byte.is_empty b)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    try
      let front = Byte.take_front_exn b in
      front = Bytes.get s 0
    with Byte.Empty -> s_len = 0)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    try
      let back = Byte.take_back_exn b in
      back = Bytes.get s (Bytes.length s - 1)
    with Byte.Empty -> s_len = 0)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    try
      let () = Byte.junk_front b in
      s_len - 1 = Byte.length b
    with Byte.Empty -> s_len = 0)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    try
      let () = Byte.junk_back b in
      s_len - 1 = Byte.length b
    with Byte.Empty -> s_len = 0)
;;

q (Q.pair a_str a_str) (fun (s, s') ->
    let s = Bytes.of_string s in
    let s' = Bytes.of_string s' in
    let b = Byte.create (max (Bytes.length s + Bytes.length s') 64) in
    Byte.blit_from b s 0 (Bytes.length s);
    Byte.blit_from b s' 0 (Bytes.length s');
    let h = Bytes.of_string "hello world" in
    Byte.blit_from b h 0 (Bytes.length h);
    (* big enough *)
    let l = Byte.length b in
    let l' = l / 2 in
    Byte.skip b l';
    Byte.length b + l' = l)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    Byte.clear b;
    Byte.length b = 0)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    try
      Byte.iteri b ~f:(fun i c -> if Byte.get_front b i <> c then raise Exit);
      true
    with Exit -> false)
;;

q (Q.pair Q.small_int a_str) (fun (i, s) ->
    let s = Bytes.of_string (s ^ " ") in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    let index = abs (i mod Byte.length b) in
    let front = Byte.get_front b index in
    front = Bytes.get s index)
;;

q (Q.pair Q.small_int a_str) (fun (i, s) ->
    let s = Bytes.of_string (s ^ " ") in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    let index = abs (i mod Byte.length b) in
    let back = Byte.get_back b index in
    back = Bytes.get s (s_len - index - 1))
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    let l = Byte.to_list b in
    let explode s =
      let rec exp i l =
        if i < 0 then
          l
        else
          exp (i - 1) (Bytes.get s i :: l)
      in
      exp (Bytes.length s - 1) []
    in
    explode s = l)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    try
      let back = Byte.peek_front_exn b in
      back = Bytes.get s 0
    with Byte.Empty -> s_len = 0)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let s_len = Bytes.length s in
    let b = Byte.create (max s_len 64) in
    Byte.blit_from b s 0 s_len;
    try
      let back = Byte.peek_back_exn b in
      back = Bytes.get s (s_len - 1)
    with Byte.Empty -> s_len = 0)
;;

q a_str (fun s ->
    let s = Bytes.of_string s in
    let b = Byte.of_array s in
    let s' = Byte.to_array b in
    s = s')

module BI = CCRingBuffer.Make (struct
  type t = int

  let dummy = 0
end)
;;

(* try to trigger an error on resize
   see issue #126 *)
t @@ fun () ->
let b = BI.create 50 in
let st = Random.State.make [| 0 |] in
for _i = 1 to 100_000 do
  if Random.State.float st 1.0 < 0.5 then
    BI.push_back b 0
  else (
    let _ = BI.take_front b in
    ()
  )
done;
true

(* Test against reference implementation (lists) on a succession of
   operations.

   Remarks on semantics:

   JUNK_FRONT/JUNK_BACK: try to remove if not empty
   SKIP: if at least n elements, skip; else nop
*)

module BS = CCRingBuffer.Byte

type op =
  | Push_back of char
  | Take_front
  | Take_back
  | Peek_front
  | Peek_back
  | Junk_front
  | Junk_back
  | Skip of int
  | Blit of string * int * int
  | Z_if_full

let str_of_op = function
  | Push_back c -> Printf.sprintf "push_back(%C)" c
  | Take_front -> Printf.sprintf "take_front"
  | Take_back -> Printf.sprintf "take_back"
  | Peek_front -> Printf.sprintf "peek_front"
  | Peek_back -> Printf.sprintf "peek_back"
  | Junk_front -> Printf.sprintf "junk_front"
  | Junk_back -> Printf.sprintf "junk_back"
  | Skip n -> Printf.sprintf "skip(%d)" n
  | Blit (s, i, len) -> Printf.sprintf "blit(%S,%d,%d)" s i len
  | Z_if_full -> "zero_if_full"

let push_back c = Push_back c

let skip n =
  assert (n >= 0);
  Skip n

let blit s i len =
  if i < 0 || len < 0 || i + len > String.length s then
    failwith ("wrong blit: " ^ str_of_op (Blit (s, i, len)));
  Blit (s, i, len)

let shrink_op =
  let open Q.Iter in
  function
  | Push_back c -> Q.Shrink.char c >|= push_back
  | Take_front | Take_back | Junk_back | Junk_front | Z_if_full | Peek_front
  | Peek_back ->
    empty
  | Skip n -> Q.Shrink.int n >|= skip
  | Blit (s, i, len) ->
    let s_i =
      Q.Shrink.int i >>= fun i' ->
      assert (i' <= i && i' + len <= String.length s);
      if i' <= 0 then
        empty
      else
        return (blit s i' len)
    and s_len =
      Q.Shrink.int len >>= fun len' ->
      assert (len' <= len && i + len' <= String.length s);
      if len' <= 0 then
        empty
      else
        return (blit s i len')
    and s_s =
      Q.Shrink.string s >>= fun s' ->
      if i + len > String.length s' then
        empty
      else
        return (blit s' i len)
    in
    append s_i (append s_len s_s)

let len_op size acc = function
  | Push_back _ -> min size (acc + 1)
  | Take_front | Take_back | Junk_front | Junk_back -> max (acc - 1) 0
  | Skip n ->
    if acc >= n then
      acc - n
    else
      acc
  | Z_if_full | Peek_front | Peek_back -> acc
  | Blit (_, _, len) -> min size (acc + len)

let apply_op b = function
  | Push_back c ->
    BS.push_back b c;
    None
  | Take_front -> BS.take_front b
  | Take_back -> BS.take_back b
  | Junk_front ->
    (try BS.junk_front b with BS.Empty -> ());
    None
  | Junk_back ->
    (try BS.junk_back b with BS.Empty -> ());
    None
  | Peek_front -> BS.peek_front b
  | Peek_back -> BS.peek_back b
  | Skip n ->
    if n <= BS.length b then BS.skip b n;
    None
  | Blit (s, i, len) ->
    assert (i + len <= String.length s);
    BS.blit_from b (Bytes.unsafe_of_string s) i len;
    None
  | Z_if_full ->
    if BS.is_full b then
      Some '0'
    else
      None

let gen_op =
  let open Q.Gen in
  let g_blit =
    string_size ~gen:g_char (5 -- 20) >>= fun s ->
    0 -- String.length s >>= fun len ->
    assert (len >= 0 && len <= String.length s);
    0 -- (String.length s - len) >|= fun i -> blit s i len
  in
  frequency
    [
      3, return Take_back;
      3, return Take_front;
      1, return Junk_back;
      1, return Junk_front;
      1, return Peek_front;
      1, return Peek_back;
      2, g_blit;
      1, 0 -- 5 >|= skip;
      2, map push_back g_char;
      1, return Z_if_full;
    ]

let arb_op = Q.make ~shrink:shrink_op ~print:str_of_op gen_op
let arb_ops = Q.list_of_size Q.Gen.(0 -- 20) arb_op

module L_impl = struct
  type t = {
    size: int;
    mutable l: char list;
  }

  let create size = { size; l = [] }

  let normalize_ b =
    let n = List.length b.l in
    if n > b.size then b.l <- CCList.drop (n - b.size) b.l

  let push_back b c =
    b.l <- b.l @ [ c ];
    normalize_ b

  let take_front b =
    match b.l with
    | [] -> None
    | c :: l ->
      b.l <- l;
      Some c

  let peek_front b =
    match b.l with
    | [] -> None
    | x :: _ -> Some x

  let take_back b =
    let n = List.length b.l in
    if n = 0 then
      None
    else (
      let init, last = CCList.take_drop (n - 1) b.l in
      let x = List.hd last in
      b.l <- init;
      Some x
    )

  let peek_back b =
    match b.l with
    | [] -> None
    | l -> Some (List.hd (List.rev l))

  let junk_front b = ignore (take_front b)
  let junk_back b = ignore (take_back b)

  let skip b n =
    if n <= List.length b.l then CCInt.range' 0 n (fun _ -> junk_front b)

  let blit b s i len =
    for j = i to i + len - 1 do
      push_back b (String.get s j)
    done

  let apply_op b = function
    | Push_back c ->
      push_back b c;
      None
    | Take_front -> take_front b
    | Take_back -> take_back b
    | Peek_front -> peek_front b
    | Peek_back -> peek_back b
    | Junk_back ->
      junk_back b;
      None
    | Junk_front ->
      junk_front b;
      None
    | Skip n ->
      skip b n;
      None
    | Blit (s, i, len) ->
      blit b s i len;
      None
    | Z_if_full ->
      if b.size = List.length b.l then
        Some '0'
      else
        None

  let to_list b = b.l
end
;;

(* check that a lot of operations can be applied without failure,
   and that the result has correct length *)
q ~count:3_000 arb_ops (fun ops ->
    let size = 64 in
    let b = BS.create size in
    List.iter (fun o -> ignore (apply_op b o)) ops;
    BS.length b = List.fold_left (len_op size) 0 ops)
;;

(* check identical behavior with list implem *)
q ~count:3_000 arb_ops (fun ops ->
    let size = 64 in
    let b = BS.create size in
    let l = L_impl.create size in
    let l1 = CCList.filter_map (apply_op b) ops in
    let l2 = CCList.filter_map (L_impl.apply_op l) ops in
    l1 = l2 && BS.to_list b = L_impl.to_list l)

(* check that deleted elements can be GCed *)
module BO = CCRingBuffer.Make (struct
  type t = int option

  let dummy = None
end)

let make_bo () =
  let b = BO.create 1000 in
  for i = 1 to BO.capacity b do
    BO.push_back b (Some i)
  done;
  b

let test_no_major_blocks clear =
  Gc.full_major ();
  let live_blocks_before = (Gc.stat ()).live_blocks in
  let b = make_bo () in
  clear b;
  Gc.full_major ();
  let live_blocks_after = (Gc.stat ()).live_blocks in
  assert (BO.length b = 0);
  let diff = live_blocks_after - live_blocks_before in
  diff < BO.capacity b / 2
;;

t @@ fun () ->
test_no_major_blocks (fun b ->
    for _ = 1 to BO.length b do
      BO.junk_front b
    done)
;;

t @@ fun () ->
test_no_major_blocks (fun b ->
    for _ = 1 to BO.length b do
      BO.junk_back b
    done)
;;

t @@ fun () ->
test_no_major_blocks (fun b ->
    for _ = 1 to BO.length b do
      ignore (BO.take_front b)
    done)
;;

t @@ fun () ->
test_no_major_blocks (fun b ->
    for _ = 1 to BO.length b do
      ignore (BO.take_back b)
    done)
;;

t @@ fun () -> test_no_major_blocks (fun b -> BO.skip b (BO.length b));;
t @@ fun () -> test_no_major_blocks (fun b -> BO.clear b)
