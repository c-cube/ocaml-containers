
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Imperative deque} *)

type 'a cell =
  | Zero
  | One of 'a
  | Two of 'a * 'a
  | Three of 'a * 'a * 'a
(** A cell holding a small number of elements *)

type 'a node = {
  mutable cell : 'a cell;
  mutable next : 'a node;
  mutable prev : 'a node;
}
(** Linked list of cells.

    invariant: only the first and last cells are allowed to
    be anything but [Three] (all the intermediate ones are [Three]) *)

type 'a t = {
  mutable cur : 'a node;
  mutable size : int;
}
(** The deque, a double linked list of cells *)

(*$inject
  let plist l = CCPrint.to_string (CCList.pp CCInt.pp) l
  let pint i = string_of_int i
*)

(*$R
  let q = create () in
  add_seq_back q Sequence.(3 -- 5);
  assert_equal [3;4;5] (to_list q);
  add_seq_front q Sequence.(of_list [2;1]);
  assert_equal [1;2;3;4;5] (to_list q);
  push_front q 0;
  assert_equal [0;1;2;3;4;5] (to_list q);
  assert_equal 5 (take_back q);
  assert_equal 0 (take_front q);
  assert_equal 4 (length q);
*)

exception Empty

let create () =
  let rec cur = { cell=Zero; prev=cur; next=cur } in
  { cur; size=0 }

let clear q =
  let rec cur = { cell=Zero; prev=cur; next=cur } in
  q.cur <- cur;
  q.size <- 0;
  ()

(*$R
  let q = of_seq Sequence.(1 -- 100) in
  assert_equal 100 (length q);
  clear q;
  assert_equal 0 (length q);
  assert_raises Empty (fun () -> peek_front q);
  assert_raises Empty (fun () -> peek_back q);
*)

let incr_size_ d = d.size <- d.size + 1
let decr_size_ d = d.size <- d.size - 1

let is_zero_ n = match n.cell with
  | Zero -> true
  | One _
  | Two _
  | Three _ -> false

let is_empty d =
  let res = d.size = 0 in
  assert (res = is_zero_ d.cur);
  res

let push_front d x =
  incr_size_ d;
  match d.cur.cell with
  | Zero -> d.cur.cell <- One x
  | One y -> d.cur.cell <- Two (x, y)
  | Two (y, z) -> d.cur.cell <- Three (x,y,z)
  | Three _ ->
    let node = { cell = One x; prev = d.cur.prev; next=d.cur; } in
    d.cur.prev.next <- node;
    d.cur.prev <- node;
    d.cur <- node (* always point to first node *)

let push_back d x =
  incr_size_ d;
  let n = d.cur.prev in (* last node *)
  match n.cell with
  | Zero -> n.cell <- One x
  | One y -> n.cell <- Two (y, x)
  | Two (y,z) -> n.cell <- Three (y, z, x)
  | Three _ ->
    let elt = { cell = One x; next=d.cur; prev=n; } in
    n.next <- elt;
    d.cur.prev <- elt

let peek_front d = match d.cur.cell with
  | Zero -> raise Empty
  | One x -> x
  | Two (x,_) -> x
  | Three (x,_,_) -> x

(*$T
  of_list [1;2;3] |> peek_front = 1
  try (ignore (of_list [] |> peek_front); false) with Empty -> true
  *)

(*$R
  let d = of_seq Sequence.(1 -- 10) in
  let printer = pint in
  OUnit.assert_equal ~printer 1 (peek_front d);
  push_front d 42;
  OUnit.assert_equal ~printer 42 (peek_front d);
  OUnit.assert_equal ~printer 42 (take_front d);
  OUnit.assert_equal ~printer 1 (take_front d);
  OUnit.assert_equal ~printer 2 (take_front d);
  OUnit.assert_equal ~printer 3 (take_front d);
  OUnit.assert_equal ~printer 10 (peek_back d);
*)

let peek_back d =
  if is_empty d then raise Empty
  else match d.cur.prev.cell with
    | Zero -> assert false
    | One x -> x
    | Two (_,x) -> x
    | Three (_,_,x) -> x

(*$T
  of_list [1;2;3] |> peek_back = 3
  try (ignore (of_list [] |> peek_back); false) with Empty -> true
*)

(*$R
  let d = of_seq Sequence.(1 -- 10) in
  let printer = pint in
  OUnit.assert_equal ~printer 1 (peek_front d);
  push_back d 42;
  OUnit.assert_equal ~printer 42 (peek_back d);
  OUnit.assert_equal ~printer 42 (take_back d);
  OUnit.assert_equal ~printer 10 (take_back d);
  OUnit.assert_equal ~printer 9 (take_back d);
  OUnit.assert_equal ~printer 8 (take_back d);
  OUnit.assert_equal ~printer 1 (peek_front d);
*)

let take_back_node_ n = match n.cell with
  | Zero -> assert false
  | One x -> n.cell <- Zero; x
  | Two (x,y) -> n.cell <- One x; y
  | Three (x,y,z) -> n.cell <- Two (x,y); z

let take_back d =
  if is_empty d then raise Empty
  else if d.cur == d.cur.prev
  then (
    (* only one cell *)
    decr_size_ d;
    take_back_node_ d.cur
  ) else (
    let n = d.cur.prev in
    let x = take_back_node_ n in
    decr_size_ d;
    if is_zero_ n
    then (  (* remove previous node *)
      d.cur.prev <- n.prev;
      n.prev.next <- d.cur;
    );
    x
  )

(*$T
  let q = of_list [1;2;3] in take_back q = 3 && to_list q = [1;2]
  *)

let take_front_node_ n = match n.cell with
  | Zero -> assert false
  | One x -> n.cell <- Zero; x
  | Two (x,y) -> n.cell <- One y; x
  | Three (x,y,z) -> n.cell <- Two (y,z); x

(*$T
  let q = of_list [1;2;3] in take_front q = 1 && to_list q = [2;3]
  *)

let take_front d =
  if is_empty d then raise Empty
  else if d.cur.prev == d.cur
  then (
    (* only one cell *)
    decr_size_ d;
    take_front_node_ d.cur
  ) else (
    decr_size_ d;
    let x = take_front_node_ d.cur in
    if is_zero_ d.cur then (
      d.cur.prev.next <- d.cur.next;
      d.cur.next.prev <- d.cur.prev;
      d.cur <- d.cur.next;
    );
    x
  )

let iter f d =
  let rec iter f ~first n =
    begin match n.cell with
    | Zero -> ()
    | One x -> f x
    | Two (x,y) -> f x; f y
    | Three (x,y,z) -> f x; f y; f z
    end;
    if n.next != first then iter f ~first n.next
  in
  iter f ~first:d.cur d.cur

(*$T
  let n = ref 0 in iter (fun _ -> incr n) (of_list [1;2;3]); !n = 3
*)

(*$R
  let d = of_seq Sequence.(1 -- 5) in
  let s = Sequence.from_iter (fun k -> iter k d) in
  let l = Sequence.to_list s in
  OUnit.assert_equal ~printer:plist [1;2;3;4;5] l;
*)

let append_front ~into q = iter (push_front into) q

let append_back ~into q = iter (push_back into) q

(*$R
  let q = of_list [3;4] in
  append_front ~into:q (of_list [2;1]);
  assert_equal [1;2;3;4] (to_list q);
  append_back ~into:q (of_list [5;6]);
  assert_equal [1;2;3;4;5;6] (to_list q);
*)

let fold f acc d =
  let rec aux ~first f acc n =
    let acc = match n.cell with
      | Zero -> acc
      | One x -> f acc x
      | Two (x,y) -> f (f acc x) y
      | Three (x,y,z) -> f (f (f acc x) y) z
    in
    if n.next == first then acc else aux ~first f acc n.next
  in
  aux ~first:d.cur f acc d.cur

(*$T
  fold (+) 0 (of_list [1;2;3]) = 6
  fold (fun acc x -> x::acc) [] (of_list [1;2;3]) = [3;2;1]
*)

let length d = d.size

(*$Q
  Q.(list int) (fun l -> \
    let q = of_list l in \
    append_front ~into:q (of_list l); \
    append_back ~into:q (of_list l); \
    length q = 3 * List.length l)
*)

(*$R
  let d = of_seq Sequence.(1 -- 10) in
  OUnit.assert_equal ~printer:pint 10 (length d)
*)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let add_seq_back q seq = seq (fun x -> push_back q x)

let add_seq_front q seq = seq (fun x -> push_front q x)

(*$R
  let q = of_list [4;5] in
  add_seq_front q Sequence.(of_list [3;2;1]);
  assert_equal [1;2;3;4;5] (to_list q);
  add_seq_back q Sequence.(of_list [6;7]);
  assert_equal [1;2;3;4;5;6;7] (to_list q);
*)

let of_seq seq =
  let deque = create () in
  seq (fun x -> push_back deque x);
  deque

let to_seq d k = iter k d

(*$Q
  Q.(list int) (fun l -> \
    Sequence.of_list l |> of_seq |> to_seq |> Sequence.to_list = l)
  *)

let of_list l =
  let q = create() in
  List.iter (push_back q) l;
  q

(*$R
  let q = of_list [1;2;3] in
  assert_equal 1 (take_front q);
  assert_equal 3 (take_back q);
  assert_equal 2 (take_front q);
  assert_equal true (is_empty q)
*)

let to_rev_list q = fold (fun l x -> x::l) [] q

let to_list q = List.rev (to_rev_list q)

let rec gen_iter_ f g = match g() with
  | None -> ()
  | Some x -> f x; gen_iter_ f g

let of_gen g =
  let q = create () in
  gen_iter_ (fun x -> push_back q x) g;
  q

let to_gen q =
  let first = q.cur in
  let cell = ref q.cur.cell in
  let cur = ref q.cur in
  let rec next () = match !cell with
    | Zero when (!cur).next == first -> None
    | Zero ->
      (* go to next node *)
      let n = !cur in
      cur := n.next;
      cell := n.next.cell;
      next ()
    | One x -> cell := Zero; Some x
    | Two (x,y) -> cell := One y; Some x
    | Three (x,y,z) -> cell := Two (y,z); Some x
  in
  next

(*$T
  of_list [1;2;3] |> to_gen |> of_gen |> to_list = [1;2;3]
*)

(*$Q
  Q.(list int) (fun l -> \
    of_list l |> to_gen |> of_gen |> to_list = l)
*)

(* naive implem of copy, for now *)
let copy d =
  let d' = create () in
  iter (fun x -> push_back d' x) d;
  d'

(*$R
  let q = of_list [1;2;3;4] in
  assert_equal 4 (length q);
  let q' = copy q in
  let cmp = equal ?eq:None in
  assert_equal 4 (length q');
  assert_equal ~cmp q q';
  push_front q 0;
  assert_bool "not equal" (not (cmp q q'));
  assert_equal 5 (length q);
  push_front q' 0;
  assert_equal ~cmp q q'
*)

let equal ?(eq=(=)) a b =
  let rec aux eq a b = match a() , b() with
    | None, None -> true
    | None, Some _
    | Some _, None -> false
    | Some x, Some y -> eq x y && aux eq a b
  in aux eq (to_gen a) (to_gen b)

let compare ?(cmp=Pervasives.compare) a b =
  let rec aux cmp a b = match a() , b() with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some x, Some y ->
        let c = cmp x y in
        if c=0 then aux cmp a b else c
  in aux cmp (to_gen a) (to_gen b)

(*$Q
   Q.(pair (list int) (list int)) (fun (l1,l2) -> \
    CCOrd.equiv (compare (of_list l1) (of_list l2)) \
      (CCList.compare Pervasives.compare l1 l2))
  *)

type 'a printer = Format.formatter -> 'a -> unit

let print pp_x out d =
  let first = ref true in
  Format.fprintf out "@[<hov2>deque {";
  iter
    (fun x ->
      if !first then first:= false else Format.fprintf out ";@ ";
      pp_x out x
    ) d;
  Format.fprintf out "}@]"

