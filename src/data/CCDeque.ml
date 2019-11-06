(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Imperative deque} *)

type 'a cell =
  | One of 'a
  | Two of 'a * 'a
  | Three of 'a * 'a * 'a
  (** A cell holding a small number of elements *)

type 'a inner_node = {
  mutable cell : 'a cell;
  mutable next : 'a inner_node;
  mutable prev : 'a inner_node;
}

type 'a node = Empty | Node of 'a inner_node
(** Linked list of cells.

    invariant: only the first and last cells are allowed to
    be anything but [Three] (all the intermediate ones are [Three])
    *)

type 'a t = {
  mutable cur : 'a node;
  mutable size : int;
}
(** The deque, a double linked list of cells *)

(*$inject
  let plist l = CCFormat.(to_string (list int)) l
  let pint i = string_of_int i
*)

(*$R
  let q = create () in
  add_seq_back q Iter.(3 -- 5);
  assert_equal [3;4;5] (to_list q);
  add_seq_front q Iter.(of_list [2;1]);
  assert_equal [1;2;3;4;5] (to_list q);
  push_front q 0;
  assert_equal [0;1;2;3;4;5] (to_list q);
  assert_equal 5 (take_back q);
  assert_equal 0 (take_front q);
  assert_equal 4 (length q);
*)

exception Empty

let create () =
  { cur = Empty; size=0 }

let clear q =
  q.cur <- Empty;
  q.size <- 0;
  ()

(*$R
  let q = of_seq Iter.(1 -- 100) in
  assert_equal 100 (length q);
  clear q;
  assert_equal 0 (length q);
  assert_raises Empty (fun () -> peek_front q);
  assert_raises Empty (fun () -> peek_back q);
*)

let incr_size_ d = d.size <- d.size + 1
let decr_size_ d = d.size <- d.size - 1

let bool_eq (a : bool) b = Stdlib.(=) a b

let is_empty d =
  let res = d.size = 0 in
  assert (bool_eq res (d.cur = Empty));
  res

let push_front d x =
  incr_size_ d;
  match d.cur with
  | Empty ->
    let rec node = { cell=One x; prev = node; next = node } in
    d.cur <- Node node
  | Node n ->
    begin match n.cell with
      | One y -> n.cell <- Two (x, y)
      | Two (y, z) -> n.cell <- Three (x,y,z)
      | Three _ ->
        let node = { cell = One x; prev = n.prev; next = n; } in
        n.prev.next <- node;
        n.prev <- node;
        d.cur <- Node node (* always point to first node *)
    end

let push_back d x =
  incr_size_ d;
  match d.cur with
  | Empty ->
    let rec node = { cell=One x; prev = node; next = node } in
    d.cur <- Node node
  | Node cur ->
    let n = cur.prev in (* last node *)
    begin match n.cell with
      | One y -> n.cell <- Two (y, x)
      | Two (y,z) -> n.cell <- Three (y, z, x)
      | Three _ ->
        let elt = { cell = One x; next=cur; prev=n; } in
        n.next <- elt;
        cur.prev <- elt
    end

let peek_front_opt d =
  match d.cur with
  | Empty -> None
  | Node cur ->
    match cur.cell with
    | One x -> Some x
    | Two (x,_) -> Some x
    | Three (x,_,_) -> Some x

let peek_front d = match peek_front_opt d with
  | None -> raise Empty
  | Some x -> x

(*$T
  of_list [1;2;3] |> peek_front = 1
  try (ignore (of_list [] |> peek_front); false) with Empty -> true
*)

(*$R
  let d = of_seq Iter.(1 -- 10) in
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

let peek_back_opt d =
  match d.cur with
  | Empty -> None
  | Node cur ->
    match cur.prev.cell with
    | One x -> Some x
    | Two (_,x) -> Some x
    | Three (_,_,x) -> Some x

let peek_back d = match peek_back_opt d with
  | None -> raise Empty
  | Some x -> x
(*$T
  of_list [1;2;3] |> peek_back = 3
  try (ignore (of_list [] |> peek_back); false) with Empty -> true
*)

(*$R
  let d = of_seq Iter.(1 -- 10) in
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
  | One x -> (true, x)
  | Two (x,y) -> n.cell <- One x; (false, y)
  | Three (x,y,z) -> n.cell <- Two (x,y); (false, z)

let remove_node_ n =
  let next = n.next in
  n.prev.next <- next;
  next.prev <- n.prev

let take_back_opt d =
  match d.cur with
  | Empty -> None
  | Node cur ->
    if Stdlib.(==) cur cur.prev
    then (
      (* only one cell *)
      decr_size_ d;
      let is_zero, x = take_back_node_ cur in
      if is_zero then d.cur <- Empty;
      Some x
    ) else (
      let n = cur.prev in
      let is_zero, x = take_back_node_ n in
      decr_size_ d;
      (* remove previous node *)
      if is_zero then remove_node_ n;
      Some x
    )

let take_back d = match take_back_opt d with
  | None -> raise Empty
  | Some x -> x

(*$T
  let q = of_list [1] in take_back q = 1 && to_list q = []
  let q = of_list [1;2] in take_back q = 2 && to_list q = [1]
  let q = of_list [1;2;3] in take_back q = 3 && to_list q = [1;2]
  let q = of_list [1;2;3;4;5;6;7;] in take_back q = 7 && to_list q = [1;2;3;4;5;6]
*)

let take_front_node_ n = match n.cell with
  | One x -> (true, x)
  | Two (x,y) -> n.cell <- One y; (false, x)
  | Three (x,y,z) -> n.cell <- Two (y,z); (false, x)

(*$T
  let q = of_list [1;2;3] in take_front q = 1 && to_list q = [2;3]
*)

let take_front_opt d =
  match d.cur with
  | Empty -> None
  | Node cur ->
    if Stdlib.(==) cur.prev cur
    then (
      (* only one cell *)
      decr_size_ d;
      let is_zero, x = take_front_node_ cur in
      if is_zero then d.cur <- Empty;
      Some x
    ) else (
      decr_size_ d;
      let is_zero, x = take_front_node_ cur in
      if is_zero then (
        cur.prev.next <- cur.next;
        cur.next.prev <- cur.prev;
        d.cur <- Node cur.next;
      );
      Some x
    )

let take_front d = match take_front_opt d with
  | None -> raise Empty
  | Some x -> x

let remove_back d = ignore (take_back_opt d)

(*$T remove_back
  let q = of_list [1;2;3;4;5;6;7] in remove_back q; to_list q = [1;2;3;4;5;6]
*)

let remove_front d = ignore (take_front_opt d)

(*$T remove_front
  let q = of_list [1;2;3;4;5;6;7] in remove_front q; to_list q = [2;3;4;5;6;7]
*)

let update_front d f =
  match d.cur with
  | Empty -> ()
  | Node cur ->
    match cur.cell with
    | One x ->
      begin match f x with
        | None ->
          if Stdlib.(!=) cur.prev cur then (
            cur.prev.next <- cur.next;
            cur.next.prev <- cur.prev;
            d.cur <- Node cur.next;
          ) else (
            d.cur <- Empty
          )
        | Some x -> cur.cell <- One x
      end
    | Two (x, y) ->
      begin match f x with
        | None -> cur.cell <- One (y)
        | Some x -> cur.cell <- Two (x,y)
      end
    | Three (x,y,z) ->
      begin match f x with
        | None -> cur.cell <- Two (y,z)
        | Some x -> cur.cell <- Three (x,y,z)
      end

(*$T update_front
  let q = of_list [1;2;3;4;5;6;7] in update_front q (fun _ -> None); to_list q = [2;3;4;5;6;7]
  let q = of_list [1;2;3;4;5;6;7] in update_front q (fun _ -> Some 9); to_list q = [9;2;3;4;5;6;7]
*)
(*$Q update_front
  Q.(list int) (fun l -> \
    let q = of_list l in \
    update_front q (fun _ -> None); \
    let output_list = if l = [] then [] else List.tl l in \
    to_list q = output_list)
  Q.(list int) (fun l -> \
    let q = of_list l in \
    update_front q (fun x -> Some (x + 42)); \
    let output_list = if l = [] then [] else List.((hd l + 42)::(tl l)) in \
    to_list q = output_list)
*)

let update_back d f =
  match d.cur with
  | Empty -> ()
  | Node cur ->
    let n = cur.prev in
    match n.cell with
    | One x ->
      begin match f x with
        | None ->
          if Stdlib.(!=) cur.prev cur then remove_node_ n
          else d.cur <- Empty
        | Some x -> n.cell <- One x
      end
    | Two (x, y) ->
      begin match f y with
        | None -> n.cell <- One (x)
        | Some y -> n.cell <- Two (x,y)
      end
    | Three (x,y,z) ->
      begin match f z with
        | None -> n.cell <- Two (x,y)
        | Some z -> n.cell <- Three (x,y,z)
      end

(*$T update_back
  let q = of_list [1;2;3;4;5;6;7] in update_back q (fun _ -> None); to_list q = [1;2;3;4;5;6]
  let q = of_list [1;2;3;4;5;6;7] in update_back q (fun _ -> Some 9); to_list q = [1;2;3;4;5;6;9]
*)
(*$Q update_back
  Q.(list int) (fun l -> \
    let q = of_list l in \
    update_back q (fun _ -> None); \
    let output_list = if l = [] then [] else List.(rev l |> tl) in \
    (to_list q |> List.rev) = output_list)
  Q.(list int) (fun l -> \
    let q = of_list l in \
    update_back q (fun x -> Some (x + 42)); \
    let output_list = if l = [] then [] else List.(rev l |> fun l -> (hd l + 42)::(tl l)) in \
    (to_list q |> List.rev) = output_list)
*)

let iter f d =
  let rec iter f ~first n =
    begin match n.cell with
      | One x -> f x
      | Two (x,y) -> f x; f y
      | Three (x,y,z) -> f x; f y; f z
    end;
    if n.next != first then iter f ~first n.next
  in
  match d.cur with
  | Empty -> ()
  | Node cur ->
    iter f ~first:cur cur

(*$T
  let n = ref 0 in iter (fun _ -> incr n) (of_list [1;2;3]); !n = 3
*)

(*$R
  let d = of_seq Iter.(1 -- 5) in
  let s = Iter.from_iter (fun k -> iter k d) in
  let l = Iter.to_list s in
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
      | One x -> f acc x
      | Two (x,y) -> f (f acc x) y
      | Three (x,y,z) -> f (f (f acc x) y) z
    in
    if Stdlib.(==) n.next first then acc else aux ~first f acc n.next
  in
  match d.cur with
  | Empty -> acc
  | Node cur ->
    aux ~first:cur f acc cur

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
  let d = of_seq Iter.(1 -- 10) in
  OUnit.assert_equal ~printer:pint 10 (length d)
*)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let add_seq_back q seq = seq (fun x -> push_back q x)

let add_seq_front q seq = seq (fun x -> push_front q x)

(*$R
  let q = of_list [4;5] in
  add_seq_front q Iter.(of_list [3;2;1]);
  assert_equal [1;2;3;4;5] (to_list q);
  add_seq_back q Iter.(of_list [6;7]);
  assert_equal [1;2;3;4;5;6;7] (to_list q);
*)

let of_seq seq =
  let deque = create () in
  seq (fun x -> push_back deque x);
  deque

let to_seq d k = iter k d

(*$Q
  Q.(list int) (fun l -> \
    Iter.of_list l |> of_seq |> to_seq |> Iter.to_list = l)
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

let size_cell_ = function
  | One _ -> 1
  | Two _ -> 2
  | Three _ -> 3

(* filter over a cell *)
let filter_cell_ f = function
  | One x as c -> if f x then Some c else None
  | Two (x,y) as c ->
    let fx = f x in
    let fy = f y in
    begin match fx, fy with
      | true, true -> Some c
      | true, false -> Some (One x)
      | false, true -> Some (One y)
      | _ -> None
    end
  | Three (x,y,z) as c ->
    let fx = f x in
    let fy = f y in
    let fz = f z in
    begin match fx, fy, fz with
      | true, true, true -> Some c
      | true, true, false -> Some (Two (x,y))
      | true, false, true -> Some (Two (x,z))
      | true, false, false -> Some (One x)
      | false, true, true -> Some (Two (y,z))
      | false, true, false -> Some (One y)
      | false, false, true -> Some (One z)
      | false, false, false -> None
    end

let filter_in_place (d:_ t) f : unit =
  (* update size, compute new cell *)
  let update_local_ n =
    d.size <- d.size - size_cell_ n.cell;
    match filter_cell_ f n.cell with
    | None -> None
    | Some n as new_cell->
      d.size <- d.size + size_cell_ n;
      new_cell
  in
  let rec loop ~stop_at n : unit =
    if n != stop_at then (
      let n_prev = n.prev in
      let n_next = n.next in
      let new_cell = update_local_ n in
      (* merge into previous cell *)
      begin match n_prev.cell, new_cell with
        | _, None -> remove_node_ n
        | Three _, Some new_cell -> n.cell <- new_cell
        | One x, Some (One y) -> remove_node_ n; n_prev.cell <- Two (x,y)
        | One (x), Some (Two (y,z))
        | Two (x,y), Some (One z) -> remove_node_ n; n_prev.cell <- Three (x,y,z)
        | One x, Some (Three (y,z,w))
        | Two (x,y), Some (Two (z,w)) -> n_prev.cell <- Three (x,y,z); n.cell <- One w
        | Two (x,y), Some (Three (z,w1,w2)) -> n_prev.cell <- Three (x,y,z); n.cell <- Two (w1,w2)
      end;
      loop ~stop_at n_next;
    );
  in
  let rec new_first_cell ~stop_at n =
    if n != stop_at then (
      match update_local_ n with
      | None ->
        new_first_cell ~stop_at n.next
      | Some c ->
        n.cell <- c; Some n
    ) else None
  in
  match d.cur with
    | Empty -> ()
    | Node cur ->
      (* special case for first cell *)
      match update_local_ cur with
      | None ->
        begin match new_first_cell ~stop_at:cur cur.next with
          | None -> d.cur <- Empty
          | Some n ->
            cur.prev.next <- n;
            n.prev <- cur.prev;
            d.cur <- Node n;
            loop ~stop_at:n n.next
        end
      | Some c ->
        cur.cell <- c;
        loop ~stop_at:cur cur.next

(*$R
   let q = of_list [1;2;3;4;5;6] in
   filter_in_place q (fun x -> x mod 2 = 0);
   assert_equal [2;4;6] (to_list q)
  *)

(*$R
   let q = of_list [2;1;4;6;10;20] in
   filter_in_place q (fun x -> x mod 2 = 0);
   assert_equal [2;4;6;10;20] (to_list q)
  *)

(*$Q
  Q.(list small_nat) (fun l -> \
    let f = fun x -> x mod 2=0 in \
    let q = of_list l in \
    (filter_in_place q f; to_list q) = (List.filter f l))
  *)

let filter f q =
  let q' = create() in
  iter (fun x -> if f x then push_back q' x) q;
  q'

(*$Q
  Q.(list small_nat) (fun l -> \
    let f = fun x -> x mod 2=0 in \
    let q = filter f (of_list l) in \
    (to_list q) = (List.filter f l))
  *)

let filter_map f q =
  let q' = create() in
  iter (fun x -> match f x with None -> () | Some y -> push_back q' y) q;
  q'

let rec gen_iter_ f g = match g() with
  | None -> ()
  | Some x -> f x; gen_iter_ f g

let of_gen g =
  let q = create () in
  gen_iter_ (fun x -> push_back q x) g;
  q

let to_gen q =
  match q.cur with
  | Empty -> (fun () -> None)
  | Node cur ->
    let first = cur in
    let cell = ref (Some cur.cell) in
    let cur = ref cur in
    let rec next () =
      match !cell with
      | None when Stdlib.(==) (!cur).next first -> None
      | None ->
        (* go to next node *)
        let n = !cur in
        cur := n.next;
        cell := Some (n.next.cell);
        next ()
      | Some (One x) -> cell := None; Some x
      | Some (Two (x,y)) -> cell := Some (One y); Some x
      | Some (Three (x,y,z)) -> cell := Some (Two (y,z)); Some x
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
  let cmp = equal ~eq:CCInt.equal in
  assert_equal 4 (length q');
  assert_equal ~cmp q q';
  push_front q 0;
  assert_bool "not equal" (not (cmp q q'));
  assert_equal 5 (length q);
  push_front q' 0;
  assert_equal ~cmp q q'
*)

let equal ~eq a b =
  let rec aux eq a b = match a() , b() with
    | None, None -> true
    | None, Some _
    | Some _, None -> false
    | Some x, Some y -> eq x y && aux eq a b
  in aux eq (to_gen a) (to_gen b)

let compare ~cmp a b =
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
    CCOrd.equiv (compare ~cmp:Stdlib.compare (of_list l1) (of_list l2)) \
      (CCList.compare Stdlib.compare l1 l2))
*)

type 'a printer = Format.formatter -> 'a -> unit

let pp pp_x out d =
  let first = ref true in
  Format.fprintf out "@[<hov2>deque {";
  iter
    (fun x ->
       if !first then first:= false else Format.fprintf out ";@ ";
       pp_x out x
    ) d;
  Format.fprintf out "}@]"

