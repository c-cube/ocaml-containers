(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Imperative deque} *)

type 'a cell =
  | One of 'a
  | Two of 'a * 'a
  | Three of 'a * 'a * 'a  (** A cell holding a small number of elements *)

type 'a inner_node = {
  mutable cell: 'a cell;
  mutable next: 'a inner_node;
  mutable prev: 'a inner_node;
}

type 'a node =
  | Empty
  | Node of 'a inner_node
      (** Linked list of cells.

    invariant: only the first and last cells are allowed to
    be anything but [Three] (all the intermediate ones are [Three])
    *)

type 'a t = {
  mutable cur: 'a node;
  mutable size: int;
}
(** The deque, a double linked list of cells *)

exception Empty

let create () = { cur = Empty; size = 0 }

let clear q =
  q.cur <- Empty;
  q.size <- 0;
  ()

let incr_size_ d = d.size <- d.size + 1
let decr_size_ d = d.size <- d.size - 1
let bool_eq (a : bool) b = Stdlib.( = ) a b

let is_empty d =
  let res = d.size = 0 in
  assert (bool_eq res (d.cur = Empty));
  res

let push_front d x =
  incr_size_ d;
  match d.cur with
  | Empty ->
    let rec node = { cell = One x; prev = node; next = node } in
    d.cur <- Node node
  | Node n ->
    (match n.cell with
    | One y -> n.cell <- Two (x, y)
    | Two (y, z) -> n.cell <- Three (x, y, z)
    | Three _ ->
      let node = { cell = One x; prev = n.prev; next = n } in
      n.prev.next <- node;
      n.prev <- node;
      d.cur <- Node node
      (* always point to first node *))

let push_back d x =
  incr_size_ d;
  match d.cur with
  | Empty ->
    let rec node = { cell = One x; prev = node; next = node } in
    d.cur <- Node node
  | Node cur ->
    let n = cur.prev in
    (* last node *)
    (match n.cell with
    | One y -> n.cell <- Two (y, x)
    | Two (y, z) -> n.cell <- Three (y, z, x)
    | Three _ ->
      let elt = { cell = One x; next = cur; prev = n } in
      n.next <- elt;
      cur.prev <- elt)

let peek_front_opt d =
  match d.cur with
  | Empty -> None
  | Node cur ->
    (match cur.cell with
    | One x -> Some x
    | Two (x, _) -> Some x
    | Three (x, _, _) -> Some x)

let peek_front d =
  match peek_front_opt d with
  | None -> raise Empty
  | Some x -> x

let peek_back_opt d =
  match d.cur with
  | Empty -> None
  | Node cur ->
    (match cur.prev.cell with
    | One x -> Some x
    | Two (_, x) -> Some x
    | Three (_, _, x) -> Some x)

let peek_back d =
  match peek_back_opt d with
  | None -> raise Empty
  | Some x -> x

let take_back_node_ n =
  match n.cell with
  | One x -> true, x
  | Two (x, y) ->
    n.cell <- One x;
    false, y
  | Three (x, y, z) ->
    n.cell <- Two (x, y);
    false, z

let remove_node_ n =
  let next = n.next in
  n.prev.next <- next;
  next.prev <- n.prev

let take_back_opt d =
  match d.cur with
  | Empty -> None
  | Node cur ->
    if Stdlib.( == ) cur cur.prev then (
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

let take_back d =
  match take_back_opt d with
  | None -> raise Empty
  | Some x -> x

let take_front_node_ n =
  match n.cell with
  | One x -> true, x
  | Two (x, y) ->
    n.cell <- One y;
    false, x
  | Three (x, y, z) ->
    n.cell <- Two (y, z);
    false, x

let take_front_opt d =
  match d.cur with
  | Empty -> None
  | Node cur ->
    if Stdlib.( == ) cur.prev cur then (
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
        d.cur <- Node cur.next
      );
      Some x
    )

let take_front d =
  match take_front_opt d with
  | None -> raise Empty
  | Some x -> x

let remove_back d = ignore (take_back_opt d)
let remove_front d = ignore (take_front_opt d)

let update_front d f =
  match d.cur with
  | Empty -> ()
  | Node cur ->
    (match cur.cell with
    | One x ->
      (match f x with
      | None ->
        if Stdlib.( != ) cur.prev cur then (
          cur.prev.next <- cur.next;
          cur.next.prev <- cur.prev;
          d.cur <- Node cur.next
        ) else
          d.cur <- Empty
      | Some x -> cur.cell <- One x)
    | Two (x, y) ->
      (match f x with
      | None -> cur.cell <- One y
      | Some x -> cur.cell <- Two (x, y))
    | Three (x, y, z) ->
      (match f x with
      | None -> cur.cell <- Two (y, z)
      | Some x -> cur.cell <- Three (x, y, z)))

let update_back d f =
  match d.cur with
  | Empty -> ()
  | Node cur ->
    let n = cur.prev in
    (match n.cell with
    | One x ->
      (match f x with
      | None ->
        if Stdlib.( != ) cur.prev cur then
          remove_node_ n
        else
          d.cur <- Empty
      | Some x -> n.cell <- One x)
    | Two (x, y) ->
      (match f y with
      | None -> n.cell <- One x
      | Some y -> n.cell <- Two (x, y))
    | Three (x, y, z) ->
      (match f z with
      | None -> n.cell <- Two (x, y)
      | Some z -> n.cell <- Three (x, y, z)))

let iter f d =
  let rec iter f ~first n =
    (match n.cell with
    | One x -> f x
    | Two (x, y) ->
      f x;
      f y
    | Three (x, y, z) ->
      f x;
      f y;
      f z);
    if n.next != first then iter f ~first n.next
  in
  match d.cur with
  | Empty -> ()
  | Node cur -> iter f ~first:cur cur

let append_front ~into q = iter (push_front into) q
let append_back ~into q = iter (push_back into) q

let fold f acc d =
  let rec aux ~first f acc n =
    let acc =
      match n.cell with
      | One x -> f acc x
      | Two (x, y) -> f (f acc x) y
      | Three (x, y, z) -> f (f (f acc x) y) z
    in
    if Stdlib.( == ) n.next first then
      acc
    else
      aux ~first f acc n.next
  in
  match d.cur with
  | Empty -> acc
  | Node cur -> aux ~first:cur f acc cur

let length d = d.size

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let add_iter_back q seq = seq (fun x -> push_back q x)
let add_iter_front q seq = seq (fun x -> push_front q x)

let of_iter seq =
  let deque = create () in
  seq (fun x -> push_back deque x);
  deque

let to_iter d k = iter k d

let of_list l =
  let q = create () in
  List.iter (push_back q) l;
  q

let to_rev_list q = fold (fun l x -> x :: l) [] q
let to_list q = List.rev (to_rev_list q)

let size_cell_ = function
  | One _ -> 1
  | Two _ -> 2
  | Three _ -> 3

(* filter over a cell *)
let filter_cell_ f = function
  | One x as c ->
    if f x then
      Some c
    else
      None
  | Two (x, y) as c ->
    let fx = f x in
    let fy = f y in
    (match fx, fy with
    | true, true -> Some c
    | true, false -> Some (One x)
    | false, true -> Some (One y)
    | _ -> None)
  | Three (x, y, z) as c ->
    let fx = f x in
    let fy = f y in
    let fz = f z in
    (match fx, fy, fz with
    | true, true, true -> Some c
    | true, true, false -> Some (Two (x, y))
    | true, false, true -> Some (Two (x, z))
    | true, false, false -> Some (One x)
    | false, true, true -> Some (Two (y, z))
    | false, true, false -> Some (One y)
    | false, false, true -> Some (One z)
    | false, false, false -> None)

let filter_in_place (d : _ t) f : unit =
  (* update size, compute new cell *)
  let update_local_ n =
    d.size <- d.size - size_cell_ n.cell;
    match filter_cell_ f n.cell with
    | None -> None
    | Some n as new_cell ->
      d.size <- d.size + size_cell_ n;
      new_cell
  in
  let rec loop ~stop_at n : unit =
    if n != stop_at then (
      let n_prev = n.prev in
      let n_next = n.next in
      let new_cell = update_local_ n in
      (* merge into previous cell *)
      (match n_prev.cell, new_cell with
      | _, None -> remove_node_ n
      | Three _, Some new_cell -> n.cell <- new_cell
      | One x, Some (One y) ->
        remove_node_ n;
        n_prev.cell <- Two (x, y)
      | One x, Some (Two (y, z)) | Two (x, y), Some (One z) ->
        remove_node_ n;
        n_prev.cell <- Three (x, y, z)
      | One x, Some (Three (y, z, w)) | Two (x, y), Some (Two (z, w)) ->
        n_prev.cell <- Three (x, y, z);
        n.cell <- One w
      | Two (x, y), Some (Three (z, w1, w2)) ->
        n_prev.cell <- Three (x, y, z);
        n.cell <- Two (w1, w2));
      loop ~stop_at n_next
    )
  in
  let rec new_first_cell ~stop_at n =
    if n != stop_at then (
      match update_local_ n with
      | None -> new_first_cell ~stop_at n.next
      | Some c ->
        n.cell <- c;
        Some n
    ) else
      None
  in
  match d.cur with
  | Empty -> ()
  | Node cur ->
    (* special case for first cell *)
    (match update_local_ cur with
    | None ->
      (match new_first_cell ~stop_at:cur cur.next with
      | None -> d.cur <- Empty
      | Some n ->
        cur.prev.next <- n;
        n.prev <- cur.prev;
        d.cur <- Node n;
        loop ~stop_at:n n.next)
    | Some c ->
      cur.cell <- c;
      loop ~stop_at:cur cur.next)

let filter f q =
  let q' = create () in
  iter (fun x -> if f x then push_back q' x) q;
  q'

let filter_map f q =
  let q' = create () in
  iter
    (fun x ->
      match f x with
      | None -> ()
      | Some y -> push_back q' y)
    q;
  q'

let rec gen_iter_ f g =
  match g () with
  | None -> ()
  | Some x ->
    f x;
    gen_iter_ f g

let of_gen g =
  let q = create () in
  gen_iter_ (fun x -> push_back q x) g;
  q

let to_gen q =
  match q.cur with
  | Empty -> fun () -> None
  | Node cur ->
    let first = cur in
    let cell = ref (Some cur.cell) in
    let cur = ref cur in
    let rec next () =
      match !cell with
      | None when Stdlib.( == ) !cur.next first -> None
      | None ->
        (* go to next node *)
        let n = !cur in
        cur := n.next;
        cell := Some n.next.cell;
        next ()
      | Some (One x) ->
        cell := None;
        Some x
      | Some (Two (x, y)) ->
        cell := Some (One y);
        Some x
      | Some (Three (x, y, z)) ->
        cell := Some (Two (y, z));
        Some x
    in
    next

(* naive implem of copy, for now *)
let copy d =
  let d' = create () in
  iter (fun x -> push_back d' x) d;
  d'

let equal ~eq a b =
  let rec aux eq a b =
    match a (), b () with
    | None, None -> true
    | None, Some _ | Some _, None -> false
    | Some x, Some y -> eq x y && aux eq a b
  in
  aux eq (to_gen a) (to_gen b)

let compare ~cmp a b =
  let rec aux cmp a b =
    match a (), b () with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some x, Some y ->
      let c = cmp x y in
      if c = 0 then
        aux cmp a b
      else
        c
  in
  aux cmp (to_gen a) (to_gen b)

type 'a printer = Format.formatter -> 'a -> unit

let pp pp_x out d =
  let first = ref true in
  Format.fprintf out "@[<hov2>deque {";
  iter
    (fun x ->
      if !first then
        first := false
      else
        Format.fprintf out ";@ ";
      pp_x out x)
    d;
  Format.fprintf out "}@]"
