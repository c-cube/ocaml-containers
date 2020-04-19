(* This file is free software, part of containers. See file "license" for more details. *)

(*$inject

  let _listuniq =
    let g = Q.(small_list (pair small_int small_int)) in
    Q.map_same_type
      (fun l ->
        CCList.sort_uniq ~cmp:(fun a b -> Stdlib.compare (fst a)(fst b)) l
      ) g
  ;;
*)

(** {1 Hash Tries} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(* TODO
   (** {2 Transient IDs} *)
   module Transient = struct
   type state = { mutable frozen: bool }
   type t = Nil | St of state
   let empty = Nil
   let equal a b = Stdlib.(==) a b
   let create () = St {frozen=false}
   let active = function Nil -> false | St st -> not st.frozen
   let frozen = function Nil -> true | St st -> st.frozen
   let freeze = function Nil -> () | St st -> st.frozen <- true
   let with_ f =
    let r = create() in
    try
      let x = f r in
      freeze r;
      x
    with e ->
      freeze r;
      raise e
   exception Frozen
   end
*)

(* function array *)
module A = struct
  type 'a t = 'a array

  let length_log = 5
  let max_length = 32
  let mask = max_length-1

  let () = assert (max_length = 1 lsl length_log)

  let length = Array.length
  let iteri = Array.iteri
  let iter = Array.iter
  let fold = Array.fold_left
  let map = Array.map

  let iteri_rev f a =
    for i = length a-1 downto 0 do f i a.(i) done

  let create () = [| |]

  let empty = [| |]
  let is_empty a = length a = 0

  let return x = [| x |]

  let get a i =
    if i<0 || i >= length a then invalid_arg "A.get";
    Array.unsafe_get a i

  (* push at the back *)
  let push x a =
    let n = length a in
    if n = max_length then invalid_arg "A.push";
    let arr = Array.make (n+1) x in
    Array.blit a 0 arr 0 n;
    arr

  let pop a =
    let n = length a in
    if n=0 then invalid_arg "A.pop";
    Array.sub a 0 (n-1)

  let append a b =
    let n_a = length a in
    let n_b = length b in
    if n_a + n_b > max_length then invalid_arg "A.append";
    if n_a = 0 then b
    else if n_b = 0 then a
    else (
      let arr = Array.make (n_a+n_b) (a.(0)) in
      Array.blit a 0 arr 0 n_a;
      Array.blit b 0 arr n_a n_b;
      arr
    )

  let set ~mut a i x =
    if i<0 || i > length a || i >= max_length then invalid_arg "A.set";
    if i=length a then (
      (* insert in a longer copy *)
      let arr = Array.make (i+1) x in
      Array.blit a 0 arr 0 i;
      arr
    ) else if mut then (
      (* replace element at [i] in place *)
      a.(i) <- x;
      a
    ) else (
      (* replace element at [i] in copy *)
      let arr = Array.copy a in
      arr.(i) <- x;
      arr
    )
end

(** {2 Functors} *)

type 'a t = {
  size: int;
  leaves: 'a A.t;
  subs: 'a t A.t;
}
(* invariant:
   - [A.length leaves < A.max_length ==> A.is_empty subs]
   - either:
    * [exists n. forall i. subs[i].size = n] (all subtrees of same size)
    * [exists n i.
        (forall j<i. sub[j].size=32^{n+1}-1) &
        (forall j>=i, sub[j].size<32^{n+1}-1)]
    (prefix of subs has size of complete binary tree; suffix has
     smaller size (actually decreasing))
*)


let empty = {size=0; leaves=A.empty; subs=A.empty}

let is_empty {size;_} = size=0

(*$T
  is_empty empty
*)

let length {size;_} = size

(*$T
  not (is_empty (return 2))
  length (return 2) = 1
*)

let return x = {leaves=A.return x; subs=A.empty; size=1}

type idx_l =
  | I_one of int
  | I_cons of int * idx_l

(* split an index into a low and high parts *)
let low_idx_ i = i land A.mask

let high_idx_ i = i lsr A.length_log

let combine_idx i j = (i lsl A.length_log) lor j

(* split an index into a high part, < 32, and a low part *)
let split_idx i : idx_l =
  let rec aux high low =
    if high = 0 then low
    else if high < A.max_length then I_cons (high-1, low)
    else aux (high_idx_ high) (I_cons (low_idx_ high, low))
  in
  aux (high_idx_ i) (I_one(low_idx_ i))

let get_ (i:int) (m:'a t) : 'a =
  let rec aux l m = match l with
    | I_one x ->
      assert (x < A.length m.leaves);
      A.get m.leaves x
    | I_cons (x, tl) -> aux tl (A.get m.subs x)
  in
  aux (split_idx i) m

(*$Q
   _listuniq (fun l -> \
    let m = of_list l in \
    List.for_all (fun (i,y) -> get_exn i m = y) @@ List.mapi CCPair.make l)
*)

let get_exn i v =
  if i >= 0 && i < length v then get_ i v else raise Not_found

let get i v =
  if i >= 0 && i < length v then Some (get_ i v) else None

let push_ (i:int) (x:'a) (m:'a t) : 'a t =
  let rec aux l m = match l with
    | I_one i ->
      assert (i=A.length m.leaves);
      assert (A.length m.leaves < A.max_length);
      assert (A.is_empty m.subs);
      {m with size=m.size+1; leaves=A.push x m.leaves}
    | I_cons (i,tl) -> aux_replace_sub tl m i
  and aux_replace_sub l m x =
    assert (x <= A.length m.subs);
    (* insert in subtree, possibly a new one *)
    let sub_m =
      if x < A.length m.subs then A.get m.subs x else empty
    in
    let sub_m = aux l sub_m in
    {m with size=m.size+1; subs=A.set ~mut:false m.subs x sub_m}
  in
  aux (split_idx i) m

let push x (v:_ t) : _ t = push_ v.size x v

let pop_ i (m:'a t) : 'a * 'a t =
  let rec aux l m = match l with
    | I_one x ->
      assert (x+1 = A.length m.leaves); (* last one *)
      let x = A.get m.leaves x in
      x, {m with size=m.size-1; leaves=A.pop m.leaves}
    | I_cons (x,tl) -> aux_remove_sub tl m x
  and aux_remove_sub l m x =
    let sub = A.get m.subs x in
    let y, sub' = aux l sub in
    if is_empty sub' then (
      assert (x+1 = A.length m.subs); (* last one *)
      y, {m with size=m.size-1; subs=A.pop m.subs}
    ) else (
      y, {m with size=m.size-1; subs=A.set ~mut:false m.subs x sub'}
    )
  in
  aux (split_idx i) m

let pop_exn (v:'a t) : 'a * 'a t =
  if v.size=0 then failwith "Fun_vec.pop_exn";
  pop_ (v.size-1) v

let pop (v:'a t) : ('a * 'a t) option =
  if v.size=0 then None else Some (pop_ (v.size-1) v)

(* regression test for #298 *)
(*$R
  let rec consume x = match CCFun_vec.pop x with
    | None -> () | Some (_, x) -> consume x
  in
  consume (of_list (CCList.(1 -- 100)));
  ()
*)

(*$QR
    Q.(pair int (small_list int)) (fun (x,l) ->
        let q0 = of_list l in
        let q = push x q0 in
        assert_equal (length q) (length q0+1);
        let y, q = pop_exn q in
        assert_equal x y;
        assert_equal (to_list q) (to_list q0);
        true
      )
    *)

let iteri ~f (m : 'a t) : unit =
  (* basically, a 32-way BFS traversal.
     The queue contains subtrees to explore, along with their high_idx_ offsets *)
  let q : (int * 'a t) Queue.t = Queue.create() in
  Queue.push (0,m) q;
  while not (Queue.is_empty q) do
    let high, m = Queue.pop q in
    A.iteri (fun i x -> f (combine_idx high i) x) m.leaves;
    A.iteri (fun i sub -> Queue.push (combine_idx i high, sub) q) m.subs;
  done

let iteri_rev ~f (m : 'a t) : unit =
  (* like {!iteri} but last element comes first *)
  let rec aux high m =
    A.iteri_rev (fun i sub -> aux (combine_idx i high) sub) m.subs;
    (* only now, explore current leaves *)
    A.iteri_rev (fun i x -> f (combine_idx high i) x) m.leaves;
  in
  aux 0 m

let foldi ~f ~x m =
  let acc = ref x in
  iteri m
    ~f:(fun i x -> acc := f !acc i x);
  !acc

let foldi_rev ~f ~x m =
  let acc = ref x in
  iteri_rev m
    ~f:(fun i x -> acc := f !acc i x);
  !acc

let iter ~f m = iteri ~f:(fun _ x -> f x) m

let fold ~f ~x m = foldi ~f:(fun acc _ x -> f acc x) ~x m

let fold_rev ~f ~x m = foldi_rev ~f:(fun acc _ x -> f acc x) ~x m

let rec map f m : _ t =
  { subs=A.map (map f) m.subs;
    leaves=A.map f m.leaves;
    size=m.size;
  }

(*$QR
  Q.(pair (fun1 Observable.int bool)(small_list int)) (fun (f,l) ->
    let f = Q.Fn.apply f in
    (List.map f l) = (of_list l |> map f |> to_list)
  )
*)

let append a b =
  if is_empty b then a
  else fold ~f:(fun v x -> push x v) ~x:a b

(*$QR
  Q.(pair (small_list int)(small_list int)) (fun (l1,l2) ->
    (l1 @ l2) = (append (of_list l1)(of_list l2) |> to_list)
  )
*)

let add_list v l = List.fold_left (fun v x -> push x v) v l

let of_list l = add_list empty l

let to_list m = fold_rev m ~f:(fun acc x -> x::acc) ~x:[]

(*$QR
  Q.(small_list int) (fun l ->
    l = to_list (of_list l))
*)

let add_seq v seq =
  let v = ref v in
  seq (fun x -> v := push x !v);
  !v

let of_seq s = add_seq empty s

let to_seq m yield = iteri ~f:(fun _ v -> yield v) m

(*$Q
  _listuniq (fun l -> \
    (List.sort Stdlib.compare l) = \
      (l |> Iter.of_list |> of_seq |> to_seq |> Iter.to_list \
        |> List.sort Stdlib.compare) )
*)

let rec add_gen m g = match g() with
  | None -> m
  | Some x -> add_gen (push x m) g

let of_gen g = add_gen empty g

(* traverse the tree by increasing hash order, where the order compares
   hashes lexicographically by A.length_log-wide chunks of bits,
   least-significant chunks first *)
let to_gen m =
  let q_cur : 'a Queue.t = Queue.create() in
  let q_sub : 'a t Queue.t = Queue.create() in
  Queue.push m q_sub;
  let rec next() =
    if not (Queue.is_empty q_cur) then (
      Some (Queue.pop q_cur)
    ) else if not (Queue.is_empty q_sub) then (
      let m = Queue.pop q_sub in
      A.iter (fun x -> Queue.push x q_cur) m.leaves;
      A.iter (fun sub -> Queue.push sub q_sub) m.subs;
      next()
    ) else None
  in next

(*$Q
  _listuniq (fun l -> \
    (List.sort Stdlib.compare l) = \
      (l |> Gen.of_list |> of_gen |> to_gen |> Gen.to_list \
        |> List.sort Stdlib.compare) )
*)

let choose m = to_gen m ()

(*$T
  choose empty = None
  choose (of_list [1,1; 2,2]) <> None
*)

let choose_exn m = match choose m with
  | None -> raise Not_found
  | Some (k,v) -> k, v

let pp ppv out m =
  let first = ref true in
  iter m
    ~f:(fun v ->
      if !first then first := false else Format.fprintf out ";@ ";
      ppv out v
    )
