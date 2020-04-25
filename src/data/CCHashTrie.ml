(* This file is free software, part of containers. See file "license" for more details. *)

(*$inject
  module M = Make(CCInt) ;;

  let _listuniq =
    let g = Q.(list (pair small_int small_int)) in
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

(** {2 Transient IDs} *)
module Transient = struct
  type t = { mutable frozen: bool }
  let empty = {frozen=true} (* special value *)
  let equal a b = Stdlib.(==) a b
  let create () = {frozen=false}
  let active st =not st.frozen
  let frozen st = st.frozen
  let freeze st = st.frozen <- true
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

module type S = sig
  type key

  type 'a t

  val empty : 'a t

  val is_empty : _ t -> bool

  val singleton : key -> 'a -> 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val mem : key -> _ t -> bool

  val get : key -> 'a t -> 'a option

  val get_exn : key -> 'a t -> 'a
  (** @raise Not_found if key not present *)

  val remove : key -> 'a t -> 'a t
  (** Remove the key, if present. *)

  val update : key -> f:('a option -> 'a option) -> 'a t -> 'a t
  (** [update k ~f m] calls [f (Some v)] if [get k m = Some v], [f None]
      otherwise. Then, if [f] returns [Some v'] it binds [k] to [v'],
      if [f] returns [None] it removes [k] *)

  val add_mut : id:Transient.t -> key -> 'a -> 'a t -> 'a t
  (** [add_mut ~id k v m] behaves like [add k v m], except it will mutate
      in place whenever possible. Changes done with an [id] might affect all
      versions of the structure obtained with the same [id] (but not
      other versions).
      @raise Transient.Frozen if [id] is frozen *)

  val remove_mut : id:Transient.t -> key -> 'a t -> 'a t
  (** Same as {!remove}, but modifies in place whenever possible
      @raise Transient.Frozen if [id] is frozen *)

  val update_mut : id:Transient.t -> key -> f:('a option -> 'a option) -> 'a t -> 'a t
  (** Same as {!update} but with mutability
      @raise Transient.Frozen if [id] is frozen *)

  val cardinal : _ t -> int

  val choose : 'a t -> (key * 'a) option

  val choose_exn : 'a t -> key * 'a
  (** @raise Not_found if not pair was found *)

  val iter : f:(key -> 'a -> unit) -> 'a t -> unit

  val fold : f:('b -> key -> 'a -> 'b) -> x:'b -> 'a t -> 'b

  (** {6 Conversions} *)

  val to_list : 'a t -> (key * 'a) list

  val add_list : 'a t -> (key * 'a) list -> 'a t

  val add_list_mut : id:Transient.t -> 'a t -> (key * 'a) list -> 'a t
  (** @raise Frozen if the ID is frozen *)

  val of_list : (key * 'a) list -> 'a t

  val add_seq : 'a t -> (key * 'a) sequence -> 'a t

  val add_seq_mut : id:Transient.t -> 'a t -> (key * 'a) sequence -> 'a t
  (** @raise Frozen if the ID is frozen *)

  val of_seq : (key * 'a) sequence -> 'a t

  val to_seq : 'a t -> (key * 'a) sequence

  val add_gen : 'a t -> (key * 'a) gen -> 'a t

  val add_gen_mut : id:Transient.t -> 'a t -> (key * 'a) gen -> 'a t
  (** @raise Frozen if the ID is frozen *)

  val of_gen : (key * 'a) gen -> 'a t

  val to_gen : 'a t -> (key * 'a) gen

  (** {6 IO} *)

  val pp : key printer -> 'a printer -> 'a t printer

  val as_tree : 'a t -> [`L of int * (key * 'a) list | `N ] ktree
  (** For debugging purpose: explore the structure of the tree,
      with [`L (h,l)] being a leaf (with shared hash [h])
      and [`N] an inner node *)
end

module type KEY = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(*
  from https://en.wikipedia.org/wiki/Hamming_weight

  //This uses fewer arithmetic operations than any other known
  //implementation on machines with slow multiplication.
  //It uses 17 arithmetic operations.
  int popcount_2(uint64_t x) {
    x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
    x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits
    x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits
    x += x >>  8;  //put count of each 16 bits into their lowest 8 bits
    x += x >> 16;  //put count of each 32 bits into their lowest 8 bits
    x += x >> 32;  //put count of each 64 bits into their lowest 8 bits
    return x & 0x7f;
  }

   m1 = 0x5555555555555555
   m2 = 0x3333333333333333
   m4 = 0x0f0f0f0f0f0f0f0f

   We use Int64 for our 64-bits popcount.
*)
module I64 = struct
  type t = Int64.t
  let (+) = Int64.add
  let (-) = Int64.sub
  let (lsl) = Int64.shift_left
  let (lsr) = Int64.shift_right_logical
  let (land) = Int64.logand
  let (lor) = Int64.logor
  let lnot = Int64.lognot
end

let popcount (b:I64.t) : int =
  let open I64 in
  let b = b - ((b lsr 1) land 0x5555555555555555L) in
  let b = (b land 0x3333333333333333L) + ((b lsr 2) land 0x3333333333333333L) in
  let b = (b + (b lsr 4)) land 0x0f0f0f0f0f0f0f0fL in
  let b = b + (b lsr 8) in
  let b = b + (b lsr 16) in
  let b = b + (b lsr 32) in
  Int64.to_int (b land 0x7fL)

(*$T
  popcount 5L = 2
  popcount 256L = 1
  popcount 255L = 8
  popcount 0xFFFFL = 16
  popcount 0xFF1FL = 13
  popcount 0xFFFFFFFFL = 32
  popcount 0xFFFFFFFFFFFFFFFFL = 64
*)

(*$Q
  Q.int (fun i -> let i = Int64.of_int i in popcount i <= 64)
*)

(* sparse array, using a bitfield and POPCOUNT *)
module A_SPARSE = struct
  type 'a t = {
    bits: int64;
    arr: 'a array;
    id: Transient.t;
  }

  let length_log = 6
  let length = 1 lsl length_log

  let () = assert (length = 64)

  let create ~id = { bits=0L; arr= [| |]; id; }

  let owns ~id a =
    Transient.active id && Transient.equal id a.id

  let get ~default a i =
    let open I64 in
    let idx = 1L lsl i in
    if a.bits land idx = 0L then (
      default
    ) else (
      let real_idx = popcount (a.bits land (idx - 1L)) in
      a.arr.(real_idx)
    )

  let set ~mut a i x =
    let open I64 in
    let idx = 1L lsl i in
    let real_idx = popcount (a.bits land (idx - 1L)) in
    if (a.bits land idx = 0L) then (
      (* insert at [real_idx] in a new array *)
      let bits = a.bits lor idx in
      let n = Array.length a.arr in
      let arr = Array.make Stdlib.(n+1) x in
      arr.(real_idx) <- x;
      if real_idx>0 then (
        Array.blit a.arr 0 arr 0 real_idx;
      );
      if real_idx<n then (
        let open Stdlib in
        Array.blit a.arr real_idx arr (real_idx+1) (n-real_idx);
      );
      {a with bits; arr}
    ) else (
      (* replace element at [real_idx] *)
      if mut then (
        a.arr.(real_idx) <- x;
        a
      ) else (
        let arr = if mut then a.arr else Array.copy a.arr in
        arr.(real_idx) <- x;
        {a with arr}
      )
    )

  let update ~mut ~default a i f =
    let open I64 in
    let idx = 1L lsl i in
    let real_idx = popcount (a.bits land (idx - 1L)) in
    if a.bits land idx = 0L then (
      (* not present *)
      let x = f default in
      (* insert at [real_idx] in a new array *)
      let bits = a.bits lor idx in
      let n = Array.length a.arr in
      let arr = Array.make Stdlib.(n+1) x in
      if real_idx>0 then (
        Array.blit a.arr 0 arr 0 real_idx;
      );
      if real_idx<n then (
        let open Stdlib in
        Array.blit a.arr real_idx arr (real_idx+1) (n-real_idx);
      );
      {a with bits; arr}
    ) else (
      let x = f a.arr.(real_idx) in
      (* replace element at [real_idx] *)
      let arr = if mut then a.arr else Array.copy a.arr in
      arr.(real_idx) <- x;
      {a with arr}
    )

  let remove a i =
    let open I64 in
    let idx = 1L lsl i in
    let real_idx = popcount (a.bits land (idx - 1L)) in
    if a.bits land idx = 0L then (
      a (* not present *)
    ) else (
      (* remove at [real_idx] *)
      let bits = a.bits land (lnot idx) in
      let n = Array.length a.arr in
      let arr = if n=1 then [||] else Array.make Stdlib.(n-1) a.arr.(0) in
      let open Stdlib in
      if real_idx > 0 then (
        Array.blit a.arr 0 arr 0 real_idx;
      );
      if real_idx+1 < n then (
        Array.blit a.arr (real_idx+1) arr real_idx (n-real_idx-1);
      );
      {a with bits; arr}
    )

  let iter f a = Array.iter f a.arr

  let fold f acc a = Array.fold_left f acc a.arr
end

(** {2 Functors} *)

module Make(Key : KEY)
  : S with type key = Key.t
= struct
  module A = A_SPARSE

  let () = assert (A.length = 1 lsl A.length_log)

  module Hash : sig
    type t = private int
    val make : Key.t -> t
    val zero : t (* special "hash" *)
    val is_0 : t -> bool
    val equal : t -> t -> bool
    val rem : t -> int (* [A.length_log] last bits *)
    val quotient : t -> t (* remove [A.length_log] last bits *)
  end = struct
    type t = int
    let make = Key.hash
    let zero = 0
    let is_0 h = h = 0
    let equal : int -> int -> bool = Stdlib.(=)
    let rem h = h land (A.length - 1)
    let quotient h = h lsr A.length_log
  end

  let hash_ = Hash.make

  type key = Key.t

  (* association list, without duplicates *)
  type 'a leaf =
    | Nil
    | One of key * 'a
    | Two of key * 'a * key * 'a
    | Cons of key * 'a * 'a leaf

  type 'a t =
    | E
    | S of Hash.t * key * 'a  (* single pair *)
    | L of Hash.t * 'a leaf (* same hash for all elements *)
    | N of 'a leaf * 'a t A.t  (* leaf for hash=0, subnodes *)

  (* invariants:
      L [] --> E
      N [E, E,...., E] -> E
  *)

  let empty = E

  let is_empty = function
    | E -> true
    | L (_, Nil) -> assert false
    | S _ | L _ | N _
      -> false

  (*$T
    M.is_empty M.empty
  *)

  let leaf_ k v ~h = L (h, Cons(k,v,Nil))

  let singleton k v = leaf_ k v ~h:(hash_ k)

  (*$T
    not (M.is_empty (M.singleton 1 2))
    M.cardinal (M.singleton 1 2) = 1
  *)

  let rec get_exn_list_ k l = match l with
    | Nil -> raise Not_found
    | One (k', v') -> if Key.equal k k' then v' else raise Not_found
    | Two (k1, v1, k2, v2) ->
      if Key.equal k k1 then v1
      else if Key.equal k k2 then v2
      else raise Not_found
    | Cons (k', v', tail) ->
      if Key.equal k k' then v' else get_exn_list_ k tail

  let rec get_exn_ k ~h m = match m with
    | E -> raise Not_found
    | S (_, k', v') -> if Key.equal k k' then v' else raise Not_found
    | L (_, l) -> get_exn_list_ k l
    | N (leaf, a) ->
      if Hash.is_0 h then get_exn_list_ k leaf
      else (
        let i = Hash.rem h in
        let h' = Hash.quotient h in
        get_exn_ k ~h:h' (A.get ~default:E a i)
      )

  let get_exn k m = get_exn_ k ~h:(hash_ k) m

  (*$Q
     _listuniq (fun l -> \
      let m = M.of_list l in \
      List.for_all (fun (x,y) -> M.get_exn x m = y) l)
  *)

  let get k m =
    try Some (get_exn_ k ~h:(hash_ k) m)
    with Not_found -> None

  let mem k m =
    try ignore (get_exn_ k ~h:(hash_ k) m); true
    with Not_found -> false

  (* TODO: use Hash.combine if array only has one non-empty LEAF element? *)

  (* add [k,v] to the list [l], removing old binding if any *)
  let rec add_list_ k v l = match l with
    | Nil -> One (k,v)
    | One (k1, v1) ->
      if Key.equal k k1 then One (k, v) else Two (k,v,k1,v1)
    | Two (k1, v1, k2, v2) ->
      if Key.equal k k1 then Two (k, v, k2, v2)
      else if Key.equal k k2 then Two (k, v, k1, v1)
      else Cons (k, v, l)
    | Cons (k', v', tail) ->
      if Key.equal k k'
      then Cons (k, v, tail) (* replace *)
      else Cons (k', v', add_list_ k v tail)

  let node_ leaf a = N (leaf, a)

  (* [h]: hash, with the part required to reach this leaf removed
      [id] is the transient ID used for mutability *)
  let rec add_ ~id k v ~h m = match m with
    | E -> S (h, k, v)
    | S (h', k', v') ->
      if Hash.equal h h' then (
        if Key.equal k k'
        then S (h, k, v)  (* replace *)
        else L (h, Cons (k, v, Cons (k', v', Nil)))
      ) else (
        make_array_ ~id ~leaf:(Cons (k', v', Nil)) ~h_leaf:h' k v ~h
      )
    | L (h', l) ->
      if Hash.equal h h'
      then L (h, add_list_ k v l)
      else (* split into N *)
        make_array_ ~id ~leaf:l ~h_leaf:h' k v ~h
    | N (leaf, a) ->
      if Hash.is_0 h
      then node_ (add_list_ k v leaf) a
      else (
        let mut = A.owns ~id a in (* can we modify [a] in place? *)
        node_ leaf (add_to_array_ ~id ~mut k v ~h a)
      )

  (* make an array containing a leaf, and insert (k,v) in it *)
  and make_array_ ~id ~leaf ~h_leaf:h' k v ~h =
    let a = A.create ~id in
    let a, leaf =
      if Hash.is_0 h' then a, leaf else (
        (* put leaf in the right bucket *)
        let i = Hash.rem h' in
        let h'' = Hash.quotient h' in
        A.set ~mut:true a i (L (h'', leaf)), Nil
      )
    in
    (* then add new node *)
    let a, leaf =
      if Hash.is_0 h then a, add_list_ k v leaf
      else add_to_array_ ~id ~mut:true k v ~h a, leaf
    in
    N (leaf, a)

  (* add k->v to [a] *)
  and add_to_array_ ~id ~mut k v ~h a =
    (* insert in a bucket *)
    let i = Hash.rem h in
    let h' = Hash.quotient h in
    A.update ~default:E ~mut a i (fun x -> add_ ~id k v ~h:h' x)

  let add k v m = add_ ~id:Transient.empty k v ~h:(hash_ k) m

  (*$Q
     _listuniq (fun l -> \
        let m = List.fold_left (fun m (x,y) -> M.add x y m) M.empty l in \
        List.for_all (fun (x,y) -> M.get_exn x m = y) l)
  *)

  let add_mut ~id k v m =
    if Transient.frozen id then raise Transient.Frozen;
    add_ ~id k v ~h:(hash_ k) m

  (*$R
    let lsort = List.sort Stdlib.compare in
    let m = M.of_list [1, 1; 2, 2] in
    let id = Transient.create() in
    let m' = M.add_mut ~id 3 3 m in
    let m' = M.add_mut ~id 4 4 m' in
    assert_equal [1, 1; 2, 2] (M.to_list m |> lsort);
    assert_equal [1, 1; 2, 2; 3,3; 4,4] (M.to_list m' |> lsort);
    Transient.freeze id;
    assert_bool "must raise"
      (try ignore(M.add_mut ~id 5 5 m'); false with Transient.Frozen -> true)
  *)


  exception LocalExit

  let is_empty_arr_ a =
    try
      A.iter (fun t -> if not (is_empty t) then raise LocalExit) a;
      true
    with LocalExit -> false

  let is_empty_list_ = function
    | Nil -> true
    | One _
    | Two _
    | Cons _ -> false

  let rec remove_list_ k l = match l with
    | Nil -> Nil
    | One (k', _) ->
      if Key.equal k k' then Nil else l
    | Two (k1, v1, k2, v2) ->
      if Key.equal k k1 then One (k2, v2)
      else if Key.equal k k2 then One (k1, v1)
      else l
    | Cons (k', v', tail) ->
      if Key.equal k k'
      then tail
      else Cons (k', v', remove_list_ k tail)

  let rec remove_rec_ ~id k ~h m = match m with
    | E -> E
    | S (_, k', _) ->
      if Key.equal k k' then E else m
    | L (h, l) ->
      let l = remove_list_ k l in
      if is_empty_list_ l then E else L (h, l)
    | N (leaf, a) ->
      let leaf, a =
        if Hash.is_0 h
        then remove_list_ k leaf, a
        else (
          let i = Hash.rem h in
          let h' = Hash.quotient h in
          let new_t = remove_rec_ ~id k ~h:h' (A.get ~default:E a i) in
          if is_empty new_t
          then leaf, A.remove a i (* remove sub-tree *)
          else (
            let mut = A.owns ~id a in
            leaf, A.set ~mut a i new_t
          )
        )
      in
      if is_empty_list_ leaf && is_empty_arr_ a
      then E
      else N (leaf, a)

  let remove k m = remove_rec_ ~id:Transient.empty k ~h:(hash_ k) m

  let remove_mut ~id k m =
    if Transient.frozen id then raise Transient.Frozen;
    remove_rec_ ~id k ~h:(hash_ k) m

  (*$QR
    _listuniq (fun l ->
      let m = M.of_list l in
      List.for_all
        (fun (x,_) ->
          let m' = M.remove x m in
          not (M.mem x m') &&
          M.cardinal m' = M.cardinal m - 1 &&
          List.for_all
            (fun (y,v) -> y = x || M.get_exn y m' = v)
            l
      ) l
    )
  *)

  let update_ ~id k f m =
    let h = hash_ k in
    let opt_v = try Some (get_exn_ k ~h m) with Not_found -> None in
    begin match opt_v, f opt_v with
      | None, None -> m
      | Some _, Some v
      | None, Some v -> add_ ~id k v ~h m
      | Some _, None -> remove_rec_ ~id k ~h m
    end

  let update k ~f m = update_ ~id:Transient.empty k f m

  let update_mut ~id k ~f m =
    if Transient.frozen id then raise Transient.Frozen;
    update_ ~id k f m

  (*$R
    let m = M.of_list [1, 1; 2, 2; 5, 5] in
    let m' = M.update 4
      ~f:(function
      | None -> Some 4
      | Some _ -> Some 0
      ) m
    in
    assert_equal [1,1; 2,2; 4,4; 5,5] (M.to_list m' |> List.sort Stdlib.compare);
  *)

  let iter ~f t =
    let rec aux = function
      | E -> ()
      | S (_, k, v) -> f k v
      | L (_,l) -> aux_list l
      | N (l,a) -> aux_list l; A.iter aux a
    and aux_list = function
      | Nil -> ()
      | One (k,v) -> f k v
      | Two (k1,v1,k2,v2) -> f k1 v1; f k2 v2
      | Cons (k, v, tl) -> f k v; aux_list tl
    in
    aux t

  let fold ~f ~x:acc t =
    let rec aux acc t = match t with
      | E -> acc
      | S (_,k,v) -> f acc k v
      | L (_,l) -> aux_list acc l
      | N (l,a) -> let acc = aux_list acc l in A.fold aux acc a
    and aux_list acc l = match l with
      | Nil -> acc
      | One (k,v) -> f acc k v
      | Two (k1,v1,k2,v2) -> f (f acc k1 v1) k2 v2
      | Cons (k, v, tl) -> let acc = f acc k v in aux_list acc tl
    in
    aux acc t

  (*$T
    let l = CCList.(1 -- 10 |> map (fun x->x,x)) in  \
    M.of_list l \
      |> M.fold ~f:(fun acc x y -> (x,y)::acc) ~x:[] \
      |> List.sort Stdlib.compare = l
  *)

  let cardinal m = fold ~f:(fun n _ _ -> n+1) ~x:0 m

  let to_list m = fold ~f:(fun acc k v -> (k,v)::acc) ~x:[] m

  let add_list_mut ~id m l =
    List.fold_left (fun acc (k,v) -> add_mut ~id k v acc) m l

  let add_list m l =
    Transient.with_ (fun id -> add_list_mut ~id m l)

  let of_list l = add_list empty l

  let add_seq_mut ~id m seq =
    let m = ref m in
    seq (fun (k,v) -> m := add_mut ~id k v !m);
    !m

  let add_seq m seq =
    Transient.with_ (fun id -> add_seq_mut ~id m seq)

  let of_seq s = add_seq empty s

  let to_seq m yield = iter ~f:(fun k v -> yield (k,v)) m

  (*$Q
    _listuniq (fun l -> \
      (List.sort Stdlib.compare l) = \
        (l |> Iter.of_list |> M.of_seq |> M.to_seq |> Iter.to_list \
          |> List.sort Stdlib.compare) )
  *)

  let rec add_gen_mut ~id m g = match g() with
    | None -> m
    | Some (k,v) -> add_gen_mut ~id (add_mut ~id k v m) g

  let add_gen m g =
    Transient.with_ (fun id -> add_gen_mut ~id m g)

  let of_gen g = add_gen empty g

  (* traverse the tree by increasing hash order, where the order compares
     hashes lexicographically by A.length_log-wide chunks of bits,
     least-significant chunks first *)
  let to_gen m =
    let st = Stack.create() in
    Stack.push m st;
    let rec next() =
      if Stack.is_empty st then None
      else match Stack.pop st with
        | E -> next ()
        | S (_,k,v) -> Some (k,v)
        | L (_, Nil) -> next()
        | L (_, One (k,v)) -> Some (k,v)
        | L (h, Two (k1,v1,k2,v2)) ->
          Stack.push (L (h, One (k2,v2))) st;
          Some (k1,v1)
        | L (h, Cons(k,v,tl)) ->
          Stack.push (L (h, tl)) st;  (* tail *)
          Some (k,v)
        | N (l, a) ->
          A.iter
            (fun sub -> Stack.push sub st)
            a;
          Stack.push (L (Hash.zero, l)) st;  (* leaf *)
          next()
    in
    next

  (*$Q
    _listuniq (fun l -> \
      (List.sort Stdlib.compare l) = \
        (l |> Gen.of_list |> M.of_gen |> M.to_gen |> Gen.to_list \
          |> List.sort Stdlib.compare) )
  *)

  let choose m = to_gen m ()

  (*$T
    M.choose M.empty = None
    M.choose M.(of_list [1,1; 2,2]) <> None
  *)

  let choose_exn m = match choose m with
    | None -> raise Not_found
    | Some (k,v) -> k, v

  let pp ppk ppv out m =
    let first = ref true in
    iter m
      ~f:(fun k v ->
        if !first then first := false else Format.fprintf out ";@ ";
        ppk out k;
        Format.pp_print_string out " -> ";
        ppv out v
      )

  let rec as_tree m () = match m with
    | E -> `Nil
    | S (h,k,v) -> `Node (`L ((h:>int), [k,v]), [])
    | L (h,l) -> `Node (`L ((h:>int), list_as_tree_ l), [])
    | N (l,a) -> `Node (`N, as_tree (L (Hash.zero, l)) :: array_as_tree_ a)
  and list_as_tree_ l = match l with
    | Nil -> []
    | One (k,v) -> [k,v]
    | Two (k1,v1,k2,v2) -> [k1,v1; k2,v2]
    | Cons (k, v, tail) -> (k,v) :: list_as_tree_ tail
  and array_as_tree_ a = A.fold (fun acc t -> as_tree t :: acc) [] a
end

(*$R
  let m = M.of_list CCList.( (501 -- 1000) @ (500 -- 1) |> map (fun i->i,i)) in
  assert_equal ~printer:CCInt.to_string 1000 (M.cardinal m);
  assert_bool "check all get"
    (Iter.for_all (fun i -> i = M.get_exn i m) Iter.(1 -- 1000));
  let m = Iter.(501 -- 1000 |> fold (fun m i -> M.remove i m) m) in
  assert_equal ~printer:CCInt.to_string 500 (M.cardinal m);
  assert_bool "check all get after remove"
    (Iter.for_all (fun i -> i = M.get_exn i m) Iter.(1 -- 500));
  assert_bool "check all get after remove"
    (Iter.for_all (fun i -> None = M.get i m) Iter.(501 -- 1000));
*)

