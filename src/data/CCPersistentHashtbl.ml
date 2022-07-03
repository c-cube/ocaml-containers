
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Persistent hash-table on top of OCaml's hashtables} *)

type 'a iter = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit
type 'a equal = 'a -> 'a -> bool

module type HashedType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** {2 Signature of such a hashtable} *)

module type S = sig
  type key
  type 'a t

  val empty : unit -> 'a t
  (** Empty table. The table will be allocated at the first binding *)

  val create : int -> 'a t
  (** Create a new hashtable, with the given initial capacity *)

  val is_empty : 'a t -> bool
  (** Is the table empty? *)

  val find : 'a t -> key -> 'a
  (** Find the value for this key, or fails
      @raise Not_found if the key is not present in the table *)

  val get_exn : key -> 'a t -> 'a
  (** Synonym to {!find} with flipped arguments *)

  val get : key -> 'a t -> 'a option
  (** Safe version of !{get_exn} *)

  val mem : 'a t -> key -> bool
  (** Is the key bound? *)

  val length : _ t -> int
  (** Number of bindings *)

  val add : 'a t -> key -> 'a -> 'a t
  (** Add the binding to the table, returning a new table. The old binding
      for this key, if it exists, is shadowed and will be restored upon
      [remove tbl k].
      @since 0.14 *)

  val replace : 'a t -> key -> 'a -> 'a t
  (** Add the binding to the table, returning a new table. This erases
      the current binding for [key], if any. *)

  val update : 'a t -> key -> ('a option -> 'a option) -> 'a t
  (** [update tbl key f] calls [f None] if [key] doesn't belong in [tbl],
      [f (Some v)] if [key -> v] otherwise; If [f] returns [None] then
      [key] is removed, else it returns [Some v'] and [key -> v'] is added. *)

  val remove : 'a t -> key -> 'a t
  (** Remove the key *)

  val copy : 'a t -> 'a t
  (** Fresh copy of the table; the underlying structure is not shared
      anymore, so using both tables alternatively will be efficient *)

  val merge :
    f:(key -> [`Left of 'a | `Right of 'b | `Both of 'a * 'b] -> 'c option) ->
    'a t -> 'b t -> 'c t
  (** Merge two tables together into a new table. The function's argument
      correspond to values associated with the key (if present); if the
      function returns [None] the key will not appear in the result. *)

  val iter : 'a t -> (key -> 'a -> unit) -> unit
  (** Iterate over bindings *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold over bindings *)

  val map : (key -> 'a -> 'b) -> 'a t -> 'b t
  (** Map all values *)

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  (** {3 Conversions} *)

  val of_iter : (key * 'a) iter -> 'a t
  (** Add (replace) bindings from the sequence to the table *)

  val of_list : (key * 'a) list -> 'a t

  val add_iter : 'a t -> (key * 'a) iter -> 'a t

  val add_list : 'a t -> (key  * 'a) list -> 'a t

  val to_iter : 'a t -> (key * 'a) iter
  (** Iter of the bindings of the table *)

  val to_list : 'a t -> (key * 'a) list

  (** {3 Misc} *)

  val equal : 'a equal -> 'a t equal

  val pp : ?pp_start:unit printer -> ?pp_stop:unit printer -> ?pp_sep:unit printer ->
    ?pp_arrow:unit printer -> key printer -> 'a printer -> 'a t printer

  val stats : _ t -> Hashtbl.statistics
  (** Statistics on the internal table.
      @since 0.14 *)
end

(** {2 Implementation} *)

module Make(H : HashedType) : S with type key = H.t = struct
  type key = H.t

  (* main hashtable *)
  type 'a t = {
    mutable arr: 'a p_array; (* invariant: length is a power of 2 *)
    length: int;
  }

  (* piece of a persistent array *)
  and 'a p_array =
    | Arr of 'a bucket array
    | Set of int * 'a bucket * 'a t

  (* bucket of the hashtbl *)
  and 'a bucket =
    | Nil
    | Cons of key * 'a * 'a bucket

  (* first power of two that is bigger than [than], starting from [n] *)
  let rec power_two_larger ~than n =
    if n>= than then n else power_two_larger ~than (2*n)

  let create i =
    let i = power_two_larger ~than:i 16 in
    { length=0;
      arr=Arr (Array.make i Nil)
    }

  let empty () = create 16

  let rec reroot_rec_ t k = match t.arr with
    | Arr a -> k a
    | Set (i, v, t') ->
      reroot_rec_ t' (fun a ->
        let v' = a.(i) in
        a.(i) <- v;
        t.arr <- Arr a;
        t'.arr <- Set (i, v', t);
        k a
      )

  (* obtain the array *)
  let reroot_ t = match t.arr with
    | Arr a -> a
    | _ -> reroot_rec_ t (fun x -> x)

  let is_empty t = t.length = 0

  let length t = t.length

  (* find index of [h] in [a] *)
  let find_idx_ a ~h =
    (* bitmask 00001111 if length(a) = 10000 *)
    h land (Array.length a - 1)

  let rec find_rec_ k l = match l with
    | Nil -> raise Not_found
    | Cons (k', v', l') ->
      if H.equal k k' then v' else find_rec_ k l'

  let find t k =
    let a = reroot_ t in
    (* unroll like crazy *)
    match a.(find_idx_ ~h:(H.hash k) a) with
      | Nil -> raise Not_found
      | Cons (k1, v1, l1) ->
        if H.equal k k1 then v1
        else match l1 with
          | Nil -> raise Not_found
          | Cons (k2,v2,l2) ->
            if H.equal k k2 then v2
            else match l2 with
              | Nil -> raise Not_found
              | Cons (k3,v3,l3) ->
                if H.equal k k3 then v3
                else match l3 with
                  | Nil -> raise Not_found
                  | Cons (k4,v4,l4) ->
                    if H.equal k k4 then v4 else find_rec_ k l4

  let get_exn k t = find t k

  let get k t =
    try Some (find t k)
    with Not_found -> None

  let mem t k =
    try ignore (find t k); true
    with Not_found -> false

  let rec buck_rev_iter_ ~f l = match l with
    | Nil -> ()
    | Cons (k,v,l') -> buck_rev_iter_ ~f l'; f k v

  (* resize [a] so it has capacity [new_size], and insert [k,v] in it *)
  let resize_ k v h a new_size =
    assert (new_size > Array.length a);
    let a' = Array.make new_size Nil in
    (* preserve order of elements by iterating on each bucket in rev order *)
    Array.iter
      (buck_rev_iter_
         ~f:(fun k v ->
           let i = find_idx_ ~h:(H.hash k) a' in
           a'.(i) <- Cons (k,v,a'.(i))
         )
      )
      a;
    let i = find_idx_ ~h a' in
    a'.(i) <- Cons (k,v,a'.(i));
    a'

  (* insert [k,v] in [l] and returns new list and boolean flag indicating
     whether it's a new element *)
  let rec replace_rec_ k v l = match l with
    | Nil -> Cons (k,v,Nil), true
    | Cons (k',v',l') ->
      if H.equal k k'
      then Cons (k,v,l'), false
      else
        let l', is_new = replace_rec_ k v l' in
        Cons (k',v',l'), is_new

  let replace t k v =
    let a = reroot_ t in
    let h = H.hash k in
    let i = find_idx_ ~h a in
    match a.(i) with
      | Nil ->
        if t.length > (Array.length a) lsl 1
        then (
          (* resize *)
          let new_size = min (2 * (Array.length a)) Sys.max_array_length in
          let a = resize_ k v h a new_size in
          {length=t.length+1; arr=Arr a}
        ) else (
          a.(i) <- Cons (k, v, Nil);
          let t' = {length=t.length + 1; arr=Arr a} in
          t.arr <- Set (i,Nil,t');
          t'
        )
      | Cons _ as l ->
        let l', is_new = replace_rec_ k v l in
        if is_new && t.length > (Array.length a) lsl 1
        then (
          (* resize and insert [k,v] (again, it's new anyway) *)
          let new_size = min (2 * (Array.length a)) Sys.max_array_length in
          let a = resize_ k v h a new_size in
          {length=t.length+1; arr=Arr a}
        ) else (
          (* no resize *)
          a.(i) <- l';
          let t' = {
            length=if is_new then t.length+1 else t.length;
            arr=Arr a;
          } in
          t.arr <- Set (i,l,t');
          t'
        )

  let add t k v =
    let a = reroot_ t in
    let h = H.hash k in
    let i = find_idx_ ~h a in
    if t.length > (Array.length a) lsl 1
    then (
      (* resize *)
      let new_size = min (2 * (Array.length a)) Sys.max_array_length in
      let a = resize_ k v h a new_size in
      {length=t.length+1; arr=Arr a}
    ) else (
      (* prepend *)
      let old = a.(i) in
      a.(i) <- Cons (k, v, old);
      let t' = {length=t.length + 1; arr=Arr a} in
      t.arr <- Set (i,old,t');
      t'
    )

  (* return [Some l'] if [l] changed into [l'] by removing [k] *)
  let rec remove_rec_ k l = match l with
    | Nil -> None
    | Cons (k', v', l') ->
      if H.equal k k'
      then Some l'
      else match remove_rec_ k l' with
        | None -> None
        | Some l' -> Some (Cons (k', v', l'))

  let remove t k =
    let a = reroot_ t in
    let i = find_idx_ ~h:(H.hash k) a in
    match a.(i) with
      | Nil -> t
      | Cons _ as l ->
        match remove_rec_ k l with
          | None -> t
          | Some l' ->
            a.(i) <- l';
            let t' = {length=t.length-1; arr=Arr a} in
            t.arr <- Set (i,l,t');
            t'

  let update t k f =
    let v = get k t in
    match v, f v with
      | None, None -> t  (* no change *)
      | Some _, None -> remove t k
      | _, Some v' -> replace t k v'

  let copy t =
    let a = Array.copy (reroot_ t) in
    {t with arr=Arr a}

  let rec buck_iter_ ~f l = match l with
    | Nil -> ()
    | Cons (k,v,l') -> f k v; buck_iter_ ~f l'

  let iter t f =
    let a = reroot_ t in
    Array.iter (buck_iter_ ~f) a

  let rec buck_fold_ f acc l = match l with
    | Nil -> acc
    | Cons (k,v,l') ->
      let acc = f acc k v in
      buck_fold_ f acc l'

  let fold f acc t =
    let a = reroot_ t in
    Array.fold_left (buck_fold_ f) acc a

  let map f t =
    let rec buck_map_ f l = match l with
      | Nil -> Nil
      | Cons (k,v,l') ->
        let v' = f k v in
        Cons (k,v', buck_map_ f l')
    in
    let a = reroot_ t in
    let a' = Array.map (buck_map_ f) a in
    {length=t.length; arr=Arr a'}

  let rec buck_filter_ ~f l = match l with
    | Nil -> Nil
    | Cons (k,v,l') ->
      let l' = buck_filter_ ~f l' in
      if f k v then Cons (k,v,l') else l'

  let buck_length_ b = buck_fold_ (fun n _ _ -> n+1) 0 b

  let filter p t =
    let a = reroot_ t in
    let length = ref 0 in
    let a' = Array.map
        (fun b ->
           let b' = buck_filter_ ~f:p b in
           length := !length + (buck_length_ b');
           b'
        ) a
    in
    {length= !length; arr=Arr a'}

  let rec buck_filter_map_ ~f l = match l with
    | Nil -> Nil
    | Cons (k,v,l') ->
      let l' = buck_filter_map_ ~f l' in
      match f k v with
        | None -> l'
        | Some v' ->
          Cons (k,v',l')

  let filter_map f t =
    let a = reroot_ t in
    let length = ref 0 in
    let a' = Array.map
        (fun b ->
           let b' = buck_filter_map_ ~f b in
           length := !length + (buck_length_ b');
           b'
        ) a
    in
    {length= !length; arr=Arr a'}

  exception ExitPTbl

  let for_all p t =
    try
      iter t (fun k v -> if not (p k v) then raise ExitPTbl);
      true
    with ExitPTbl -> false

  let exists p t =
    try
      iter t (fun k v -> if p k v then raise ExitPTbl);
      false
    with ExitPTbl -> true

  let merge ~f t1 t2 =
    let tbl = create (max (length t1) (length t2)) in
    let tbl = fold
        (fun tbl k v1 ->
           let comb =
             try `Both (v1, find t2 k)
             with Not_found -> `Left v1
           in
           match f k comb with
             | None -> tbl
             | Some v' -> replace tbl k v')
        tbl t1
    in
    fold
      (fun tbl k v2 ->
         if mem t1 k then tbl
         else match f k (`Right v2) with
           | None -> tbl
           | Some v' -> replace tbl k v'
      ) tbl t2

  let add_iter init seq =
    let tbl = ref init in
    seq (fun (k,v) -> tbl := replace !tbl k v);
    !tbl

  let of_iter seq = add_iter (empty ()) seq

  let add_list init l =
    add_iter init (fun k -> List.iter k l)

  let of_list l = add_list (empty ()) l

  let to_list t = fold (fun acc k v -> (k,v)::acc) [] t

  let to_iter t =
    fun k ->
      iter t (fun x y -> k (x,y))

  let equal eq t1 t2 =
    length t1 = length t2
    &&
    for_all
      (fun k v -> match get k t2 with
         | None -> false
         | Some v' -> eq v v'
      ) t1

  let pp ?(pp_start=fun _ () -> ()) ?(pp_stop=fun _ () -> ())
      ?(pp_sep=fun fmt () -> Format.fprintf fmt ",@ ")
      ?(pp_arrow=fun fmt () -> Format.fprintf fmt "@ -> ") pp_k pp_v fmt t =
    let first = ref true in
    pp_start fmt ();
    iter t
      (fun k v ->
         if !first then first:=false
         else pp_sep fmt ();
         pp_k fmt k;
         pp_arrow fmt ();
         pp_v fmt v
      );
    pp_stop fmt ()

  let stats t =
    let a = reroot_ t in
    let max_bucket_length =
      Array.fold_left (fun n b -> max n (buck_length_ b)) 0 a in
    let bucket_histogram = Array.make (max_bucket_length+1) 0 in
    Array.iter
      (fun b ->
         let l = buck_length_ b in
         bucket_histogram.(l) <- bucket_histogram.(l) + 1
      ) a;
    {Hashtbl.
      num_bindings=t.length;
      num_buckets=Array.length a;
      max_bucket_length;
      bucket_histogram;
    }
end

