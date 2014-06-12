
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 LINQ-like operations on collections} *)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a hash = 'a -> int

let _id x = x

type 'a search_result =
  | SearchContinue
  | SearchStop of 'a

module Coll = struct
  type 'a t =
    | Seq : 'a sequence -> 'a t
    | List : 'a list -> 'a t
    | Set : (module CCSequence.Set.S
             with type elt = 'a and type t = 'b) * 'b -> 'a t

  let of_seq s = Seq s
  let of_list l = List l
  let of_array a = Seq (CCSequence.of_array a)

  let set_of_seq (type elt) ?(cmp=Pervasives.compare) seq =
    let module S = CCSequence.Set.Make(struct
      type t = elt
      let compare = cmp
    end) in
    let set = S.of_seq seq in
    Set ((module S), set)

  let to_seq (type elt) = function
    | Seq s -> s
    | List l -> (fun k -> List.iter k l)
    | Set (m, set) ->
        let module S = (val m : CCSequence.Set.S
             with type elt = elt and type t = 'b) in
        S.to_seq set

  let to_list (type elt) = function
    | Seq s -> CCSequence.to_list s
    | List l -> l
    | Set (m, set) ->
        let module S = (val m : CCSequence.Set.S
             with type elt = elt and type t = 'b) in
        S.elements set

  let _fmap ~lst ~seq c = match c with
    | List l -> List (lst l)
    | Seq s -> Seq (seq s)
    | Set _ ->
        List (lst (to_list c))

  let fold (type elt) f acc c = match c with
    | List l -> List.fold_left f acc l
    | Seq s -> CCSequence.fold f acc s
    | Set (m, set) ->
        let module S = (val m : CCSequence.Set.S
             with type elt = elt and type t = 'b) in
        S.fold (fun x acc -> f acc x) set acc

  let map f c =
    _fmap ~lst:(List.map f) ~seq:(CCSequence.map f) c

  let filter p c =
    _fmap ~lst:(List.filter p) ~seq:(CCSequence.filter p) c

  let flat_map f c =
    let c' = to_seq c in
    Seq (CCSequence.flatMap (fun x -> to_seq (f x)) c')

  let filter_map f c =
    _fmap ~lst:(CCList.filter_map f) ~seq:(CCSequence.fmap f) c

  let size (type elt) = function
    | List l -> List.length l
    | Seq s -> CCSequence.length s
    | Set (m, set) ->
        let module S = (val m : CCSequence.Set.S
             with type elt = elt and type t = 'b) in
        S.cardinal set

  let choose (type elt) = function
    | List [] -> None
    | List (x::_) -> Some x
    | Seq s ->
        begin match CCSequence.take 1 s |> CCSequence.to_list with
        | [x] -> Some x
        | _ -> None
        end
    | Set (m, set) ->
        let module S = (val m : CCSequence.Set.S
             with type elt = elt and type t = 'b) in
        try Some (S.choose set) with Not_found -> None

  let take n c =
    _fmap ~lst:(CCList.take n) ~seq:(CCSequence.take n) c

  exception MySurpriseExit

  let _seq_take_while p seq k =
    try
      seq (fun x -> if not (p x) then k x else raise MySurpriseExit)
    with MySurpriseExit -> ()

  let take_while p c =
    to_seq c |> _seq_take_while p |> of_seq

  let distinct ~cmp c = set_of_seq ~cmp (to_seq c)

  let sort cmp c = match c with
    | List l -> List (List.sort cmp l)
    | Seq s -> List (List.sort cmp (CCSequence.to_rev_list s))
    | _ ->
        to_seq c |> set_of_seq ~cmp

  let search obj c =
    let _search_seq obj seq =
      let ret = ref None in
      begin try
        seq (fun x -> match obj#check x with
          | SearchContinue -> ()
          | SearchStop y -> ret := Some y; raise MySurpriseExit);
      with MySurpriseExit -> ()
      end;
      match !ret with
      | None -> obj#failure
      | Some x -> x
    in
    to_seq c |> _search_seq obj

  let contains (type elt) ~eq x c = match c with
    | List l -> List.exists (eq x) l
    | Seq s -> CCSequence.exists (eq x) s
    | Set (m, set) ->
        let module S = (val m : CCSequence.Set.S
             with type elt = elt and type t = 'b) in
        (* XXX: here we don't use the equality relation *)
        try
          let y = S.find x set in
          assert (eq x y);
          true
        with Not_found -> false
end

type 'a collection = 'a Coll.t

module Map = struct
  type ('a, 'b) t = {
    is_empty : unit -> bool;
    size : unit -> int; (** Number of keys *)
    get : 'a -> 'b option;
    fold : 'c. ('c -> 'a -> 'b -> 'c) -> 'c -> 'c;
    to_seq : ('a * 'b) sequence;
  }

  type ('a, 'b) build = {
    mutable cur : ('a, 'b) t;
    add : 'a -> 'b -> unit;
    update : 'a -> ('b option -> 'b option) -> unit;
  }

  (* careful to use this map linearly *)
  let make_hash (type key) ?(eq=(=)) ?(hash=Hashtbl.hash) () =
    let module H = Hashtbl.Make(struct
      type t = key
      let equal = eq
      let hash = hash
    end) in
    (* build table *)
    let tbl = H.create 32 in
    let cur = {
      is_empty = (fun () -> H.length tbl = 0);
      size = (fun () -> H.length tbl);
      get = (fun k ->
        try Some (H.find tbl k)
        with Not_found -> None);
      fold = (fun f acc -> H.fold (fun k v acc -> f acc k v) tbl acc);
      to_seq = (fun k -> H.iter (fun key v -> k (key,v)) tbl);
    } in
    { cur;
      add = (fun k v -> H.replace tbl k v);
      update = (fun k f ->
        match (try f (Some (H.find tbl k)) with Not_found -> f None) with
        | None -> H.remove tbl k
        | Some v' -> H.replace tbl k v');
    }

  let make_cmp (type key) ?(cmp=Pervasives.compare) () =
    let module M = CCSequence.Map.Make(struct
      type t = key
      let compare = cmp
    end) in
    let map = ref M.empty in
    let cur = {
      is_empty = (fun () -> M.is_empty !map);
      size = (fun () -> M.cardinal !map);
      get = (fun k ->
        try Some (M.find k !map)
        with Not_found -> None);
      fold = (fun f acc ->
        M.fold
          (fun key set acc -> f acc key set) !map acc
      );
      to_seq = (fun k -> M.to_seq !map k);
    } in
    {
      cur;
      add = (fun k v -> map := M.add k v !map);
      update = (fun k f ->
        match (try f (Some (M.find k !map)) with Not_found -> f None) with
        | None -> map := M.remove k !map
        | Some v' -> map := M.add k v' !map);
    }

  type 'a key_info = {
    eq : 'a equal option;
    cmp : 'a ord option;
    hash : 'a hash option;
  }

  let default_key_info = {
    eq=None; cmp=None; hash=None;
  }

  let make_info info =
    match info with
    | { hash=None; _}
    | { eq=None; _} ->
        begin match info.cmp with
        | None -> make_cmp ()
        | Some cmp -> make_cmp ~cmp ()
        end
    | {eq=Some eq; hash=Some hash; _} -> make_hash ~eq ~hash ()

  let multiset build seq =
    seq (fun (k,v) ->
      build.update k (function
        | None -> Some [v]
        | Some l -> Some (v::l)));
    { is_empty = build.cur.is_empty;
      size = build.cur.size;
      get = (fun k -> match build.cur.get k with
        | None -> None
        | Some v -> Some (Coll.of_list v));
      fold = (fun f acc ->
        build.cur.fold (fun acc k v -> f acc k (Coll.of_list v)) acc);
      to_seq = build.cur.to_seq
        |> CCSequence.map (fun (k,v) -> k,Coll.of_list v);
    }

  let multimap_cmp ?cmp seq =
    let build = make_cmp ?cmp () in
    multiset build seq

  let count build seq =
    seq (fun x ->
      build.update x
        (function
            | None -> Some 1
            | Some n -> Some (n+1)));
    build.cur

  let get m x = m.get x

  let get_exn m x =
    match m.get x with
    | None -> raise Not_found
    | Some x -> x

  let size m = m.size ()

  let to_seq m = m.to_seq

  let to_list m = m.to_seq |> CCSequence.to_rev_list
end

(** {2 Query operators} *)

type (_,_) safety =
  | Safe : ('a, 'a option) safety
  | Unsafe : ('a, 'a) safety

type (_, _) unary =
  | Map : ('a -> 'b) -> ('a collection, 'b collection) unary
  | GeneralMap : ('a -> 'b) -> ('a, 'b) unary
  | Filter : ('a -> bool) -> ('a collection, 'a collection) unary
  | Fold : ('b -> 'a -> 'b) * 'b -> ('a collection, 'b) unary
  | Reduce : ('c, 'd) safety * ('a -> 'b) * ('a -> 'b -> 'b) * ('b -> 'c)
    -> ('a collection, 'd) unary
  | Size : ('a collection, int) unary
  | Choose : ('a,'b) safety -> ('a collection, 'b) unary
  | FilterMap : ('a -> 'b option) -> ('a collection, 'b collection) unary
  | FlatMap : ('a -> 'b collection) -> ('a collection, 'b collection) unary
  | Take : int -> ('a collection, 'a collection) unary
  | TakeWhile : ('a -> bool) -> ('a collection, 'a collection) unary
  | Sort : 'a ord -> ('a collection, 'a collection) unary
  | Distinct : 'a ord -> ('a collection, 'a collection) unary
  | Search :
    < check: ('a -> 'b search_result);
      failure : 'b;
    > -> ('a collection, 'b) unary
  | Contains : 'a equal * 'a -> ('a collection, bool) unary
  | Get : ('b,'c) safety * 'a -> (('a,'b) Map.t, 'c) unary
  | GroupBy : 'b ord * ('a -> 'b)
    -> ('a collection, ('b,'a collection) Map.t) unary
  | Count : 'a ord -> ('a collection, ('a, int) Map.t) unary

type ('a,'b,'key,'c) join_descr = {
  join_key1 : 'a -> 'key;
  join_key2 : 'b -> 'key;
  join_merge : 'key -> 'a -> 'b -> 'c option;
  join_key : 'key Map.key_info;
}

type ('a,'b) group_join_descr = {
  gjoin_proj : 'b -> 'a;
  gjoin_key : 'a Map.key_info;
}

type set_op =
  | Union
  | Inter
  | Diff

type (_, _, _) binary =
  | Join : ('a, 'b, 'key, 'c) join_descr
    -> ('a collection, 'b collection, 'c collection) binary
  | GroupJoin : ('a, 'b) group_join_descr
    -> ('a collection, 'b collection, ('a, 'b collection) Map.t) binary
  | Product : ('a collection, 'b collection, ('a*'b) collection) binary
  | Append : ('a collection, 'a collection, 'a collection) binary
  | SetOp : set_op * 'a ord -> ('a collection, 'a collection, 'a collection) binary

(* type of queries that return a 'a *)
and 'a t =
  | Start : 'a -> 'a t
  | Unary : ('a, 'b) unary * 'a t -> 'b t
  | Binary : ('a, 'b, 'c) binary * 'a t * 'b t -> 'c t
  | QueryMap : ('a -> 'b) * 'a t -> 'b t
  | Bind : ('a -> 'b t) * 'a t -> 'b t

let start x = Start x

let start_list l =
  Start (Coll.of_list l)

let start_array a =
  Start (Coll.of_array a)

let start_hashtbl h =
  Start (Coll.of_seq (CCSequence.of_hashtbl h))

let start_seq seq =
  Start (Coll.of_seq seq)

(** {6 Execution} *)

let rec _optimize : type a. a t -> a t
  = fun q -> match q with
    | Start _ -> q
    | Unary (u, q) ->
        _optimize_unary u (_optimize q)
    | Binary (b, q1, q2) ->
        _optimize_binary b (_optimize q1) (_optimize q2)
    | QueryMap (f, q) -> QueryMap (f, _optimize q)
    | Bind _ -> q  (* cannot optimize before execution *)
and _optimize_unary : type a b. (a,b) unary -> a t -> b t
  = fun u q -> match u, q with
    | Map f, Unary (Map g, q') ->
        _optimize_unary (Map (fun x -> f (g x))) q'
    | Filter p, Unary (Map f, cont) ->
        _optimize_unary
          (FilterMap (fun x -> let y = f x in if p y then Some y else None))
          cont
    | Map f, Unary (Filter p, cont) ->
        _optimize_unary
          (FilterMap (fun x -> if p x then Some (f x) else None))
          cont
    | Map f, Binary (Append, q1, q2) ->
        _optimize_binary Append (Unary (u, q1)) (Unary (u, q2))
    | Filter p, Binary (Append, q1, q2) ->
        _optimize_binary Append (Unary (u, q1)) (Unary (u, q2))
    | Fold (f,acc), Unary (Map f', cont) ->
        _optimize_unary
          (Fold ((fun acc x -> f acc (f' x)), acc))
          cont
    | Reduce (safety, start, mix, stop), Unary (Map f, cont) ->
        _optimize_unary
          (Reduce (safety,
            (fun x -> start (f x)),
            (fun x acc -> mix (f x) acc),
            stop))
          cont
    | Size, Unary (Map _, cont) ->
        _optimize_unary Size cont  (* ignore the map! *)
    | Size, Unary (Sort _, cont) ->
        _optimize_unary Size cont
    | _ -> Unary (u,q)
    (* TODO: other cases *)
and _optimize_binary : type a b c. (a,b,c) binary -> a t -> b t -> c t
  = fun b q1 q2 -> match b, q1, q2 with
    | _ -> Binary (b, q1, q2)  (* TODO *)

(* apply a unary operator on a collection *)
let _do_unary : type a b. (a,b) unary -> a -> b
= fun u c -> match u with
  | Map f -> Coll.map f c
  | GeneralMap f -> f c
  | Filter p -> Coll.filter p c
  | Fold (f, acc) -> Coll.fold f acc c
  | Reduce (safety, start, mix, stop) ->
      let acc = Coll.to_seq c
        |> CCSequence.fold
          (fun acc x -> match acc with
            | None -> Some (start x)
            | Some acc -> Some (mix x acc)
          ) None
      in
      begin match acc, safety with
      | Some x, Safe -> Some (stop x)
      | None, Safe -> None
      | Some x, Unsafe -> stop x
      | None, Unsafe -> invalid_arg "reduce: empty collection"
      end
  | Size -> Coll.size c
  | Choose Safe -> Coll.choose c
  | Choose Unsafe ->
      begin match Coll.choose c with
        | Some x -> x
        | None -> invalid_arg "choose: empty collection"
      end
  | FilterMap f -> Coll.filter_map f c
  | FlatMap f -> Coll.flat_map f c
  | Take n -> Coll.take n c
  | TakeWhile p -> Coll.take_while p c
  | Sort cmp -> Coll.sort cmp c
  | Distinct cmp -> Coll.distinct ~cmp c
  | Search obj -> Coll.search obj c
  | Get (Safe, k) -> Map.get c k
  | Get (Unsafe, k) -> Map.get_exn c k
  | GroupBy (cmp,f) ->
      Coll.to_seq c
      |> CCSequence.map (fun x -> f x, x)
      |> Map.multimap_cmp ~cmp
  | Contains (eq, x) -> Coll.contains ~eq x c
  | Count cmp ->
      Coll.to_seq c
      |> Map.count (Map.make_cmp ~cmp ())


(* TODO: join of two collections *)
let _do_join ~join c1 c2 =
  let _build = Map.make_info join.join_key in
  assert false

(* TODO *)
let _do_group_join ~gjoin c1 c2 =
  assert false

let _do_product c1 c2 =
  let s1 = Coll.to_seq c1 and s2 = Coll.to_seq c2 in
  Coll.of_seq (CCSequence.product s1 s2)

let _do_binary : type a b c. (a, b, c) binary -> a -> b -> c
= fun b c1 c2 -> match b with
  | Join join -> _do_join ~join c1 c2
  | GroupJoin gjoin -> _do_group_join ~gjoin c1 c2
  | Product -> _do_product c1 c2
  | Append ->
      Coll.of_seq (CCSequence.append (Coll.to_seq c1) (Coll.to_seq c2))
  | SetOp (Inter,cmp) ->
      (* use a join *)
      _do_join ~join:{
        join_key1=_id;
        join_key2=_id;
        join_merge=(fun _ a b -> Some a);
        join_key=Map.({default_key_info with cmp=Some cmp; })
      } c1 c2
  | SetOp (Union,cmp) -> failwith "union: not implemented"  (* TODO *)
  | SetOp (Diff,cmp) -> failwith "diff: not implemented"  (* TODO *)

let rec _run : type a. opt:bool -> a t -> a
  = fun ~opt q -> match q with
  | Start c -> c
  | Unary (u, q') -> _do_unary u (_run ~opt q')
  | Binary (b, q1, q2) -> _do_binary b (_run ~opt q1) (_run ~opt q2)
  | QueryMap (f, q') -> f (_run ~opt q')
  | Bind (f, q') ->
      let x = _run ~opt q' in
      let q'' = f x in
      let q'' = if opt then _optimize q'' else q'' in
      _run ~opt q''

let run q = _run ~opt:true (_optimize q)
let run_no_opt q = _run ~opt:false q

(** {6 Basics on Collections} *)

let map f q = Unary (Map f, q)

let filter p q = Unary (Filter p, q)

let choose q = Unary (Choose Safe, q)

let choose_exn q = Unary (Choose Unsafe, q)

let filter_map f q = Unary (FilterMap f, q)

let flat_map f q = Unary (FlatMap f, q)

let flat_map_seq f q =
  let f' x = Coll.of_seq (f x) in
  Unary (FlatMap f', q)

let flat_map_list f q =
  let f' x = Coll.of_list (f x) in
  Unary (FlatMap f', q)

let take n q = Unary (Take n, q)

let take_while p q = Unary (TakeWhile p, q)

let sort ?(cmp=Pervasives.compare) () q = Unary (Sort cmp, q)

let distinct ?(cmp=Pervasives.compare) () q =
  Unary (Distinct cmp, q)

let get key q =
  Unary (Get (Safe, key), q)

let get_exn key q =
  Unary (Get (Unsafe, key), q)

let map_iter q =
  Unary (GeneralMap (fun m -> Coll.of_seq m.Map.to_seq), q)

let map_iter_flatten q =
  let f m = m.Map.to_seq
      |> CCSequence.flatMap
        (fun (k,v) -> Coll.to_seq v |> CCSequence.map (fun v' -> k,v'))
      |> Coll.of_seq
  in
  Unary (GeneralMap f, q)

let map_to_list q =
  Unary (GeneralMap Map.to_list, q)

let group_by ?(cmp=Pervasives.compare) f q =
  Unary (GroupBy (cmp,f), q)

let count ?(cmp=Pervasives.compare) () q =
  Unary (Count cmp, q)

let fold f acc q =
  Unary (Fold (f, acc), q)

let size q = Unary (Size, q)

let sum q = Unary (Fold ((+), 0), q)

let reduce start mix stop q =
  Unary (Reduce (Safe, start,mix,stop), q)

let reduce_exn start mix stop q =
  Unary (Reduce (Unsafe, start,mix,stop), q)

let _avg_start x = (x,1)
let _avg_mix x (y,n) = (x+y,n+1)
let _avg_stop (x,n) = x/n

let _lift_some f x y = match y with
  | None -> Some x
  | Some y -> Some (f x y)

let max q = Unary (Reduce (Safe, _id, Pervasives.max, _id), q)
let min q = Unary (Reduce (Safe, _id, Pervasives.min, _id), q)
let average q = Unary (Reduce (Safe, _avg_start, _avg_mix, _avg_stop), q)

let max_exn q = Unary (Reduce (Unsafe, _id, Pervasives.max, _id), q)
let min_exn q = Unary (Reduce (Unsafe, _id, Pervasives.min, _id), q)
let average_exn q = Unary (Reduce (Unsafe, _avg_start, _avg_mix, _avg_stop), q)

let is_empty q =
  Unary (Search (object
    method check _ = SearchStop false (* stop in case there is an element *)
    method failure = true
  end), q)

let contains ?(eq=(=)) x q =
  Unary (Contains (eq, x), q)

let for_all p q =
  Unary (Search (object
    method check x = if p x then SearchContinue else SearchStop false
    method failure = true
  end), q)

let exists p q =
  Unary (Search (object
    method check x = if p x then SearchStop true else SearchContinue
    method failure = false
  end), q)

let find p q =
  Unary (Search (object
    method check x = if p x then SearchStop (Some x) else SearchContinue
    method failure = None
  end), q)

let find_map f q =
  Unary (Search (object
    method check x = match f x with
      | Some y -> SearchStop (Some y)
      | None -> SearchContinue
    method failure = None
  end), q)

(** {6 Binary Operators} *)

let join ?cmp ?eq ?hash join_key1 join_key2 ~merge q1 q2 =
  let j = {
    join_key1;
    join_key2;
    join_merge=merge;
    join_key = Map.({ eq; cmp; hash; });
  } in
  Binary (Join j, q1, q2)

let group_join ?cmp ?eq ?hash gjoin_proj q1 q2 =
  let j = {
    gjoin_proj;
    gjoin_key = Map.({ eq; cmp; hash; });
  } in
  Binary (GroupJoin j, q1, q2)

let product q1 q2 = Binary (Product, q1, q2)

let append q1 q2 = Binary (Append, q1, q2)

let inter ?(cmp=Pervasives.compare) () q1 q2 =
  Binary (SetOp (Inter, cmp), q1, q2)

let union ?(cmp=Pervasives.compare) () q1 q2 =
  Binary (SetOp (Union, cmp), q1, q2)

let diff ?(cmp=Pervasives.compare) () q1 q2 =
  Binary (SetOp (Diff, cmp), q1, q2)

let fst q = map fst q
let snd q = map snd q

let flatten_opt q = filter_map _id q

let opt_get_exn q =
  QueryMap ((function
    | Some x -> x
    | None -> invalid_arg "opt_get_exn"), q)

(** {6 Monadic stuff} *)

let return x = Start x

let bind f q = Bind (f,q)

let (>>=) x f = Bind (f, x)

let query_map f q = QueryMap (f, q)

(** {6 Output containers} *)

let to_list q =
  QueryMap (Coll.to_list, q)

let to_array q =
  QueryMap ((fun c -> Array.of_list (Coll.to_list c)), q)

let to_seq q =
  QueryMap ((fun c -> Coll.to_seq c |> CCSequence.persistent), q)

let to_hashtbl q =
  QueryMap ((fun c -> CCSequence.to_hashtbl (Coll.to_seq c)), q)

let to_queue q =
  QueryMap ((fun c q -> CCSequence.to_queue q (Coll.to_seq c)), q)

let to_stack q =
  QueryMap ((fun c s -> CCSequence.to_stack s (Coll.to_seq c)), q)

(** {6 Misc} *)

let run_list q = run (q |> to_list)
