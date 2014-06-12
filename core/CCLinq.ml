
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
type 'a klist = unit -> [ `Nil | `Cons of 'a * 'a klist ]

let _id x = x

module Coll = struct
  type 'a t =
    | Seq of 'a sequence
    | List of 'a list

  let of_seq s = Seq s
  let of_list l = List l
  let of_array a = Seq (CCSequence.of_array a)
  let empty = List []

  let to_seq = function
    | Seq s -> s
    | List l -> (fun k -> List.iter k l)

  let to_list = function
    | Seq s -> CCSequence.to_list s
    | List l -> l

  let _fmap ~lst ~seq c = match c with
    | List l -> List (lst l)
    | Seq s -> Seq (seq s)

  let _fold ~lst ~seq acc c = match c with
    | List l -> List.fold_left lst acc l
    | Seq s -> CCSequence.fold seq acc s

  let iter f c = match c with
    | List l -> List.iter f l
    | Seq s -> s f

  let map f c =
    _fmap ~lst:(List.map f) ~seq:(CCSequence.map f) c

  let filter p c =
    _fmap ~lst:(List.filter p) ~seq:(CCSequence.filter p) c

  let flat_map f c =
    let c' = to_seq c in
    Seq (CCSequence.flatMap f c')

  let filter_map f c =
    _fmap ~lst:(CCList.filter_map f) ~seq:(CCSequence.fmap f) c

  let size = function
    | List l -> List.length l
    | Seq s -> CCSequence.length s

  let fold f acc c = _fold ~lst:f ~seq:f acc c
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

  let make_hash (type key) ?(eq=(=)) ?(hash=Hashtbl.hash) seq =
    let module H = Hashtbl.Make(struct
      type t = key
      let equal = eq
      let hash = hash
    end) in
    (* build table *)
    let tbl = H.create 32 in
    seq
      (fun (k,v) ->
        let l = try H.find tbl k with Not_found -> [] in
        H.replace tbl k (v::l)
      );
    (* provide the multimap interface *)
    let to_seq cont = H.iter (fun k v -> cont (k, Coll.of_list v)) tbl
    in
    {
      is_empty = (fun () -> H.length tbl = 0);
      size = (fun () -> H.length tbl);
      get = (fun k ->
        try Some (Coll.of_list (H.find tbl k))
        with Not_found -> None);
      fold = (fun f acc -> H.fold (fun k v acc -> f acc k (Coll.of_list v)) tbl acc);
      to_seq;
    }

  let make_map (type key) (type value)
  ?(cmp_key=Pervasives.compare) ?(cmp_val=Pervasives.compare) seq =
    let module M = CCSequence.Map.Make(struct
      type t = key
      let compare = cmp_key
    end) in
    let module S = CCSequence.Set.Make(struct
      type t = value
      let compare = cmp_val
    end) in
    let _map_set set = Coll.of_seq (S.to_seq set) in
    let map = CCSequence.fold
      (fun map (k,v) ->
        let set = try M.find k map with Not_found -> S.empty in
        M.add k (S.add v set) map
      ) M.empty seq
    in
    let to_seq = 
      M.to_seq map |> CCSequence.map (fun (k,v) -> k, _map_set v)
    in
    {
      is_empty = (fun () -> M.is_empty map);
      size = (fun () -> M.cardinal map);
      get = (fun k ->
        try Some (_map_set (M.find k map))
        with Not_found -> None);
      fold = (fun f acc ->
        M.fold
          (fun key set acc -> f acc key (_map_set set)) map acc
      );
      to_seq;
    }

  let get m x = m.get x

  let get_exn m x =
    match m.get x with
    | None -> raise Not_found
    | Some x -> x

  let size m = m.size ()

  let to_seq m = m.to_seq

  type 'a key_info = {
    eq : 'a equal option;
    cmp : 'a ord option;
    hash : 'a hash option;
  }
end

(** {2 Query operators} *)

type safe = Safe
type unsafe = Unsafe
type (_,_) safety =
  | Safe : ('a, 'a option) safety
  | Unsafe : ('a, 'a) safety

type 'a search_result =
  | SearchContinue
  | SearchStop of 'a

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
  | Distinct : 'a ord option * 'a equal option * 'a hash option
    -> ('a collection, 'a collection) unary
  | Search :
    < check: ('a -> 'b search_result);
      failure : 'b;
    > -> ('a collection, 'b) unary
  | Get : ('b,'c) safety * 'a -> (('a,'b) Map.t, 'c) unary
  | GroupBy : 'b ord * 'a ord * ('a -> 'b)
    -> ('a collection, ('b,'a collection) Map.t) unary
  | Count : 'a ord -> ('a collection, ('a, int) Map.t) unary

type ('a,'b,'key,'c) join_descr = {
  join_key1 : 'a -> 'key;
  join_key2 : 'b -> 'key;
  join_merge : 'key -> 'a -> 'b -> 'c;
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
  | Inter : 'a ord -> ('a collection, 'a collection, 'a collection) binary

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

(** {6 Composition} *)

let apply u q = Unary (u, q)

let (>>>) = apply

(** {6 Execution} *)

let rec _optimize : type a. a t -> a t
  = fun q -> match q with
    | Start _ -> q
    | Unary (u, q) ->
        _optimize_unary u (_optimize q)
    | Binary (b, q1, q2) ->
        _optimize_binary b (_optimize q1) (_optimize q2)
    | QueryMap (f, q) -> QueryMap (f, _optimize q)
and _optimize_unary : type a b. (a,b) unary -> a t -> b t
  = fun u q -> match u, q with
    | Map f, Unary (Map g, q') ->
        _optimize_unary (Map (fun x -> f (g x))) q'
    | _ -> Unary (u,q)
    (* TODO *)
and _optimize_binary : type a b c. (a,b,c) binary -> a t -> b t -> c t
  = fun b q1 q2 -> match b, q1, q2 with
  | _ -> Binary (b, q1, q2)  (* TODO *)


(* apply a unary operator on a collection *)
let _do_unary : type a b. (a,b) unary -> a -> b
= fun u c -> match u with
  | Map f -> Coll.map f c
  | Filter p -> Coll.filter p c
  | Fold (f, acc) -> Coll.fold f acc c  (* TODO: optimize *)

(* TODO: join of two collections *)
let _do_join ~join c1 c2 =
  assert false

let _do_product c1 c2 =
  let s1 = Coll.to_seq c1 and s2 = Coll.to_seq c2 in
  Coll.of_seq (CCSequence.product s1 s2)

let _do_binary : type a b c. (a, b, c) binary -> a -> b -> c
= fun b c1 c2 -> match b with
  | Join join -> _do_join ~join c1 c2
  | Product -> _do_product c1 c2

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

let size q = Unary (Size, q)

let choose q = Unary (Choose Safe, q)

let choose_exn q = Unary (Choose Unsafe, q)

let filter_map f q = Unary (FilterMap f, q)

let flat_map f q = Unary (FlatMap f, q)

let flat_map_seq f q =
  let f' x = Coll.of_seq (f x) in
  Unary (FlatMap f', q)

let take n q = Unary (Take n, q)

let take_while p q = Unary (TakeWhile p, q)

let sort ~cmp q = Unary (Sort cmp, q)

let distinct ?cmp ?eq ?hash () q =
  Unary (Distinct (cmp,eq,hash), q)

let get key q =
  Unary (Get (Safe, key), q)

let get_exn key q =
  Unary (Get (Unsafe, key), q)

let map_to_seq q =
  Unary (GeneralMap (fun m -> Coll.of_seq m.Map.to_seq), q)

let map_to_seq_flatten q =
  let f m = m.Map.to_seq
      |> CCSequence.flatMap
        (fun (k,v) -> Coll.to_seq v |> CCSequence.map (fun v' -> k,v'))
      |> Coll.of_seq
  in
  Unary (GeneralMap f, q)

let group_by ?(cmp_key=Pervasives.compare) ?(cmp_val=Pervasives.compare) f q =
  Unary (GroupBy (cmp_key,cmp_val,f), q)

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
