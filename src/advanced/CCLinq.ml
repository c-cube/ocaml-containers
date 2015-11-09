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
type 'a with_err = [`Ok of 'a | `Error of string ]

let _id x = x

exception ExitWithError of string
let _exit_with_error s = raise (ExitWithError s)
let _error_of_exn f = try `Ok (f ()) with ExitWithError s -> `Error s

module PMap = struct
  type ('a, 'b) t = {
    is_empty : unit -> bool;
    size : unit -> int; (* Number of keys *)
    get : 'a -> 'b option;
    fold : 'c. ('c -> 'a -> 'b -> 'c) -> 'c -> 'c;
    to_seq : ('a * 'b) sequence;
  }

  let get m x = m.get x
  let mem m x = match m.get x with
    | None -> false
    | Some _ -> true
  let to_seq m = m.to_seq
  let fold f acc m = m.fold f acc
  let size m = m.size ()

  type ('a, 'b) build = {
    mutable cur : ('a, 'b) t;
    add : 'a -> 'b -> unit;
    update : 'a -> ('b option -> 'b option) -> unit;
  }

  let build_get b = b.cur
  let add b x y = b.add x y
  let update b f = b.update f

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
    let module M = Sequence.Map.Make(struct
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

  type 'a build_method =
    | FromCmp of 'a ord
    | FromHash of 'a equal * 'a hash
    | Default

  let make ?(build=Default) () = match build with
    | Default -> make_hash ()
    | FromCmp cmp -> make_cmp ~cmp ()
    | FromHash (eq,hash) -> make_hash ~eq ~hash ()

  (* choose a build method from the optional arguments *)
  let _make_build ?cmp ?eq ?hash () =
    let _maybe default o = match o with
      | Some x -> x
      | None -> default
    in
    match eq, hash with
    | Some _, _
    | _, Some _ ->
        FromHash ( _maybe (=) eq, _maybe Hashtbl.hash hash)
    | _ ->
        match cmp with
        | Some f -> FromCmp f
        | _ -> Default

  let multimap_of_seq ?(build=make ()) seq =
    seq (fun (k,v) ->
      build.update k (function
        | None -> Some [v]
        | Some l -> Some (v::l)));
    build.cur

  let count_of_seq ?(build=make ()) seq =
    seq (fun x ->
      build.update x
        (function
            | None -> Some 1
            | Some n -> Some (n+1)));
    build.cur

  (* map values *)
  let map f m = {
    is_empty = m.is_empty;
    size = m.size;
    get = (fun k -> match m.get k with
      | None -> None
      | Some v -> Some (f v)
    );
    to_seq = Sequence.map (fun (x,y) -> x, f y) m.to_seq;
    fold = (fun f' acc ->
      m.fold (fun acc x y -> f' acc x (f y)) acc
    );
  }

  let to_list m = Sequence.to_rev_list m.to_seq

  let reverse_ ~build m =
    let build = make ~build () in
    let seq = Sequence.map (fun (x,y) -> y,x) (to_seq m) in
    multimap_of_seq ~build seq

  let reverse_multimap_ ~build m =
    let build = make ~build () in
    let seq = to_seq m in
    let seq = Sequence.flat_map
        (fun (x,l) -> Sequence.map (fun y -> y,x) (Sequence.of_list l)
        ) seq
    in
    multimap_of_seq ~build seq

  let reverse ?cmp ?eq ?hash () m =
    let build = _make_build ?cmp ?eq ?hash () in
    reverse_ ~build m

  let reverse_multimap  ?cmp ?eq ?hash () m =
    let build = _make_build ?cmp ?eq ?hash () in
    reverse_multimap_ ~build m

  let fold_multimap f acc m =
    m.fold (fun acc x l -> List.fold_left (fun acc y -> f acc x y) acc l) acc

  let get_seq key m = match get m key with
    | None -> Sequence.empty
    | Some x -> Sequence.return x

  let iter m = m.to_seq

  let flatten m =
    let seq = Sequence.flat_map
      (fun (k,v) -> Sequence.map (fun v' -> k,v') v)
       m.to_seq
    in
    seq

  let flatten_l m =
    let seq = Sequence.flatMap
        (fun (k,v) -> Sequence.map (fun v' -> k,v') (Sequence.of_list v))
        m.to_seq
    in
    seq
end

type 'a search_result =
  | SearchContinue
  | SearchStop of 'a

type ('a,'b,'key,'c) join_descr = {
  join_key1 : 'a -> 'key;
  join_key2 : 'b -> 'key;
  join_merge : 'key -> 'a -> 'b -> 'c option;
  join_build : 'key PMap.build_method;
}

type ('a,'b) group_join_descr = {
  gjoin_proj : 'b -> 'a;
  gjoin_build : 'a PMap.build_method;
}

module ImplemSetOps = struct
  let choose s = Sequence.take 1 s

  let distinct ~cmp s = Sequence.sort_uniq ~cmp s

  let search obj s =
    match
      Sequence.find
        (fun x -> match obj#check x with
          | SearchContinue -> None
          | SearchStop y -> Some y
        ) s
    with None -> obj#failure
       | Some x -> x

  let do_join ~join c1 c2 =
    let build1 =
      let seq = Sequence.map (fun x -> join.join_key1 x, x) c1 in
      PMap.multimap_of_seq ~build:(PMap.make ~build:join.join_build ()) seq
    in
    let l = Sequence.fold
      (fun acc y ->
        let key = join.join_key2 y in
        match PMap.get build1 key with
        | None -> acc
        | Some l1 ->
            List.fold_left
              (fun acc x -> match join.join_merge key x y with
                | None -> acc
                | Some res -> res::acc
              ) acc l1
      ) [] c2
    in
    Sequence.of_list l

  let do_group_join ~gjoin c1 c2 =
    let build = PMap.make ~build:gjoin.gjoin_build () in
    c1 (fun x -> PMap.add build x []);
    c2
      (fun y ->
        (* project [y] into some element of [c1] *)
        let x = gjoin.gjoin_proj y in
        PMap.update build x
          (function
            | None -> None   (* [x] not present, ignore! *)
            | Some l -> Some (y::l)
          )
      );
    PMap.build_get build

  let do_union ~build c1 c2 =
    let build = PMap.make ~build () in
    c1 (fun x -> PMap.add build x ());
    c2 (fun x -> PMap.add build x ());
    let seq = PMap.to_seq (PMap.build_get build) in
    Sequence.map fst seq

  type inter_status =
    | InterLeft
    | InterDone  (* already output *)

  let do_inter ~build c1 c2 =
    let build = PMap.make ~build () in
    let l = ref [] in
    c1 (fun x -> PMap.add build x InterLeft);
    c2 (fun x ->
      PMap.update build x
        (function
           | None -> Some InterDone
           | Some InterDone as foo -> foo
           | Some InterLeft ->
               l := x :: !l;
               Some InterDone
        )
    );
    Sequence.of_list !l

  let do_diff ~build c1 c2 =
    let build = PMap.make ~build () in
    c2 (fun x -> PMap.add build x ());
    let map = PMap.build_get build in
    (* output elements of [c1] not in [map] *)
    Sequence.filter (fun x -> not (PMap.mem map x)) c1
end

(** {2 Query operators} *)

type (_, _) unary =
  | Map : ('a -> 'b) -> ('a, 'b ) unary
  | Filter : ('a -> bool) -> ('a, 'a ) unary
  | Fold : ('b -> 'a -> 'b) * 'b -> ('a, 'b) unary
  | Reduce : ('a -> 'b) * ('a -> 'b -> 'b) * ('b -> 'c)
    -> ('a, 'c) unary
  | Size : ('a, int) unary
  | Choose : ('a, 'a) unary
  | FilterMap : ('a -> 'b option) -> ('a, 'b) unary
  | FlatMap : ('a -> 'b sequence) -> ('a, 'b) unary
  | Take : int -> ('a, 'a) unary
  | TakeWhile : ('a -> bool) -> ('a, 'a) unary
  | Sort : 'a ord -> ('a, 'a) unary
  | Distinct : 'a ord -> ('a, 'a) unary
  | Search :
    < check: ('a -> 'b search_result);
      failure : 'b;
    > -> ('a, 'b) unary
  | Contains : 'a equal * 'a -> ('a, bool) unary
  | GroupBy : 'b PMap.build_method * ('a -> 'b)
    -> ('a, ('b,'a list) PMap.t) unary
  | Count : 'a PMap.build_method -> ('a, ('a, int) PMap.t) unary
  | Lazy : ('a lazy_t, 'a) unary

type set_op =
  | Union
  | Inter
  | Diff

type (_, _, _) binary =
  | App : ('a -> 'b, 'a, 'b) binary
  | Join : ('a, 'b, 'key, 'c) join_descr
    -> ('a, 'b, 'c) binary
  | GroupJoin : ('a, 'b) group_join_descr
    -> ('a, 'b, ('a, 'b list) PMap.t) binary
  | Product : ('a, 'b, ('a*'b)) binary
  | Append : ('a, 'a, 'a) binary
  | SetOp : set_op * 'a PMap.build_method
    -> ('a, 'a, 'a) binary

(* type of queries that return a 'a *)
and 'a t =
  | Return : 'a -> 'a t
  | OfSeq : 'a sequence -> 'a t
  | Unary : ('a, 'b) unary * 'a t -> 'b t
  | Binary : ('a, 'b, 'c) binary * 'a t * 'b t -> 'c t
  | Bind : ('a -> 'b t) * 'a t -> 'b t
  | Reflect : 'a t -> 'a sequence t

let start x = Return x

let of_list l =
  OfSeq (Sequence.of_list l)

let of_array a =
  OfSeq (Sequence.of_array a)

let of_array_i a =
  OfSeq (Sequence.of_array_i a)

let of_hashtbl h =
  OfSeq (Sequence.of_hashtbl h)

let range i j = OfSeq (Sequence.int_range ~start:i ~stop:j)

let (--) = range

let of_seq seq =
  OfSeq seq

let of_queue q =
  OfSeq (Sequence.of_queue q)

let of_stack s =
  OfSeq (Sequence.of_stack s)

let of_string s =
  OfSeq (Sequence.of_str s)

(** {6 Execution} *)

let rec _optimize : type a. a t -> a t
  = fun q -> match q with
    | Return _ -> q
    | Unary (u, q) ->
        _optimize_unary u (_optimize q)
    | Binary (b, q1, q2) ->
        _optimize_binary b (_optimize q1) (_optimize q2)
    | Reflect q -> Reflect (_optimize q)
    | OfSeq _ -> q
    | Bind (f,q) -> Bind(f, _optimize q)  (* cannot optimize [f] before execution *)
and _optimize_unary : type a b. (a,b) unary -> a t -> b t
  = fun u q -> match u, q with
    | Size, Unary (Choose, _) -> Return 1
    | Map f, Unary (Map g, q') ->
        _optimize_unary (Map (fun x -> f (g x))) q'
    | Filter p, Unary (Map f, cont) ->
        _optimize_unary
          (FilterMap (fun x -> let y = f x in if p y then Some y else None))
          cont
    | Filter p, Unary (Filter p', q) ->
        _optimize_unary (Filter (fun x -> p x && p' x)) q
    | FilterMap f, Unary (Map g, q') ->
        _optimize_unary (FilterMap (fun x -> f (g x))) q'
    | Map f, Unary (Filter p, cont) ->
        _optimize_unary
          (FilterMap (fun x -> if p x then Some (f x) else None))
          cont
    | Map _, Binary (Append, q1, q2) ->
        _optimize_binary Append (Unary (u, q1)) (Unary (u, q2))
    | Filter _, Binary (Append, q1, q2) ->
        _optimize_binary Append (Unary (u, q1)) (Unary (u, q2))
    | Fold (f,acc), Unary (Map f', cont) ->
        _optimize_unary
          (Fold ((fun acc x -> f acc (f' x)), acc))
          cont
    | Reduce (start, mix, stop), Unary (Map f, cont) ->
        _optimize_unary
          (Reduce (
            (fun x -> start (f x)),
            (fun x acc -> mix (f x) acc),
            stop))
          cont
    | Size, Unary (Map _, cont) ->
        _optimize_unary Size cont  (* ignore the map! *)
    | Size, Unary (Sort _, cont) ->
        _optimize_unary Size cont
    | _ -> Unary (u, _optimize q)
    (* TODO: other cases *)
and _optimize_binary : type a b c. (a,b,c) binary -> a t -> b t -> c t
  = fun b q1 q2 -> match b, q1, q2 with
    | App, Return f, Return x -> Return (f x)
    | App, Return f, x -> _optimize_unary (Map f) x
    | App, f, Return x -> _optimize_unary (Map (fun f -> f x)) f
    | App, _, _ -> Binary (b, _optimize q1, _optimize q2)
    | Join _, _, _ -> Binary (b, _optimize q1, _optimize q2)
    | GroupJoin _, _, _ -> Binary (b, _optimize q1, _optimize q2)
    | Product, _, _ -> Binary (b, _optimize q1, _optimize q2)
    | Append, _, _ -> Binary (b, _optimize q1, _optimize q2)
    | SetOp _, _, _ -> Binary (b, _optimize q1, _optimize q2)

(* apply a unary operator on a collection *)
let _do_unary : type a b. (a,b) unary -> a sequence -> b sequence
= fun u c -> match u with
  | Map f -> Sequence.map f c
  | Filter p -> Sequence.filter p c
  | Fold (f, acc) -> Sequence.return (Sequence.fold f acc c)
  | Reduce (start, mix, stop) ->
      let acc = Sequence.fold
        (fun acc x -> match acc with
          | None -> Some (start x)
          | Some acc -> Some (mix x acc)
        ) None c
      in
      begin match acc with
        | None -> Sequence.empty
        | Some x -> Sequence.return (stop x)
      end
  | Size -> Sequence.return (Sequence.length c)
  | Choose -> ImplemSetOps.choose c
  | FilterMap f -> Sequence.filter_map f c
  | FlatMap f -> Sequence.flat_map f c
  | Take n -> Sequence.take n c
  | TakeWhile p -> Sequence.take_while p c
  | Sort cmp -> Sequence.sort ~cmp c
  | Distinct cmp -> ImplemSetOps.distinct ~cmp c
  | Search obj -> Sequence.return (ImplemSetOps.search obj c)
  | GroupBy (build,f) ->
      let seq = Sequence.map (fun x -> f x, x) c in
      Sequence.return (PMap.multimap_of_seq ~build:(PMap.make ~build ()) seq)
  | Contains (eq, x) -> Sequence.return (Sequence.mem ~eq x c)
  | Count build ->
      Sequence.return (PMap.count_of_seq ~build:(PMap.make ~build ()) c)
  | Lazy -> Sequence.map Lazy.force c

let _do_binary : type a b c. (a, b, c) binary -> a sequence -> b sequence -> c sequence
= fun b c1 c2 -> match b with
  | Join join -> ImplemSetOps.do_join ~join c1 c2
  | GroupJoin gjoin -> Sequence.return (ImplemSetOps.do_group_join ~gjoin c1 c2)
  | Product -> Sequence.product c1 c2
  | Append -> Sequence.append c1 c2
  | App -> Sequence.(c1 <*> c2)
  | SetOp (Inter,build) -> ImplemSetOps.do_inter ~build c1 c2
  | SetOp (Union,build) -> ImplemSetOps.do_union ~build c1 c2
  | SetOp (Diff,build) -> ImplemSetOps.do_diff ~build c1 c2

let rec _run : type a. opt:bool -> a t -> a sequence
= fun ~opt q -> match q with
  | Return c -> Sequence.return c
  | Unary (u, q') -> _do_unary u (_run ~opt q')
  | Binary (b, q1, q2) -> _do_binary b (_run ~opt q1) (_run ~opt q2)
  | OfSeq s -> s
  | Bind (f, q') ->
      let seq = _run ~opt q' in
      Sequence.flat_map
        (fun x ->
          let q'' = f x in
          let q'' = if opt then _optimize q'' else q'' in
          _run ~opt q''
        ) seq
  | Reflect q ->
    let seq = Sequence.persistent_lazy (_run ~opt q) in
    Sequence.return seq

let _apply_limit ?limit seq = match limit with
    | None -> seq
    | Some l -> Sequence.take l seq

(* safe execution *)
let run ?limit q =
  let seq = _run ~opt:true (_optimize q) in
  _apply_limit ?limit seq

let run_no_optim ?limit q =
  let seq = _run ~opt:false q in
  _apply_limit ?limit seq

let run1 q =
  let seq = _run ~opt:true (_optimize q) in
  match Sequence.head seq with
  | Some x -> x
  | None -> raise Not_found

(** {6 Basics} *)

let empty = OfSeq Sequence.empty

let map f q = Unary (Map f, q)

let (>|=) q f = Unary (Map f, q)

let filter p q = Unary (Filter p, q)

let choose q = Unary (Choose, q)

let filter_map f q = Unary (FilterMap f, q)

let flat_map f q = Unary (FlatMap f, q)

let flat_map_l f q =
  let f' x = Sequence.of_list (f x) in
  Unary (FlatMap f', q)

let flatten_seq q = Unary (FlatMap (fun x->x), q)

let flatten q = Unary (FlatMap Sequence.of_list, q)

let take n q = Unary (Take n, q)

let take_while p q = Unary (TakeWhile p, q)

let sort ?(cmp=Pervasives.compare) () q = Unary (Sort cmp, q)

let distinct ?(cmp=Pervasives.compare) () q =
  Unary (Distinct cmp, q)

let group_by ?cmp ?eq ?hash f q =
  Unary (GroupBy (PMap._make_build ?cmp ?eq ?hash (),f), q)

let group_by' ?cmp ?eq ?hash f q =
  flat_map PMap.iter (group_by ?cmp ?eq ?hash f q)

let count ?cmp ?eq ?hash () q =
  Unary (Count (PMap._make_build ?cmp ?eq ?hash ()), q)

let count' ?cmp () q =
  flat_map PMap.iter (count ?cmp () q)

let fold f acc q =
  Unary (Fold (f, acc), q)

let size q = Unary (Size, q)

let sum q = Unary (Fold ((+), 0), q)

let reduce start mix stop q =
  Unary (Reduce (start,mix,stop), q)

let _avg_start x = (x,1)
let _avg_mix x (y,n) = (x+y,n+1)
let _avg_stop (x,n) = x/n

let _lift_some f x y = match y with
  | None -> Some x
  | Some y -> Some (f x y)

let max q = Unary (Reduce (_id, Pervasives.max, _id), q)
let min q = Unary (Reduce (_id, Pervasives.min, _id), q)
let average q = Unary (Reduce (_avg_start, _avg_mix, _avg_stop), q)

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
  let join_build = PMap._make_build ?eq ?hash ?cmp () in
  let j = {
    join_key1;
    join_key2;
    join_merge=merge;
    join_build;
  } in
  Binary (Join j, q1, q2)

let group_join ?cmp ?eq ?hash gjoin_proj q1 q2 =
  let gjoin_build = PMap._make_build ?eq ?hash ?cmp () in
  let j = {
    gjoin_proj;
    gjoin_build;
  } in
  Binary (GroupJoin j, q1, q2)

let product q1 q2 = Binary (Product, q1, q2)

let append q1 q2 = Binary (Append, q1, q2)

let inter ?cmp ?eq ?hash () q1 q2 =
  let build = PMap._make_build ?cmp ?eq ?hash () in
  Binary (SetOp (Inter, build), q1, q2)

let union ?cmp ?eq ?hash () q1 q2 =
  let build = PMap._make_build ?cmp ?eq ?hash () in
  Binary (SetOp (Union, build), q1, q2)

let diff ?cmp ?eq ?hash () q1 q2 =
  let build = PMap._make_build ?cmp ?eq ?hash () in
  Binary (SetOp (Diff, build), q1, q2)

let fst q = map fst q
let snd q = map snd q

let map1 f q = map (fun (x,y) -> f x, y) q
let map2 f q = map (fun (x,y) -> x, f y) q

let flatten_opt q = filter_map _id q

let opt_unwrap q =
  Unary
    (Map
       (function
         | Some x -> x
         | None -> _exit_with_error "opt_unwrap"),
     q
    )

(** {6 Applicative} *)

let pure x = Return x

let app f x = Binary (App, f, x)

let (<*>) = app

(** {6 Monadic stuff} *)

let return x = Return x

let bind f q = Bind (f,q)

let (>>=) x f = Bind (f, x)

(** {6 Misc} *)

let lazy_ q = Unary (Lazy, q)

let reflect q = Reflect q

(** {6 Infix} *)

module Infix = struct
  let (>>=) = (>>=)
  let (>|=) = (>|=)
  let (<*>) = (<*>)
  let (--) = (--)
end

(** {6 Adapters} *)

let to_seq q = reflect q

let to_hashtbl q =
  Unary (Map (fun c -> Sequence.to_hashtbl c), Reflect q)

let to_queue q =
  Unary (Map (fun c -> let q = Queue.create() in Sequence.to_queue q c; q), Reflect q)

let to_stack q =
  Unary (Map (fun c -> let s = Stack.create () in Sequence.to_stack s c; s), Reflect q)

module List = struct
  let of_list l = OfSeq (Sequence.of_list l)
  let to_list q = map Sequence.to_list (Reflect q)
  let run q = run1 (to_list q)
end

module Array = struct
  let of_array a = OfSeq (Sequence.of_array a)
  let to_array q =
    map (fun s -> Array.of_list (Sequence.to_list s)) (Reflect q)
  let run q = run1 (to_array q)
end

module AdaptSet(S : Set.S) = struct
  let of_set set = OfSeq (fun k -> S.iter k set)

  let to_set q =
    let f c = Sequence.fold (fun set x -> S.add x set) S.empty c in
    map f (reflect q)

  let run q = run1 (to_set q)
end

module AdaptMap(M : Map.S) = struct
  let _to_seq m k = M.iter (fun x y -> k (x,y)) m

  let of_map map = OfSeq (_to_seq map)

  let to_pmap m = {
    PMap.get = (fun x -> try Some (M.find x m) with Not_found -> None);
    PMap.size = (fun () -> M.cardinal m);
    PMap.is_empty = (fun () -> M.is_empty m);
    PMap.fold = (fun f acc -> M.fold (fun x y acc -> f acc x y) m acc);
    PMap.to_seq = _to_seq m;
  }

  let to_map q =
    let f c =
      Sequence.fold (fun m (x,y) -> M.add x y m) M.empty c
    in
    map f (reflect q)

  let run q = run1 (to_map q)
end

module IO = struct
  let _slurp with_input =
    let l = lazy (with_input (fun ic -> CCIO.read_all ic)) in
    lazy_ (return l)

  let slurp ic = _slurp (fun f -> f ic)

  let _with_file_in filename f =
    try
      let ic = open_in filename in
      try
        let x = f ic in
        close_in ic;
        x
      with e ->
        close_in ic;
        _exit_with_error (Printexc.to_string e)
    with e ->
      _exit_with_error (Printexc.to_string e)

  let _with_file_out filename f =
    try
      let oc = open_out filename in
      try
        let x = f oc in
        close_out oc;
        x
      with e ->
        close_out oc;
        _exit_with_error (Printexc.to_string e)
    with e ->
      _exit_with_error (Printexc.to_string e)

  let slurp_file filename = _slurp (_with_file_in filename)

  (* find [c] in [s], starting at offset [i] *)
  let rec _find s c i =
    if i >= String.length s then None
    else if s.[i] = c then Some i
    else _find s c (i+1)

  let rec _lines s i k = match _find s '\n' i with
    | None ->
      if i<String.length s then k (String.sub s i (String.length s-i))
    | Some j ->
        let s' = String.sub s i (j-i) in
        k s';
        _lines s (j+1) k

  let lines q =
    (* sequence of lines *)
    let f s = _lines s 0 in
    flat_map f q

  let lines' q =
    let f s = lazy (Sequence.to_list (_lines s 0)) in
    lazy_ (map f q)

  let _join ~sep ?(stop="") seq =
    let buf = Buffer.create 128 in
    Sequence.iteri
      (fun i x ->
        if i>0 then Buffer.add_string buf sep;
        Buffer.add_string buf x)
      seq;
    Buffer.add_string buf stop;
    Buffer.contents buf

  let unlines q =
    let f l = lazy (_join ~sep:"\n" ~stop:"\n" l) in
    lazy_ (map f (reflect q))

  let join sep q =
    let f l = lazy (_join ~sep l) in
    lazy_ (map f (reflect q))

  let out oc q =
    output_string oc (run1 q)

  let out_lines oc q =
    let x = run q in
    Sequence.iter (fun l -> output_string oc l; output_char oc '\n') x

  let to_file_exn filename q =
    _with_file_out filename (fun oc -> out oc q)

  let to_file filename q =
    try `Ok (_with_file_out filename (fun oc  -> out oc q))
    with Failure s -> `Error s

  let to_file_lines_exn filename q =
    _with_file_out filename (fun oc -> out_lines oc q)

  let to_file_lines filename q =
    try `Ok (_with_file_out filename (fun oc  -> out_lines oc q))
    with Failure s -> `Error s
end
