
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

(* TODO: add CCVector as a collection *)

let _id x = x

exception ExitWithError of string
let _exit_with_error s = raise (ExitWithError s)
let _error_of_exn f = try `Ok (f ()) with ExitWithError s -> `Error

type 'a collection =
  | Seq : 'a sequence -> 'a collection
  | List : 'a list -> 'a collection
  | Set : (module CCSequence.Set.S
           with type elt = 'a and type t = 'b) * 'b -> 'a collection

module PMap = struct
  type ('a, 'b) t = {
    is_empty : unit -> bool;
    size : unit -> int; (** Number of keys *)
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
  let get_err m x = match m.get x with
    | Some y -> `Ok y
    | None -> `Error "PMap.get: lookup error"

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

  type 'a build_method =
    | FromCmp of 'a ord
    | FromHash of 'a equal * 'a hash
    | Default

  let make ?(build=Default) () = match build with
    | Default -> make_hash ()
    | FromCmp cmp -> make_cmp ~cmp ()
    | FromHash (eq,hash) -> make_hash ~eq ~hash ()

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

  let get_exn m x =
    match m.get x with
    | None -> raise Not_found
    | Some x -> x

  (* map values *)
  let map f m = {
    is_empty = m.is_empty;
    size = m.size;
    get = (fun k -> match m.get k with
      | None -> None
      | Some v -> Some (f v)
    );
    to_seq = CCSequence.map (fun (x,y) -> x, f y) m.to_seq;
    fold = (fun f' acc ->
      m.fold (fun acc x y -> f' acc x (f y)) acc
    );
  }

  let to_list m = m.to_seq |> CCSequence.to_rev_list

  let to_coll m = Seq m.to_seq

  let reverse ~build m =
    let build = make ~build () in
    to_seq m
      |> CCSequence.map (fun (x,y) -> y,x)
      |> multimap_of_seq ~build

  let reverse_multimap ~build m =
    let build = make ~build () in
    to_seq m
      |> CCSequence.flatMap
        (fun (x,l) -> 
          CCSequence.of_list l
          |> CCSequence.map (fun y -> y,x)
        )
      |> multimap_of_seq ~build
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

module Coll = struct
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

  let choose_exn (type elt) c =
    let fail () = _exit_with_error "choose: empty collection" in 
    match c with
    | List [] -> fail ()
    | List (x::_) -> x
    | Seq s ->
        begin match CCSequence.take 1 s |> CCSequence.to_list with
        | [x] -> x
        | _ -> fail ()
        end
    | Set (m, set) ->
        let module S = (val m : CCSequence.Set.S
             with type elt = elt and type t = 'b) in
        try S.choose set with Not_found -> fail ()

  let choose_err c =
    try `Ok (choose_exn c)
    with ExitWithError s -> `Error s

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

  let do_join ~join c1 c2 =
    let build1 =
      to_seq c1
      |> CCSequence.map (fun x -> join.join_key1 x, x)
      |> PMap.multimap_of_seq ~build:(PMap.make ~build:join.join_build ())
    in
    let l = CCSequence.fold
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
      ) [] (to_seq c2)
    in
    of_list l

  let do_group_join ~gjoin c1 c2 =
    let build = PMap.make ~build:gjoin.gjoin_build () in
    to_seq c1 (fun x -> PMap.add build x []);
    to_seq c2
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

  let do_product c1 c2 =
    let s1 = to_seq c1 and s2 = to_seq c2 in
    of_seq (CCSequence.product s1 s2)

  let do_union ~build c1 c2 =
    let build = PMap.make ~build () in
    to_seq c1 (fun x -> PMap.add build x ());
    to_seq c2 (fun x -> PMap.add build x ());
    PMap.to_seq (PMap.build_get build)
      |> CCSequence.map fst
      |> of_seq

  type inter_status =
    | InterLeft
    | InterDone  (* already output *)

  let do_inter ~build c1 c2 =
    let build = PMap.make ~build () in
    let l = ref [] in
    to_seq c1 (fun x -> PMap.add build x InterLeft);
    to_seq c2 (fun x ->
      PMap.update build x
        (function
           | None -> Some InterDone
           | Some InterDone as foo -> foo
           | Some InterLeft ->
               l := x :: !l;
               Some InterDone
        )
    );
    of_list !l

  let do_diff ~build c1 c2 =
    let build = PMap.make ~build () in
    to_seq c2 (fun x -> PMap.add build x ());
    let map = PMap.build_get build in
    (* output elements of [c1] not in [map] *)
    to_seq c1
      |> CCSequence.filter (fun x -> not (PMap.mem map x))
      |> of_seq
end

(** {2 Query operators} *)

type (_,_) safety =
  | Explicit : ('a, 'a with_err) safety
  | Implicit : ('a, 'a) safety

type (_, _) unary =
  | PMap : ('a -> 'b) -> ('a collection, 'b collection) unary
  | GeneralMap : ('a -> 'b) -> ('a, 'b) unary
  | Filter : ('a -> bool) -> ('a collection, 'a collection) unary
  | Fold : ('b -> 'a -> 'b) * 'b -> ('a collection, 'b) unary
  | FoldMap : ('acc -> 'a -> 'b -> 'acc) * 'acc
    -> (('a,'b) PMap.t, 'acc) unary
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
  | Get : ('b,'c) safety * 'a -> (('a,'b) PMap.t, 'c) unary
  | GroupBy : 'b PMap.build_method * ('a -> 'b)
    -> ('a collection, ('b,'a list) PMap.t) unary
  | Count : 'a PMap.build_method -> ('a collection, ('a, int) PMap.t) unary
  | Lazy : ('a lazy_t, 'a) unary

type set_op =
  | Union
  | Inter
  | Diff

type (_, _, _) binary =
  | Join : ('a, 'b, 'key, 'c) join_descr
    -> ('a collection, 'b collection, 'c collection) binary
  | GroupJoin : ('a, 'b) group_join_descr
    -> ('a collection, 'b collection, ('a, 'b list) PMap.t) binary
  | Product : ('a collection, 'b collection, ('a*'b) collection) binary
  | Append : ('a collection, 'a collection, 'a collection) binary
  | SetOp : set_op * 'a PMap.build_method
    -> ('a collection, 'a collection, 'a collection) binary

(* type of queries that return a 'a *)
and 'a t =
  | Start : 'a -> 'a t
  | Catch : 'a with_err t -> 'a t
  | Unary : ('a, 'b) unary * 'a t -> 'b t
  | Binary : ('a, 'b, 'c) binary * 'a t * 'b t -> 'c t
  | QueryMap : ('a -> 'b) * 'a t -> 'b t
  | Bind : ('a -> 'b t) * 'a t -> 'b t

let start x = Start x

let of_list l =
  Start (Coll.of_list l)

let of_array a =
  Start (Coll.of_array a)

let of_array_i a =
  Start (CCSequence.of_array_i a |> Coll.of_seq)

let of_hashtbl h =
  Start (Coll.of_seq (CCSequence.of_hashtbl h))

let of_seq seq =
  Start (Coll.of_seq seq)

let of_queue q =
  Start (CCSequence.of_queue q |> Coll.of_seq)

let of_stack s =
  Start (CCSequence.of_stack s |> Coll.of_seq)

let of_string s =
  Start (CCSequence.of_str s |> Coll.of_seq)

(** {6 Execution} *)

let rec _optimize : type a. a t -> a t
  = fun q -> match q with
    | Start _ -> q
    | Catch q' -> Catch (_optimize q')
    | Unary (u, q) ->
        _optimize_unary u (_optimize q)
    | Binary (b, q1, q2) ->
        _optimize_binary b (_optimize q1) (_optimize q2)
    | QueryMap (f, q) -> QueryMap (f, _optimize q)
    | Bind _ -> q  (* cannot optimize before execution *)
and _optimize_unary : type a b. (a,b) unary -> a t -> b t
  = fun u q -> match u, q with
    | PMap f, Unary (PMap g, q') ->
        _optimize_unary (PMap (fun x -> f (g x))) q'
    | Filter p, Unary (PMap f, cont) ->
        _optimize_unary
          (FilterMap (fun x -> let y = f x in if p y then Some y else None))
          cont
    | PMap f, Unary (Filter p, cont) ->
        _optimize_unary
          (FilterMap (fun x -> if p x then Some (f x) else None))
          cont
    | PMap f, Binary (Append, q1, q2) ->
        _optimize_binary Append (Unary (u, q1)) (Unary (u, q2))
    | Filter p, Binary (Append, q1, q2) ->
        _optimize_binary Append (Unary (u, q1)) (Unary (u, q2))
    | Fold (f,acc), Unary (PMap f', cont) ->
        _optimize_unary
          (Fold ((fun acc x -> f acc (f' x)), acc))
          cont
    | Reduce (safety, start, mix, stop), Unary (PMap f, cont) ->
        _optimize_unary
          (Reduce (safety,
            (fun x -> start (f x)),
            (fun x acc -> mix (f x) acc),
            stop))
          cont
    | Size, Unary (PMap _, cont) ->
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
  | PMap f -> Coll.map f c
  | GeneralMap f -> f c
  | Filter p -> Coll.filter p c
  | Fold (f, acc) -> Coll.fold f acc c
  | FoldMap (f, acc) -> PMap.fold f acc c
  | Reduce (safety, start, mix, stop) ->
      let acc = Coll.to_seq c
        |> CCSequence.fold
          (fun acc x -> match acc with
            | None -> Some (start x)
            | Some acc -> Some (mix x acc)
          ) None
      in
      begin match acc, safety with
      | Some x, Implicit -> stop x
      | None, Implicit -> _exit_with_error "reduce: empty collection"
      | Some x, Explicit -> `Ok (stop x)
      | None, Explicit -> `Error "reduce: empty collection"
      end
  | Size -> Coll.size c
  | Choose Implicit -> Coll.choose_exn c
  | Choose Explicit -> Coll.choose_err c
  | FilterMap f -> Coll.filter_map f c
  | FlatMap f -> Coll.flat_map f c
  | Take n -> Coll.take n c
  | TakeWhile p -> Coll.take_while p c
  | Sort cmp -> Coll.sort cmp c
  | Distinct cmp -> Coll.distinct ~cmp c
  | Search obj -> Coll.search obj c
  | Get (Implicit, k) -> PMap.get_exn c k
  | Get (Explicit, k) -> PMap.get_err c k
  | GroupBy (build,f) ->
      Coll.to_seq c
      |> CCSequence.map (fun x -> f x, x)
      |> PMap.multimap_of_seq ~build:(PMap.make ~build ())
  | Contains (eq, x) -> Coll.contains ~eq x c
  | Count build ->
      Coll.to_seq c
      |> PMap.count_of_seq ~build:(PMap.make ~build ())
  | Lazy -> Lazy.force c

let _do_binary : type a b c. (a, b, c) binary -> a -> b -> c
= fun b c1 c2 -> match b with
  | Join join -> Coll.do_join ~join c1 c2
  | GroupJoin gjoin -> Coll.do_group_join ~gjoin c1 c2
  | Product -> Coll.do_product c1 c2
  | Append ->
      Coll.of_seq (CCSequence.append (Coll.to_seq c1) (Coll.to_seq c2))
  | SetOp (Inter,build) -> Coll.do_inter ~build c1 c2
  | SetOp (Union,build) -> Coll.do_union ~build c1 c2
  | SetOp (Diff,build) -> Coll.do_diff ~build c1 c2

let rec _run : type a. opt:bool -> a t -> a
= fun ~opt q -> match q with
  | Start c -> c
  | Catch q' ->
      begin match _run ~opt q' with
        | `Ok x -> x
        | `Error s -> _exit_with_error s
      end
  | Unary (u, q') -> _do_unary u (_run ~opt q')
  | Binary (b, q1, q2) -> _do_binary b (_run ~opt q1) (_run ~opt q2)
  | QueryMap (f, q') -> f (_run ~opt q')
  | Bind (f, q') ->
      let x = _run ~opt q' in
      let q'' = f x in
      let q'' = if opt then _optimize q'' else q'' in
      _run ~opt q''

(* safe execution *)
let run q =
  try `Ok (_run ~opt:true (_optimize q))
  with
  | ExitWithError s -> `Error s
  | e -> `Error (Printexc.to_string e)

let run_exn q =
  match run q with
  | `Ok x -> x
  | `Error s -> failwith s

let run_no_optim q =
  try `Ok (_run ~opt:false q)
  with
  | ExitWithError s -> `Error s
  | e -> `Error (Printexc.to_string e)

(** {6 Basics on Collections} *)

let map f q = Unary (PMap f, q)

let filter p q = Unary (Filter p, q)

let choose q = Unary (Choose Implicit, q)

let choose_err q = Unary (Choose Explicit, q)

let filter_map f q = Unary (FilterMap f, q)

let flat_map f q = Unary (FlatMap f, q)

let flat_map_seq f q =
  let f' x = Coll.of_seq (f x) in
  Unary (FlatMap f', q)

let flat_map_l f q =
  let f' x = Coll.of_list (f x) in
  Unary (FlatMap f', q)

let flatten q = Unary (FlatMap (fun x->x), q)

let flatten_l q = Unary (FlatMap Coll.of_list, q)

let take n q = Unary (Take n, q)

let take_while p q = Unary (TakeWhile p, q)

let sort ?(cmp=Pervasives.compare) () q = Unary (Sort cmp, q)

let distinct ?(cmp=Pervasives.compare) () q =
  Unary (Distinct cmp, q)

(* choose a build method from the optional arguments *)
let _make_build ?cmp ?eq ?hash () =
  let _maybe default o = match o with
    | Some x -> x
    | None -> default
  in
  match eq, hash with
  | Some _, _
  | _, Some _ ->
      PMap.FromHash ( _maybe (=) eq, _maybe Hashtbl.hash hash)
  | _ ->
      match cmp with
      | Some f -> PMap.FromCmp f
      | _ -> PMap.Default

(** {6 Queries on PMaps} *)

module M = struct
  let get key q =
    Unary (Get (Implicit, key), q)

  let get_err key q =
    Unary (Get (Explicit, key), q)

  let iter q =
    Unary (GeneralMap (fun m -> Coll.of_seq m.PMap.to_seq), q)

  let flatten q =
    let f m = m.PMap.to_seq
        |> CCSequence.flatMap
          (fun (k,v) -> Coll.to_seq v |> CCSequence.map (fun v' -> k,v'))
        |> Coll.of_seq
    in
    Unary (GeneralMap f, q)

  let flatten' q =
    let f m = m.PMap.to_seq
        |> CCSequence.flatMap
          (fun (k,v) -> CCSequence.of_list v |> CCSequence.map (fun v' -> k,v'))
        |> Coll.of_seq
    in
    Unary (GeneralMap f, q)

  let map f q =
    Unary (GeneralMap (PMap.map f), q)

  let to_list q =
    Unary (GeneralMap PMap.to_list, q)

  let reverse ?cmp ?eq ?hash () q =
    let build = _make_build ?cmp ?eq ?hash () in
    Unary (GeneralMap (PMap.reverse ~build), q)

  let reverse_multimap  ?cmp ?eq ?hash () q =
    let build = _make_build ?cmp ?eq ?hash () in
    Unary (GeneralMap (PMap.reverse_multimap ~build), q)

  let fold f acc q =
    Unary (FoldMap (f, acc), q)

  let fold_multimap f acc q =
    let f' acc x l =
      List.fold_left (fun acc y -> f acc x y) acc l
    in
    Unary (FoldMap (f', acc), q)
end

let group_by ?cmp ?eq ?hash f q =
  Unary (GroupBy (_make_build ?cmp ?eq ?hash (),f), q)

let group_by' ?cmp ?eq ?hash f q =
  M.iter (group_by ?cmp f q)

let count ?cmp ?eq ?hash () q =
  Unary (Count (_make_build ?cmp ?eq ?hash ()), q)

let count' ?cmp () q =
  M.iter (count ?cmp () q)

let fold f acc q =
  Unary (Fold (f, acc), q)

let size q = Unary (Size, q)

let sum q = Unary (Fold ((+), 0), q)

let reduce start mix stop q =
  Unary (Reduce (Implicit, start,mix,stop), q)

let reduce_err start mix stop q =
  Unary (Reduce (Explicit, start,mix,stop), q)

let _avg_start x = (x,1)
let _avg_mix x (y,n) = (x+y,n+1)
let _avg_stop (x,n) = x/n

let _lift_some f x y = match y with
  | None -> Some x
  | Some y -> Some (f x y)

let max q = Unary (Reduce (Implicit, _id, Pervasives.max, _id), q)
let min q = Unary (Reduce (Implicit, _id, Pervasives.min, _id), q)
let average q = Unary (Reduce (Implicit, _avg_start, _avg_mix, _avg_stop), q)

let max_err q = Unary (Reduce (Explicit, _id, Pervasives.max, _id), q)
let min_err q = Unary (Reduce (Explicit, _id, Pervasives.min, _id), q)
let average_err q = Unary (Reduce (Explicit, _avg_start, _avg_mix, _avg_stop), q)

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
  let join_build = _make_build ?eq ?hash ?cmp () in
  let j = {
    join_key1;
    join_key2;
    join_merge=merge;
    join_build;
  } in
  Binary (Join j, q1, q2)

let group_join ?cmp ?eq ?hash gjoin_proj q1 q2 =
  let gjoin_build = _make_build ?eq ?hash ?cmp () in
  let j = {
    gjoin_proj;
    gjoin_build;
  } in
  Binary (GroupJoin j, q1, q2)

let product q1 q2 = Binary (Product, q1, q2)

let append q1 q2 = Binary (Append, q1, q2)

let inter ?cmp ?eq ?hash () q1 q2 =
  let build = _make_build ?cmp ?eq ?hash () in
  Binary (SetOp (Inter, build), q1, q2)

let union ?cmp ?eq ?hash () q1 q2 =
  let build = _make_build ?cmp ?eq ?hash () in
  Binary (SetOp (Union, build), q1, q2)

let diff ?cmp ?eq ?hash () q1 q2 =
  let build = _make_build ?cmp ?eq ?hash () in
  Binary (SetOp (Diff, build), q1, q2)

let fst q = map fst q
let snd q = map snd q

let map1 f q = map (fun (x,y) -> f x, y) q
let map2 f q = map (fun (x,y) -> x, f y) q

let flatten_opt q = filter_map _id q

let opt_unwrap q =
  QueryMap ((function
    | Some x -> x
    | None -> _exit_with_error "opt_unwrap"), q)

let catch q =
  QueryMap ((function
    | `Ok x -> x
    | `Error s -> _exit_with_error s), q)

(** {6 Monadic stuff} *)

let return x = Start x

let bind f q = Bind (f,q)

let (>>=) x f = Bind (f, x)

let query_map f q = QueryMap (f, q)

(** {6 Misc} *)

let lazy_ q = Unary (Lazy, q)

(** {6 Adapters} *)

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

module L = struct
  let of_list l = Start (Coll.of_list l)
  let to_list q =
    QueryMap (Coll.to_list, q)
  let run q = run (to_list q)
  let run_exn q = run_exn (to_list q)
end

module AdaptSet(S : Set.S) = struct
  let of_set set =
    return (Coll.of_seq (fun k -> S.iter k set))

  let to_set q =
    let f c =
      Coll.to_seq c |> CCSequence.fold (fun set x -> S.add x set) S.empty
    in
    query_map f q

  let run q = run (to_set q)
  let run_exn q = run_exn (to_set q)
end

module AdaptMap(M : Map.S) = struct
  let _to_seq m k = M.iter (fun x y -> k (x,y)) m

  let of_map map =
    return (Coll.of_seq (_to_seq map))

  let to_pmap m = {
    PMap.get = (fun x -> try Some (M.find x m) with Not_found -> None);
    PMap.size = (fun () -> M.cardinal m);
    PMap.is_empty = (fun () -> M.is_empty m);
    PMap.fold = (fun f acc -> M.fold (fun x y acc -> f acc x y) m acc);
    PMap.to_seq = _to_seq m;
  }

  let to_map q =
    let f c =
      Coll.to_seq c
      |> CCSequence.fold (fun m (x,y) -> M.add x y m) M.empty
    in
    query_map f q

  let run q = run (q |> to_map)
  let run_exn q = run_exn (q |> to_map)
end

module IO = struct
  let _slurp with_input =
    let l = lazy (
      with_input
        (fun ic ->
          let buf_size = 256 in
          let content = Buffer.create 120
          and buf = String.make buf_size 'a' in
          let rec next () =
            let num = input ic buf 0 buf_size in
            if num = 0
              then Buffer.contents content (* EOF *)
              else (Buffer.add_substring content buf 0 num; next ())
          in next ()
        )
    ) in
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
    | None -> ()
    | Some j ->
        let s' = String.sub s i (j-i) in
        k s';
        _lines s (j+1) k

  let lines q =
    (* sequence of lines *)
    let f s = _lines s 0 |> Coll.of_seq in
    query_map f q
      
  let lines' q =
    let f s = lazy (_lines s 0 |> CCSequence.to_list) in
    lazy_ (query_map f q)

  let _join ~sep ?(stop="") l =
    let buf = Buffer.create 128 in
    Coll.to_seq l
      |> CCSequence.iteri
        (fun i x ->
          if i>0 then Buffer.add_string buf sep;
          Buffer.add_string buf x);
    Buffer.add_string buf stop;
    Buffer.contents buf

  let unlines q =
    let f l = lazy (_join ~sep:"\n" ~stop:"\n" l) in
    lazy_ (query_map f q)

  let join sep q =
    let f l = lazy (_join ~sep l) in
    lazy_ (query_map f q)

  let out oc q =
    run_exn q |> output_string oc

  let out_lines oc q =
    run_exn q
    |> Coll.to_seq
    |> CCSequence.iter (fun l -> output_string oc l; output_char oc '\n')

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
