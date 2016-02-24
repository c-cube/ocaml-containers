
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extension to the standard Hashtbl}  *)

type 'a sequence = ('a -> unit) -> unit
type 'a eq = 'a -> 'a -> bool
type 'a hash = 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Polymorphic tables} *)

let get tbl x =
  try Some (Hashtbl.find tbl x)
  with Not_found -> None

let get_or tbl x ~or_ =
  try Hashtbl.find tbl x
  with Not_found -> or_

(*$=
  "c" (let tbl = of_list [1,"a"; 2,"b"] in get_or tbl 3 ~or_:"c")
  "b" (let tbl = of_list [1,"a"; 2,"b"] in get_or tbl 2 ~or_:"c")
*)

let keys tbl k = Hashtbl.iter (fun key _ -> k key) tbl

let values tbl k = Hashtbl.iter (fun _ v -> k v) tbl

let keys_list tbl = Hashtbl.fold (fun k _ a -> k::a) tbl []
let values_list tbl = Hashtbl.fold (fun _ v a -> v::a) tbl []

let add_list tbl k v =
  let l = try Hashtbl.find tbl k with Not_found -> [] in
  Hashtbl.replace tbl k (v::l)

let incr ?(by=1) tbl x =
  let n = get_or tbl x ~or_:0 in
  if n+by <= 0
  then Hashtbl.remove tbl x
  else Hashtbl.replace tbl x (n+by)

let decr ?(by=1) tbl x =
  try
    let n = Hashtbl.find tbl x in
    if n-by <= 0
    then Hashtbl.remove tbl x
    else Hashtbl.replace tbl x (n-by)
  with Not_found -> ()

let map_list f h =
  Hashtbl.fold
    (fun x y acc -> f x y :: acc)
    h []

(*$T
  of_list [1,"a"; 2,"b"] |> map_list (fun x y -> string_of_int x ^ y) \
    |> List.sort Pervasives.compare = ["1a"; "2b"]
*)

let to_seq tbl k = Hashtbl.iter (fun key v -> k (key,v)) tbl

let add_seq tbl seq = seq (fun (k,v) -> Hashtbl.add tbl k v)

let of_seq seq =
  let tbl = Hashtbl.create 32 in
  add_seq tbl seq;
  tbl

let add_seq_count tbl seq = seq (fun k -> incr tbl k)

let of_seq_count seq =
  let tbl = Hashtbl.create 32 in
  add_seq_count tbl seq;
  tbl

let to_list tbl =
  Hashtbl.fold
    (fun k v l -> (k,v) :: l)
    tbl []

let of_list l =
  let tbl = Hashtbl.create 32 in
  List.iter (fun (k,v) -> Hashtbl.add tbl k v) l;
  tbl

let update tbl ~f ~k =
  let v = get tbl k in
  match v, f k v with
  | None, None -> ()
  | None, Some v' -> Hashtbl.add tbl k v'
  | Some _, Some v' -> Hashtbl.replace tbl k v'
  | Some _, None -> Hashtbl.remove tbl k

(*$R
  let tbl = Hashtbl.create 32 in
  update tbl ~k:1 ~f:(fun _ _ -> Some "1");
  assert_equal (Some "1") (get tbl 1);
  update tbl ~k:2 ~f:(fun _ v->match v with Some _ -> assert false | None -> Some "2");
  assert_equal (Some "2") (get tbl 2);
  assert_equal 2 (Hashtbl.length tbl);
  update tbl ~k:1 ~f:(fun _ _ -> None);
  assert_equal None (get tbl 1);
*)

let print pp_k pp_v fmt m =
  Format.fprintf fmt "@[<hov2>tbl {@,";
  let first = ref true in
  Hashtbl.iter
    (fun k v ->
      if !first then first := false else Format.pp_print_string fmt ", ";
      pp_k fmt k;
      Format.pp_print_string fmt " -> ";
      pp_v fmt v;
      Format.pp_print_cut fmt ()
    ) m;
  Format.fprintf fmt "}@]"

(** {2 Functor} *)

module type S = sig
  include Hashtbl.S

  val get : 'a t -> key -> 'a option
  (** Safe version of {!Hashtbl.find} *)

  val get_or : 'a t -> key -> or_:'a -> 'a
  (** [get_or tbl k ~or_] returns the value associated to [k] if present,
      and returns [or_] otherwise (if [k] doesn't belong in [tbl])
      @since 0.16 *)

  val add_list : 'a list t -> key -> 'a -> unit
  (** [add_list tbl x y] adds [y] to the list [x] is bound to. If [x] is
      not bound, it becomes bound to [[y]].
      @since 0.16 *)

  val incr : ?by:int -> int t -> key -> unit
  (** [incr ?by tbl x] increments or initializes the counter associated with [x].
      If [get tbl x = None], then after update, [get tbl x = Some 1];
      otherwise, if [get tbl x = Some n], now [get tbl x = Some (n+1)].
      @param by if specified, the int value is incremented by [by] rather than 1
      @since 0.16 *)

  val decr : ?by:int -> int t -> key -> unit
  (** Same as {!incr} but substract 1 (or the value of [by]).
      If the value reaches 0, the key is removed from the table.
      This does nothing if the key is not already present in the table.
      @since 0.16 *)

  val keys : 'a t -> key sequence
  (** Iterate on keys (similar order as {!Hashtbl.iter}) *)

  val values : 'a t -> 'a sequence
  (** Iterate on values in the table *)

  val keys_list : _ t -> key list
  (** [keys t] is the list of keys in [t].
      @since 0.8 *)

  val values_list : 'a t -> 'a list
  (** [values t] is the list of values in [t].
      @since 0.8 *)

  val map_list : (key -> 'a -> 'b) -> 'a t -> 'b list
  (** Map on a hashtable's items, collect into a list *)

  val to_seq : 'a t -> (key * 'a) sequence
  (** Iterate on values in the table *)

  val of_seq : (key * 'a) sequence -> 'a t
  (** From the given bindings, added in order *)

  val add_seq : 'a t -> (key * 'a) sequence -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      @since 0.16 *)

  val add_seq_count : int t -> key sequence -> unit
  (** [add_seq_count tbl seq] increments the count of each element of [seq]
      by calling {!incr}. This is useful for counting how many times each
      element of [seq] occurs.
      @since 0.16 *)

  val of_seq_count : key sequence -> int t
  (** Similar to {!add_seq_count}, but allocates a new table and returns it
      @since 0.16 *)

  val to_list : 'a t -> (key * 'a) list
  (** List of bindings (order unspecified)  *)

  val of_list : (key * 'a) list -> 'a t
  (** From the given list of bindings, added in order *)

  val update : 'a t -> f:(key -> 'a option -> 'a option) -> k:key -> unit
  (** [update tbl ~f ~k] updates key [k] by calling [f k (Some v)] if
      [k] was mapped to [v], or [f k None] otherwise; if the call
      returns [None] then [k] is removed/stays removed, if the call
      returns [Some v'] then the binding [k -> v'] is inserted
      using {!Hashtbl.replace}
      @since 0.14 *)

  val print : key printer -> 'a printer -> 'a t printer
  (** Printer for tables
      @since 0.13 *)
end

(*$inject
  module T = Make(CCInt)
*)

module Make(X : Hashtbl.HashedType)
  : S with type key = X.t and type 'a t = 'a Hashtbl.Make(X).t
= struct
  include Hashtbl.Make(X)

  let get tbl x =
    try Some (find tbl x)
    with Not_found -> None

  let get_or tbl x ~or_ =
    try find tbl x
    with Not_found -> or_

  (*$=
    "c" (let tbl = T.of_list [1,"a"; 2,"b"] in T.get_or tbl 3 ~or_:"c")
    "b" (let tbl = T.of_list [1,"a"; 2,"b"] in T.get_or tbl 2 ~or_:"c")
  *)

  let incr ?(by=1) tbl x =
    let n = get_or tbl x ~or_:0 in
    if n+by <= 0
    then remove tbl x
    else replace tbl x (n+by)

  (*$R
    let tbl = T.create 32 in
    T.incr tbl 1 ;
    T.incr tbl 2;
    T.incr tbl 1;
    assert_equal 2 (T.find tbl 1);
    assert_equal 1 (T.find tbl 2);
    assert_equal 2 (T.length tbl);
    T.decr tbl 2;
    assert_equal 0 (T.get_or tbl 2 ~or_:0);
    assert_equal 1 (T.length tbl);
    assert_bool "2 removed" (not (T.mem tbl 2));
  *)

  let add_list tbl k v =
    let l = try find tbl k with Not_found -> [] in
    replace tbl k (v::l)

  let decr ?(by=1) tbl x =
    try
      let n = find tbl x in
      if n-by <= 0
      then remove tbl x
      else replace tbl x (n-by)
    with Not_found -> ()

  let keys tbl k = iter (fun key _ -> k key) tbl

  let values tbl k = iter (fun _ v -> k v) tbl

  let keys_list tbl = fold (fun k _ a -> k::a) tbl []
  let values_list tbl = fold (fun _ v a -> v::a) tbl []

  let map_list f h =
    fold
      (fun x y acc -> f x y :: acc)
      h []

  let update tbl ~f ~k =
    let v = get tbl k in
    match v, f k v with
    | None, None -> ()
    | None, Some v' -> add tbl k v'
    | Some _, Some v' -> replace tbl k v'
    | Some _, None -> remove tbl k

  let to_seq tbl k = iter (fun key v -> k (key,v)) tbl

  let add_seq tbl seq = seq (fun (k,v) -> add tbl k v)

  let of_seq seq =
    let tbl = create 32 in
    add_seq tbl seq;
    tbl

  let add_seq_count tbl seq = seq (fun k -> incr tbl k)

  let of_seq_count seq =
    let tbl = create 32 in
    add_seq_count tbl seq;
    tbl

  let to_list tbl =
    fold
      (fun k v l -> (k,v) :: l)
      tbl []

  let of_list l =
    let tbl = create 32 in
    List.iter (fun (k,v) -> add tbl k v) l;
    tbl

  let print pp_k pp_v fmt m =
    Format.fprintf fmt "@[<hov2>tbl {@,";
    let first = ref true in
    iter
      (fun k v ->
        if !first then first := false else Format.pp_print_string fmt ", ";
        pp_k fmt k;
        Format.pp_print_string fmt " -> ";
        pp_v fmt v;
        Format.pp_print_cut fmt ()
      ) m;
    Format.fprintf fmt "}@]"
end

(** {2 Default Table} *)

module type DEFAULT = sig
  type key

  type 'a t
  (** A hashtable for keys of type [key] and values of type ['a] *)

  val create : ?size:int -> 'a -> 'a t
  (** [create d] makes a new table that maps every key to [d] by default.
      @param size optional size of the initial table *)

  val create_with : ?size:int -> (key -> 'a) -> 'a t
  (** Similar to [create d] but here [d] is a function called to obtain a
      new default value for each distinct key. Useful if the default
      value is stateful. *)

  val get : 'a t -> key -> 'a
  (** Unfailing retrieval (possibly returns the default value) *)

  val set : 'a t -> key -> 'a -> unit
  (** Replace the current binding for this key *)

  val remove : 'a t -> key -> unit
  (** Remove the binding for this key. If [get tbl k] is called later, the
      default value for the table will be returned *)

  val to_seq : 'a t -> (key * 'a) sequence
  (** Pairs of [(elem, count)] for all elements whose count is positive *)
end

module MakeDefault(X : Hashtbl.HashedType) = struct
  type key = X.t

  module T = Hashtbl.Make(X)

  type 'a t = {
    default : key -> 'a;
    tbl : 'a T.t
  }

  let create_with ?(size=32) default = { default; tbl=T.create size }

  let create ?size d = create_with ?size (fun _ -> d)

  let get tbl k =
    try T.find tbl.tbl k
    with Not_found ->
      let v = tbl.default k in
      T.add tbl.tbl k v;
      v

  let set tbl k v = T.replace tbl.tbl k v

  let remove tbl k = T.remove tbl.tbl k

  let to_seq tbl k = T.iter (fun key v -> k (key,v)) tbl.tbl
end

(** {2 Count occurrences using a Hashtbl} *)

module type COUNTER = sig
  type elt
  (** Elements that are to be counted *)

  type t

  val create : int -> t
  (** A counter maps elements to natural numbers (the number of times this
      element occurred) *)

  val incr : t -> elt -> unit
  (** Increment the counter for the given element *)

  val incr_by : t -> int -> elt -> unit
  (** Add or remove several occurrences at once. [incr_by c x n]
      will add [n] occurrences of [x] if [n>0],
      and remove [abs n] occurrences if [n<0]. *)

  val get : t -> elt -> int
  (** Number of occurrences for this element *)

  val decr : t -> elt -> unit
  (** Remove one occurrence of the element
      @since 0.14 *)

  val length : t -> int
  (** Number of distinct elements
      @since 0.14 *)

  val add_seq : t -> elt sequence -> unit
  (** Increment each element of the sequence *)

  val of_seq : elt sequence -> t
  (** [of_seq s] is the same as [add_seq (create ())] *)

  val to_seq : t -> (elt * int) sequence
  (** [to_seq tbl] returns elements of [tbl] along with their multiplicity
      @since 0.14 *)

  val add_list : t -> (elt * int) list -> unit
  (** Similar to {!add_seq}
      @since 0.14 *)

  val of_list : (elt * int) list -> t
  (** Similar to {!of_seq}
      @since 0.14 *)

  val to_list : t -> (elt * int) list
  (** @since 0.14 *)
end

module MakeCounter(X : Hashtbl.HashedType)
  : COUNTER
  with type elt = X.t
  and type t = int Hashtbl.Make(X).t
= struct
  type elt = X.t

  module T = Hashtbl.Make(X)

  type t = int T.t

  let create size = T.create size

  let get tbl x = try T.find tbl x with Not_found -> 0

  let length = T.length

  let incr tbl x =
    let n = get tbl x in
    T.replace tbl x (n+1)

  let incr_by tbl n x =
    let n' = get tbl x in
    if n' + n <= 0
    then T.remove tbl x
    else T.replace tbl x (n+n')

  let decr tbl x = incr_by tbl 1 x

  let add_seq tbl seq = seq (incr tbl)

  let of_seq seq =
    let tbl = create 32 in
    add_seq tbl seq;
    tbl

  let to_seq tbl yield = T.iter (fun x i -> yield (x,i)) tbl

  let add_list tbl l =
    List.iter (fun (x,i) -> incr_by tbl i x) l

  let of_list l =
    let tbl = create 32 in
    add_list tbl l;
    tbl

  let to_list tbl =
    T.fold (fun x i acc -> (x,i) :: acc) tbl []
end

(*$inject
  module C = MakeCounter(CCInt)

  let list_int = Q.(make
    ~print:Print.(list (pair int int))
    ~small:List.length
    ~shrink:Shrink.(list ?shrink:None)
    Gen.(list small_int >|= List.map (fun i->i,1))
  )

  *)

(*$Q
  list_int (fun l -> \
    l |> C.of_list |> C.to_list |> List.length = \
      (l |> CCList.sort_uniq |> List.length))
  list_int (fun l -> \
    l |> C.of_list |> C.to_seq |> Sequence.fold (fun n(_,i)->i+n) 0 = \
      List.fold_left (fun n (_,_) ->n+1) 0 l)
*)
