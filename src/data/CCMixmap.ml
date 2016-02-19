
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Maps with Heterogeneous Values} *)

(*$R
  let module M = CCMixmap.Make(CCInt) in

  let inj_int = CCMixmap.create_inj() in
  let inj_str = CCMixmap.create_inj() in
  let inj_list_int = CCMixmap.create_inj() in

  let m =
    M.empty
    |> M.add ~inj:inj_int 1 1
    |> M.add ~inj:inj_str 2 "2"
    |> M.add ~inj:inj_list_int 3 [3;3;3]
  in

  assert_equal (M.get ~inj:inj_int 1 m) (Some 1) ;
  assert_equal (M.get ~inj:inj_str 1 m) None ;
  assert_equal (M.get ~inj:inj_str 2 m) (Some "2") ;
  assert_equal (M.get ~inj:inj_int 2 m) None ;
  assert_equal (M.get ~inj:inj_list_int 3 m) (Some [3;3;3]) ;
  assert_equal (M.get ~inj:inj_str 3 m) None ;
*)

type 'b injection = {
  get : (unit -> unit) -> 'b option;
  set : 'b -> (unit -> unit);
}

let create_inj () =
  let r = ref None in
  let get f =
    r := None;
    f ();
    !r
  and set v =
    (fun () -> r := Some v)
  in
  {get;set}

module type S = sig
  type key

  type t
  (** A map containing values of different types, indexed by {!key}. *)

  val empty : t
  (** Empty map *)

  val get : inj:'a injection -> key -> t -> 'a option
  (** Get the value corresponding to this key, if it exists and
      belongs to the same key *)

  val add : inj:'a injection -> key -> 'a -> t -> t
  (** Bind the key to the value, using [inj] *)

  val find : inj:'a injection -> key -> t -> 'a
  (** Find the value for the given key, which must be of the right type.
      @raise Not_found if either the key is not found, or if its value
        doesn't belong to the right type *)

  val cardinal : t -> int
  (** Number of bindings *)

  val remove : key -> t -> t
  (** Remove the binding for this key *)

  val mem : inj:_ injection-> key -> t -> bool
  (** Is the given key in the map, with the right type? *)

  val iter_keys : f:(key -> unit) -> t -> unit
  (** Iterate on the keys of this map *)

  val fold_keys : f:('a -> key -> 'a) -> x:'a -> t -> 'a
  (** Fold over the keys *)

  (** {2 Iterators} *)

  type 'a sequence = ('a -> unit) -> unit

  val keys_seq : t -> key sequence
  (** All the keys *)

  val bindings_of : inj:'a injection -> t -> (key * 'a) sequence
  (** All the bindings that come from the corresponding injection *)

  type value =
    | Value : ('a injection -> 'a option) -> value

  val bindings : t -> (key * value) sequence
  (** Iterate on all bindings *)
end

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module Make(X : ORD) : S with type key = X.t = struct
  module M = Map.Make(X)

  type key = X.t
  type t = (unit -> unit) M.t

  let empty = M.empty

  let find ~inj x map =
    match inj.get (M.find x map) with
      | None -> raise Not_found
      | Some v -> v

  let get ~inj x map =
    try inj.get (M.find x map)
    with Not_found -> None

  let add ~inj x y map =
    M.add x (inj.set y) map

  let cardinal = M.cardinal

  let remove = M.remove

  let mem ~inj x map =
    try
      inj.get (M.find x map) <> None
    with Not_found -> false

  let iter_keys ~f map =
    M.iter (fun x _ -> f x) map

  let fold_keys ~f ~x map =
    M.fold (fun x _ acc -> f acc x) map x

  (** {2 Iterators} *)

  type 'a sequence = ('a -> unit) -> unit

  let keys_seq map yield =
    M.iter
      (fun x _ -> yield x)
      map

  let bindings_of ~inj map yield =
    M.iter
      (fun k value ->
        match inj.get value with
        | None -> ()
        | Some v -> yield (k, v)
      ) map

  type value =
    | Value : ('b injection -> 'b option) -> value

  let bindings map yield =
    M.iter
      (fun x y -> yield (x, Value (fun inj -> inj.get y)))
      map
end
