(*
copyright (c) 2013, simon cruanes
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

(** {1 Multimap} *)

type 'a sequence = ('a -> unit) -> unit

module type S = sig
  type key
  type value
  type t

  val empty : t
    (** Empty multimap *)

  val is_empty : t -> bool
    (** Empty multimap? *)

  val add : t -> key -> value -> t
    (** Add a key/value binding *)

  val remove : t -> key -> value -> t
    (** Remove the binding *)

  val remove_all : t -> key -> t
    (** Remove the key from the map *)

  val mem : t -> key -> bool
    (** Is there a binding for this key? *)

  val find : t -> key -> value list
    (** List of values for this key *)

  val find_iter : t -> key -> (value -> unit) -> unit
    (** Iterate on bindings for this key *)

  val count : t -> key -> int
    (** Number of bindings for this key *)

  val iter : t -> (key -> value -> unit) -> unit
    (** Iterate on all key/value *)

  val fold : t -> 'a -> ('a -> key -> value -> 'a) -> 'a
    (** Fold on all key/value *)

  val size : t -> int
    (** Number of keys *)

  val union : t -> t -> t
    (** Union of multimaps *)

  val inter : t -> t -> t
    (** Intersection of multimaps *)

  val diff : t -> t -> t
    (** Difference of maps, ie bindings of the first that are not
        in the second *)

  val equal : t -> t -> bool
    (** Same multimap *)

  val compare : t -> t -> int
    (** Total order on multimaps *)

  val submap : t -> t -> bool
    (** [submap m1 m2] is true iff all bindings of [m1] are also in [m2] *)

  val to_seq : t -> (key * value) sequence

  val of_seq : ?init:t -> (key * value) sequence -> t

  val keys : t -> key sequence

  val values : t -> value sequence
    (** Some values may occur several times *)
end

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make(K : OrderedType)(V : OrderedType) = struct
  type key = K.t
  type value = V.t

  module M = Map.Make(K)
  module S = Set.Make(V)

  type t = S.t M.t
    (** Map of sets *)

  let empty = M.empty

  let is_empty = M.is_empty

  let add m k v =
    let set = try M.find k m with Not_found -> S.empty in
    M.add k (S.add v set) m

  let remove m k v =
    try
      let set = M.find k m in
      let set' = S.remove v set in
      if S.is_empty set'
        then M.remove k m
        else M.add k set' m
    with Not_found ->
      m

  let remove_all m k =
    M.remove k m

  let mem m k = M.mem k m

  let find m k =
    try
      let set = M.find k m in
      S.elements set
    with Not_found ->
      []

  let find_iter m k f =
    try
      let set = M.find k m in
      S.iter f set
    with Not_found ->
      ()

  let count m k =
    try
      let set = M.find k m in
      S.cardinal set
    with Not_found ->
      0

  let iter m f =
    M.iter (fun k set -> S.iter (fun v -> f k v) set) m

  let fold m acc f =
    M.fold (fun k set acc -> S.fold (fun v acc -> f acc k v) set acc) m acc

  let size m = M.cardinal m

  let union m1 m2 =
    M.merge
      (fun _k v1 v2 -> match v1, v2 with
        | None, None -> None
        | Some set1, Some set2 -> Some (S.union set1 set2)
        | Some set, None
        | None, Some set -> Some set)
      m1 m2

  let inter m1 m2 =
    M.merge
      (fun _k v1 v2 -> match v1, v2 with
        | None, _
        | _, None -> None
        | Some set1, Some set2 ->
          let set = S.inter set1 set2 in
          if S.is_empty set
            then None
            else Some set)
      m1 m2

  let diff m1 m2 =
    M.merge
      (fun _k v1 v2 -> match v1, v2 with
        | None, _ -> None
        | Some set, None -> Some set
        | Some set1, Some set2 ->
          let set' = S.diff set1 set2 in
          if S.is_empty set'
            then None
            else Some set')
      m1 m2

  let equal m1 m2 =
    M.equal S.equal m1 m2

  let compare m1 m2 =
    M.compare S.compare m1 m2

  let submap m1 m2 =
    M.for_all
      (fun k set1 ->
        try
          let set2 = M.find k m2 in
          S.subset set1 set2
        with Not_found ->
          false)
      m1

  let to_seq m k = iter m (fun x y -> k (x,y))

  let of_seq ?(init=empty) seq =
    let m = ref init in
    seq (fun (k,v) -> m := add !m k v);
    !m

  let keys m k = M.iter (fun x _ -> k x) m

  let values m k = iter m (fun _ v -> k v)
end

module type BIDIR = sig
  type t
  type left
  type right

  val empty : t

  val is_empty : t -> bool

  val add : t -> left -> right -> t
  (** Add a binding (left,right) *)

  val remove : t -> left -> right -> t
  (** Remove a specific binding *)

  val cardinal_left : t -> int
  (** number of distinct left keys *)

  val cardinal_right : t -> int
  (** number of distinct right keys *)

  val remove_left : t -> left -> t
  (** Remove all bindings for the left key *)

  val remove_right : t -> right -> t
  (** Remove all bindings for the right key *)

  val mem_left : t -> left -> bool
  (** Is the left key present in at least one pair? *)

  val mem_right : t -> right -> bool
  (** Is the right key present in at least one pair? *)

  val find_left : t -> left -> right sequence
  (** Find all bindings for this given left-key *)

  val find_right : t -> right -> left sequence
  (** Find all bindings for this given right-key *)

  val find1_left : t -> left -> right option
  (** like {!find_left} but returns at most one value *)

  val find1_right : t -> right -> left option
  (** like {!find_right} but returns at most one value *)

  val fold : ('a -> left -> right -> 'a) -> 'a -> t -> 'a
  (** Fold on pairs *)

  val pairs : t -> (left * right) sequence
  (** Iterate on pairs *)

  val add_pairs : t -> (left * right) sequence -> t
  (** Add pairs *)

  val seq_left : t -> left sequence
  val seq_right : t -> right sequence
end

let _fold_seq f acc seq =
  let acc = ref acc in
  seq (fun x -> acc := f !acc x);
  !acc

let _head_seq seq =
  let r = ref None in
  begin try seq (fun x -> r := Some x; raise Exit)
  with Exit -> ();
  end;
  !r

module MakeBidir(L : OrderedType)(R : OrderedType) = struct
  type left = L.t
  type right = R.t

  module MapL = Make(L)(R)
  module MapR = Make(R)(L)

  type t = {
    left : MapL.t;
    right : MapR.t;
  }

  let empty = {
    left = MapL.empty;
    right = MapR.empty;
  }

  let is_empty m = MapL.is_empty m.left

  let add m a b = {
    left = MapL.add m.left a b;
    right = MapR.add m.right b a;
  }

  let remove m a b = {
    left = MapL.remove m.left a b;
    right = MapR.remove m.right b a;
  }

  let cardinal_left m = MapL.size m.left
  let cardinal_right m = MapR.size m.right

  let find_left m a = MapL.find_iter m.left a
  let find_right m b = MapR.find_iter m.right b

  let remove_left m a =
    _fold_seq
      (fun m b -> remove m a b)
      m (find_left m a)

  let remove_right m b =
    _fold_seq
      (fun m a -> remove m a b)
      m (find_right m b)

  let mem_left m a = MapL.mem m.left a
  let mem_right m b = MapR.mem m.right b

  let find1_left m a = _head_seq (find_left m a)
  let find1_right m b = _head_seq (find_right m b)

  let fold f acc m =
    MapL.fold m.left acc f

  let pairs m = MapL.to_seq m.left

  let add_pairs m seq = _fold_seq (fun m (a,b) -> add m a b) m seq

  let seq_left m = MapL.keys m.left

  let seq_right m = MapR.keys m.right
end
