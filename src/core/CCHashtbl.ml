
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

(** {1 Extension to the standard Hashtbl}  *)

type 'a sequence = ('a -> unit) -> unit
type 'a eq = 'a -> 'a -> bool
type 'a hash = 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Polymorphic tables} *)

let get tbl x =
  try Some (Hashtbl.find tbl x)
  with Not_found -> None

let keys tbl k = Hashtbl.iter (fun key _ -> k key) tbl

let values tbl k = Hashtbl.iter (fun _ v -> k v) tbl

let keys_list tbl = Hashtbl.fold (fun k _ a -> k::a) tbl []
let values_list tbl = Hashtbl.fold (fun _ v a -> v::a) tbl []

let map_list f h =
  Hashtbl.fold
    (fun x y acc -> f x y :: acc)
    h []

(*$T
  of_list [1,"a"; 2,"b"] |> map_list (fun x y -> string_of_int x ^ y) \
    |> List.sort Pervasives.compare = ["1a"; "2b"]
*)

let to_seq tbl k = Hashtbl.iter (fun key v -> k (key,v)) tbl

let of_seq seq =
  let tbl = Hashtbl.create 32 in
  seq (fun (k,v) -> Hashtbl.add tbl k v);
  tbl

let to_list tbl =
  Hashtbl.fold
    (fun k v l -> (k,v) :: l)
    tbl []

let of_list l =
  let tbl = Hashtbl.create 32 in
  List.iter (fun (k,v) -> Hashtbl.add tbl k v) l;
  tbl

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

  val keys : 'a t -> key sequence
  (** Iterate on keys (similar order as {!Hashtbl.iter}) *)

  val values : 'a t -> 'a sequence
  (** Iterate on values in the table *)

  val keys_list : ('a, 'b) Hashtbl.t -> 'a list
  (** [keys t] is the list of keys in [t].
      @since 0.8 *)

  val values_list : ('a, 'b) Hashtbl.t -> 'b list
  (** [values t] is the list of values in [t].
      @since 0.8 *)

  val map_list : (key -> 'a -> 'b) -> 'a t -> 'b list
  (** Map on a hashtable's items, collect into a list *)

  val to_seq : 'a t -> (key * 'a) sequence
  (** Iterate on values in the table *)

  val of_seq : (key * 'a) sequence -> 'a t
  (** From the given bindings, added in order *)

  val to_list : 'a t -> (key * 'a) list
  (** List of bindings (order unspecified)  *)

  val of_list : (key * 'a) list -> 'a t
  (** From the given list of bindings, added in order *)

  val print : key printer -> 'a printer -> 'a t printer
end

module Make(X : Hashtbl.HashedType) = struct
  include Hashtbl.Make(X)

  let get tbl x =
    try Some (find tbl x)
    with Not_found -> None

  let keys tbl k = iter (fun key _ -> k key) tbl

  let values tbl k = iter (fun _ v -> k v) tbl

  let keys_list tbl = Hashtbl.fold (fun k _ a -> k::a) tbl []
  let values_list tbl = Hashtbl.fold (fun _ v a -> v::a) tbl []

  let map_list f h =
    fold
      (fun x y acc -> f x y :: acc)
      h []

  let to_seq tbl k = iter (fun key v -> k (key,v)) tbl

  let of_seq seq =
    let tbl = create 32 in
    seq (fun (k,v) -> add tbl k v);
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
    Format.pp_print_string fmt "@[<hov2>tbl {@,";
    let first = ref true in
    iter
      (fun k v ->
        if !first then first := false else Format.pp_print_string fmt ", ";
        pp_k fmt k;
        Format.pp_print_string fmt " -> ";
        pp_v fmt v;
        Format.pp_print_cut fmt ()
      ) m;
    Format.pp_print_string fmt "}@]"
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
  (** Add several occurrences at once *)

  val get : t -> elt -> int
  (** Number of occurrences for this element *)

  val add_seq : t -> elt sequence -> unit
  (** Increment each element of the sequence *)

  val of_seq : elt sequence -> t
  (** [of_seq s] is the same as [add_seq (create ())] *)
end

module MakeCounter(X : Hashtbl.HashedType) = struct
  type elt = X.t

  module T = Hashtbl.Make(X)

  type t = int T.t

  let create size = T.create size

  let get tbl x = try T.find tbl x with Not_found -> 0

  let incr tbl x =
    let n = get tbl x in
    T.replace tbl x (n+1)

  let incr_by tbl n x =
    let n' = get tbl x in
    if n' + n <= 0
    then T.remove tbl x
    else T.replace tbl x (n+n')

  let add_seq tbl seq = seq (incr tbl)

  let of_seq seq =
    let tbl = create 32 in
    add_seq tbl seq;
    tbl
end
