(*
copyright (c) 2013-2015, simon cruanes
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

(** {1 Bigarrays of dimension 1}

    @deprecated do not use, this was always experimental
    {b NOTE this module will be removed soon and should not be depended upon}

    {b status: deprecated}
    @since 0.12 *)

(** {2 used types} *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a sequence = ('a -> unit) -> unit
type 'a or_error = [`Ok of 'a | `Error of string]
type random = Random.State.t

type json = [ `Assoc of (string * json) list
            | `Bool of bool
            | `Float of float
            | `Int of int
            | `List of json list
            | `Null
            | `String of string ]
type 'a to_json = 'a -> json
type 'a of_json = json -> 'a or_error

(** {2 Type Declarations} *)

type ('a, 'b, 'perm) t constraint 'perm = [< `R | `W]
(** Array of OCaml values of type ['a] with C representation of type [b']
    with permissions ['perm] *)

type ('a, 'b, 'perm) array_ = ('a, 'b, 'perm) t

exception WrongDimension
(** Raised when arrays do not have expected length *)

(** {2 Basic Operations} *)

val make : ?x:'a -> kind:('a,'b) Bigarray.kind -> int -> ('a, 'b, 'perm) t
(** New array with undefined elements
    @param kind the kind of bigarray
    @param x optional element to fill every slot
    @param n the number of elements *)

val make_int : int -> (int, Bigarray.int_elt, 'perm) t
val make_char : int -> (char, Bigarray.int8_unsigned_elt, 'perm) t
val make_int8s : int -> (int, Bigarray.int8_signed_elt, 'perm) t
val make_int8u : int -> (int, Bigarray.int8_unsigned_elt, 'perm) t
val make_int16s : int -> (int, Bigarray.int16_signed_elt, 'perm) t
val make_int16u : int -> (int, Bigarray.int16_unsigned_elt, 'perm) t
val make_int32 : int -> (int32, Bigarray.int32_elt, 'perm) t
val make_int64 : int -> (int64, Bigarray.int64_elt, 'perm) t
val make_native : int -> (nativeint, Bigarray.nativeint_elt, 'perm) t
val make_float32 : int -> (float, Bigarray.float32_elt, 'perm) t
val make_float64 : int -> (float, Bigarray.float64_elt, 'perm) t
val make_complex32 : int -> (Complex.t, Bigarray.complex32_elt, 'perm) t
val make_complex64 : int -> (Complex.t, Bigarray.complex64_elt, 'perm) t

val init : kind:('a, 'b) Bigarray.kind -> f:(int -> 'a) -> int -> ('a, 'b, 'perm) t
(** Initialize with given size and initialization function *)

val of_bigarray : ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t -> ('a, 'b, 'perm) t
(** Convert from a big array *)

val to_bigarray : ('a, 'b, [`R | `W]) t -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
(** Obtain the underlying array *)

val ro : ('a, 'b, [>`R]) t -> ('a, 'b, [`R]) t
(** Change permission (old reference to array might still be mutable!) *)

val wo : ('a, 'b, [>`W]) t -> ('a, 'b, [`W]) t
(** Change permission *)

val length : (_, _, [>`R]) t -> int
(** Number of elements *)

val set : ('a, _, [>`W]) t -> int -> 'a -> unit
(** set n-th element *)

val get : ('a, _, [>`R]) t -> int -> 'a
(** Get n-th element *)

val fill : ('a, _, [>`W]) t -> 'a -> unit
(** [fill a x] fills [a] with [x] *)

val sub : ('a, 'b, 'perm) t -> int -> int -> ('a, 'b, 'perm) t
(** [sub a i len] takes the slice of length [len] starting at offset [i] *)

val blit : ('a, 'b, [>`R]) t -> ('a, 'b, [>`W]) t -> unit
(** blit the first array to the second *)

val copy : ('a, 'b, [>`R]) t -> ('a, 'b, 'perm) t
(** Fresh copy *)

val iter : f:('a -> unit) -> ('a, _, [>`R]) t -> unit
(** [iter a ~f] calls [f v] where [get a i = v] for each [i < length a].
    It iterates on all bits in increasing order *)

val iteri : f:(int -> 'a -> unit) -> ('a, _, [>`R]) t -> unit
(** [iteri a ~f] calls [f i v] where [get a i = v] for each [i < length a].
    It iterates on all elements in increasing order *)

val foldi : ('b -> int -> 'a -> 'b) -> 'b -> ('a, _, [>`R]) t -> 'b

val for_all : f:('a -> bool) -> ('a, _, [>`R]) t -> bool

val exists : f:('a -> bool) -> ('a, _, [>`R]) t -> bool

val pp : 'a printer -> ('a, _, [>`R]) t printer
(** Print the SDR nicely *)

(** {2 Boolean Vectors} *)

module Bool : sig
  type ('b, 'perm) t = (int, 'b, 'perm) array_
  (** A simple bitvector based on some integral type ['b] *)

  val get : (_, [>`R]) t -> int -> bool

  val set : (_, [>`W]) t -> int -> bool -> unit

  val zeroes : int -> (Bigarray.int8_unsigned_elt, 'perm) t
  val ones : int -> (Bigarray.int8_unsigned_elt, 'perm) t

  val iter_zeroes : f:(int -> unit) -> (_, [>`R]) t -> unit
  (** [iter_ones ~f a] calls [f i] for every index [i] such that [get a i = false] *)

  val iter_ones : f:(int -> unit) -> (_, [>`R]) t -> unit
  (** [iter_ones ~f a] calls [f i] for every index [i] such that [get a i = true] *)

  val cardinal : (_, [>`R]) t -> int
  (** Number of ones *)

  val pp : (_,[>`R]) t printer
  (** Print the bitvector nicely *)

  (** {6 Operations} *)

  val or_ : ?res:('b, [>`W] as 'perm) t -> ('b, [>`R]) t -> ('b, [>`R]) t -> ('b, 'perm) t
  (** [or_ a b ~into] puts the boolean "or" of [a] and [b] in [into]
      expects [length into = max (length a) (length b)]
      @raise WrongDimension if dimensions do not match *)

  val and_ : ?res:('b, [>`W] as 'perm) t -> ('b, [>`R]) t -> ('b, [>`R]) t -> ('b, 'perm) t
  (** Boolean conjunction. See {!or} for the parameters *)

  val not_ : ?res:('b, [>`W] as 'perm) t -> ('b, [>`R]) t -> ('b, 'perm) t
  (** Boolean negation (negation of a 0 becomes a 1) *)

  val mix : ?res:('b, [>`W] as 'perm) t -> ('b, [>`R]) t -> ('b, [>`R]) t -> ('b, 'perm) t
  (** [mix a b ~into] assumes [length a + length b = length into] and
      mixes (interleaves) bits of [a] and [b] in [into].
      @raise WrongDimension if dimensions do not match *)

  val convolution : ?res:('b, [>`W] as 'perm) t -> ('b,[>`R]) t -> by:('b, [>`R]) t -> ('b,'perm) t
  (** [convolution a ~by:b ~into] assumes [length into = length a >= length b]
      and computes the boolean convolution of [a] by [b]
      @raise WrongDimension if dimensions do not match *)
end

(** {2 Operations} *)

val map :
  ?res:('a, 'b, ([>`W] as 'perm)) t ->
  f:('a -> 'a) ->
  ('a, 'b, [>`R]) t ->
  ('a, 'b, 'perm) t

val map2 :
  ?res:('a, 'b, ([>`W] as 'perm)) t ->
  f:('a -> 'a2 -> 'a) ->
  ('a, 'b, [>`R]) t ->
  ('a2, _, [>`R]) t ->
  ('a, 'b, 'perm) t

val append :
  ?res:('a, 'b, ([>`W] as 'perm)) t ->
  ('a, 'b, [>`R]) t ->
  ('a, 'b, [>`R]) t ->
  ('a, 'b, 'perm) t
(** [append a b ~into] assumes [length a + length b = length into] and
    copies [a] and [b] side by side in [into]
    @raise WrongDimension if dimensions do not match *)

val filter :
  ?res:(Bigarray.int8_unsigned_elt, [>`W] as 'perm) Bool.t ->
  f:('a -> bool) ->
  ('a, 'b, [>`R]) t ->
  (Bigarray.int8_unsigned_elt, 'perm) Bool.t

module type S = sig
  type elt
  type ('a, 'perm) t = (elt, 'a, 'perm) array_

  val add :
    ?res:('a, [>`W] as 'perm) t ->
    ('a, [>`R]) t ->
    ('a, [>`R]) t ->
    ('a, 'perm) t
  (** Elementwise sum
      @raise WrongDimension if dimensions do not fit *)

  val mult :
    ?res:('a, [>`W] as 'perm) t ->
    ('a, [>`R]) t ->
    ('a, [>`R]) t ->
    ('a, 'perm) t
  (** Elementwise product *)

  val scalar_add :
    ?res:('a, [>`W] as 'perm) t ->
    ('a, [>`R]) t ->
    x:elt ->
    ('a, 'perm) t
    (** @raise WrongDimension if dimensions do not fit *)

  val scalar_mult :
    ?res:('a, [>`W] as 'perm) t ->
    ('a, [>`R]) t ->
    x:elt ->
    ('a, 'perm) t
    (** @raise WrongDimension if dimensions do not fit *)

  val sum_elt : (_, [>`R]) t -> elt
  (** Efficient sum of elements *)

  val product_elt : (_, [>`R]) t -> elt
  (** Efficient product of elements *)

  val dot_product : (_, [>`R]) t -> (_, [>`R]) t -> elt
  (** [dot_product a b] returns [sum_i a(i)*b(i)] with the given
      sum and product, on [elt].
      [dot_product a b = sum_elt (product a b)]
      @raise WrongDimension if [a] and [b] do not have the same size *)

  module Infix : sig
    val ( * ) : ('a, [>`R]) t -> ('a, [>`R]) t -> ('a, 'perm) t
    (** Alias to {!mult} *)

    val ( + ) : ('a, [>`R]) t -> (_, [>`R]) t -> ('a, 'perm) t
    (** Alias to {!add} *)

    val ( *! ) : ('a, [>`R]) t -> elt -> ('a, 'perm) t
    (** Alias to {!scalar_mult} *)

    val ( +! ) : ('a, [>`R]) t -> elt -> ('a, 'perm) t
    (** Alias to {!scalar_add} *)
  end

  include module type of Infix
end

module Int : S with type elt = int

module Float : S with type elt = float

(** {2 Conversions} *)

val to_list : ('a, _, [>`R]) t -> 'a list
val to_array : ('a, _, [>`R]) t -> 'a array
val to_seq : ('a, _, [>`R]) t -> 'a sequence

val of_array : kind:('a, 'b) Bigarray.kind -> 'a array -> ('a, 'b, 'perm) t

(** {2 Serialization} *)

val to_yojson : 'a to_json -> ('a, _, [>`R]) t to_json
val of_yojson : kind:('a, 'b) Bigarray.kind -> 'a of_json -> ('a, 'b, 'perm) t of_json

val int_to_yojson : int to_json
val int_of_yojson : int of_json
val float_to_yojson : float to_json
val float_of_yojson : float of_json

(** {2 Views} *)

module View : sig
  type 'a t
  (** A view on an array or part of an array *)

  val of_array : ('a, _, [>`R]) array_ -> 'a t

  val get : 'a t -> int -> 'a
  (** [get v i] returns the [i]-th element of [v]. Caution, this is not
      as cheap as a regular array indexing, and it might involve recursion.
      @raise Invalid_argument if index out of bounds *)

  val length : _ t -> int
  (** [length v] is the number of elements of [v] *)

  val map : f:('a -> 'b) -> 'a t -> 'b t
  (** Map values *)

  val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Map values
      @raise WrongDimension if lengths do not fit *)

  val select : idx:(int, _, [>`R]) array_ -> 'a t -> 'a t
  (** [select ~idx v] is the view that has length [length idx]
      and such that [get (select ~idx a) i = get a (get idx i)] *)

  val select_a : idx:int array -> 'a t -> 'a t
  (** See {!select} *)

  val select_view : idx:int t -> 'a t -> 'a t
  (** See {!select} *)

  val foldi : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on values with their index *)

  val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
  (** [iteri ~f v] iterates on elements of [v] with their index *)

  module type S = sig
    type elt
    val mult : elt t -> elt t -> elt t
    val add : elt t -> elt t -> elt t
    val sum : elt t -> elt
    val prod : elt t -> elt
    val add_scalar : elt t -> x:elt -> elt t
    val mult_scalar : elt t -> x:elt -> elt t
  end

  module Int : sig
    include S with type elt = int
  end

  module Float : sig
    include S with type elt = float
    (* TODO: more, like trigo functions *)
  end

  val raw :
    length:(('a, 'b, [>`R]) array_ -> int) ->
    get:(('a, 'b, [>`R]) array_ -> int -> 'a) ->
    ('a, 'b, [>`R]) array_ ->
    'a t

  val to_array :
    ?res:('a, 'b, [>`W] as 'perm) array_ ->
    ?kind:('a, 'b) Bigarray.kind ->
    'a t ->
    ('a, 'b, 'perm) array_
  (** [to_array v] returns a fresh copy of the content of [v].
      Exactly one of [res] and [kind] must be provided *)
end
