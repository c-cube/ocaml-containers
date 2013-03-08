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

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
  damages (including, but not limited to, procurement of substitute goods or
  services; loss of use, data, or profits; or business interruption) however
  caused and on any theory of liability, whether in contract, strict liability,
  or tort (including negligence or otherwise) arising in any way out of the use
  of this software, even if advised of the possibility of such damage.
*)

(** Transient iterators, that abstract on a finite sequence of elements. They
    are designed to allow easy transfer (mappings) between data structures,
    without defining n^2 conversions between the n types. *)

type +'a t = ('a -> unit) -> unit
  (** Sequence abstract iterator type, representing a finite sequence of
      values of type ['a]. *)

type (+'a, +'b) t2 = ('a -> 'b -> unit) -> unit
  (** Sequence of pairs of values of type ['a] and ['b]. *)

(** {2 Build a sequence} *)

val from_iter : (('a -> unit) -> unit) -> 'a t
  (** Build a sequence from a iter function *)

val empty : 'a t
  (** Empty sequence *)

val singleton : 'a -> 'a t
  (** Singleton sequence *)

val repeat : 'a -> 'a t
  (** Infinite sequence of the same element *)

val iterate : ('a -> 'a) -> 'a -> 'a t
  (** [iterate f x] is the infinite sequence (x, f(x), f(f(x)), ...) *)

val forever : (unit -> 'b) -> 'b t
  (** Sequence that calls the given function to produce elements *)

val cycle : 'a t -> 'a t
  (** Cycle forever through the given sequence. Assume the
      given sequence can be traversed any amount of times (not transient). *)

(** {2 Consume a sequence} *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Consume the sequence, passing all its arguments to the function *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** Iterate on elements and their index in the sequence *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold over elements of the sequence, consuming it *)

val foldi : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold over elements of the sequence and their index, consuming it *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** Map objects of the sequence into other elements, lazily *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  (** Map objects, along with their index in the sequence *)

val for_all : ('a -> bool) -> 'a t -> bool
  (** Do all elements satisfy the predicate? *)

val exists : ('a -> bool) -> 'a t -> bool
  (** Exists there some element satisfying the predicate? *)

val length : 'a t -> int
  (** How long is the sequence? *)

val is_empty : 'a t -> bool
  (** Is the sequence empty? *)

(** {2 Transform a sequence} *)

val filter : ('a -> bool) -> 'a t -> 'a t
  (** Filter on elements of the sequence *)

val append : 'a t -> 'a t -> 'a t
  (** Append two sequences *)

val concat : 'a t t -> 'a t
  (** Concatenate a sequence of sequences into one sequence *)

val flatMap : ('a -> 'b t) -> 'a t -> 'b t
  (** Monadic bind. It applies the function to every element of the
      initial sequence, and calls [concat]. *)

val intersperse : 'a t -> 'a -> 'a t
  (** Insert the second element between every element of the sequence *)

val persistent : 'a t -> 'a t
  (** Iterate on the sequence, storing elements in a data structure.
      The resulting sequence can be iterated on as many times as needed. *)

val sort : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
  (** Sort the sequence. Eager, O(n) ram and O(n ln(n)) time. *)

val sort_uniq : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
  (** Sort the sequence and remove duplicates. Eager, same as [sort] *)

val group : ?eq:('a -> 'a -> bool) -> 'a t -> 'a list t
  (** Group equal consecutive elements. *)

val uniq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
  (** Remove consecutive duplicate elements. Basically this is
      like [fun seq -> map List.hd (group seq)]. *)

val product : 'a t -> 'b t -> ('a * 'b) t
  (** Cartesian product of the sequences. The first one is transformed
      by calling [persistent] on it, so that it can be traversed
      several times (outer loop of the product) *)

val unfoldr : ('b -> ('a * 'b) option) -> 'b -> 'a t 
  (** [unfoldr f b] will apply [f] to [b]. If it
      yields [Some (x,b')] then [x] is returned
      and unfoldr recurses with [b']. *)

val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
  (** Sequence of intermediate results *)

val max : ?lt:('a -> 'a -> bool) -> 'a t -> 'a -> 'a
  (** Max element of the sequence, using the given comparison
      function. A default element has to be provided. *)

val min : ?lt:('a -> 'a -> bool) -> 'a t -> 'a -> 'a
  (** Min element of the sequence, using the given comparison function *)

val take : int -> 'a t -> 'a t
  (** Take at most [n] elements from the sequence *)

val drop : int -> 'a t -> 'a t
  (** Drop the [n] first elements of the sequence *)

val rev : 'a t -> 'a t
  (** Reverse the sequence. O(n) memory and time. *)

(** {2 Binary sequences} *)

val empty2 : ('a, 'b) t2

val is_empty2 : (_, _) t2 -> bool

val length2 : (_, _) t2 -> int

val zip : ('a, 'b) t2 -> ('a * 'b) t

val unzip : ('a * 'b) t -> ('a, 'b) t2

val zip_i : 'a t -> (int, 'a) t2
  (** Zip elements of the sequence with their index in the sequence *)

val fold2 : ('c -> 'a -> 'b -> 'c) -> 'c -> ('a, 'b) t2 -> 'c

val iter2 : ('a -> 'b -> unit) -> ('a, 'b) t2 -> unit

val map2 : ('a -> 'b -> 'c) -> ('a, 'b) t2 -> 'c t

val map2_2 : ('a -> 'b -> 'c) -> ('a -> 'b -> 'd) -> ('a, 'b) t2 -> ('c, 'd) t2
  (** [map2_2 f g seq2] maps each [x, y] of seq2 into [f x y, g x y] *)

(** {2 Basic data structures converters} *)

val to_list : 'a t -> 'a list

val to_rev_list : 'a t -> 'a list
  (** Get the list of the reversed sequence (more efficient) *)

val of_list : 'a list -> 'a t

val to_array : 'a t -> 'a array
  (** Convert to an array. Currently not very efficient because
      and intermediate list is used. *)

val of_array : 'a array -> 'a t

val of_array_i : 'a array -> (int * 'a) t
  (** Elements of the array, with their index *)

val of_array2 : 'a array -> (int, 'a) t2

val array_slice : 'a array -> int -> int -> 'a t
  (** [array_slice a i j] Sequence of elements whose indexes range
      from [i] to [j] *)

val of_stream : 'a Stream.t -> 'a t
  (** Sequence of elements of a stream (usable only once) *)

val to_stream : 'a t -> 'a Stream.t
  (** Convert to a stream. linear in memory and time (a copy is made in memory) *)

val to_stack : 'a Stack.t -> 'a t -> unit
  (** Push elements of the sequence on the stack *)

val of_stack : 'a Stack.t -> 'a t
  (** Sequence of elements of the stack (same order as [Stack.iter]) *)

val to_queue : 'a Queue.t -> 'a t -> unit
  (** Push elements of the sequence into the queue *)

val of_queue : 'a Queue.t -> 'a t
  (** Sequence of elements contained in the queue, FIFO order *)

val hashtbl_add : ('a, 'b) Hashtbl.t -> ('a * 'b) t -> unit
  (** Add elements of the sequence to the hashtable, with
      Hashtbl.add *)

val hashtbl_replace : ('a, 'b) Hashtbl.t -> ('a * 'b) t -> unit
  (** Add elements of the sequence to the hashtable, with
      Hashtbl.replace (erases conflicting bindings) *)

val to_hashtbl : ('a * 'b) t -> ('a, 'b) Hashtbl.t
  (** Build a hashtable from a sequence of key/value pairs *)

val to_hashtbl2 : ('a, 'b) t2 -> ('a, 'b) Hashtbl.t
  (** Build a hashtable from a sequence of key/value pairs *)

val of_hashtbl : ('a, 'b) Hashtbl.t -> ('a * 'b) t
  (** Sequence of key/value pairs from the hashtable *)

val of_hashtbl2 : ('a, 'b) Hashtbl.t -> ('a, 'b) t2
  (** Sequence of key/value pairs from the hashtable *)

val hashtbl_keys : ('a, 'b) Hashtbl.t -> 'a t
val hashtbl_values : ('a, 'b) Hashtbl.t -> 'b t

val of_str : string -> char t
val to_str :  char t -> string
val of_in_channel : in_channel -> char t

val to_buffer : char t -> Buffer.t -> unit
  (** Copy content of the sequence into the buffer *)

val int_range : start:int -> stop:int -> int t
  (** Iterator on integers in [start...stop] by steps 1 *)

val of_set : (module Set.S with type elt = 'a and type t = 'b) -> 'b -> 'a t
  (** Convert the given set to a sequence. The set module must be provided. *)

val to_set : (module Set.S with type elt = 'a and type t = 'b) -> 'a t -> 'b
  (** Convert the sequence to a set, given the proper set module *)

(** {2 Functorial conversions between sets and sequences} *)

module Set : sig
  module type S = sig
    type set
    include Set.S with type t := set
    val of_seq : elt t -> set
    val to_seq : set -> elt t
  end

  (** Create an enriched Set module from the given one *)
  module Adapt(X : Set.S) : S with type elt = X.elt and type set = X.t
    
  (** Functor to build an extended Set module from an ordered type *)
  module Make(X : Set.OrderedType) : S with type elt = X.t
end

(** {2 Conversion between maps and sequences.} *)

module Map : sig
  module type S = sig
    type +'a map
    include Map.S with type 'a t := 'a map
    val to_seq : 'a map -> (key * 'a) t
    val of_seq : (key * 'a) t -> 'a map
    val keys : 'a map -> key t
    val values : 'a map -> 'a t
  end

  (** Adapt a pre-existing Map module to make it sequence-aware *)
  module Adapt(M : Map.S) : S with type key = M.key and type 'a map = 'a M.t

  (** Create an enriched Map module, with sequence-aware functions *)
  module Make(V : Map.OrderedType) : S with type key = V.t
end

(** {2 Infinite sequences of random values} *)

val random_int : int -> int t

val random_bool : bool t

val random_float : float -> float t

val random_array : 'a array -> 'a t
  (** Sequence of choices of an element in the array *)

val random_list : 'a list -> 'a t

(** {2 Type-classes} *)

module TypeClass : sig
  (** {3 Classes} *)
  type ('a,'b) sequenceable = {
    to_seq : 'b -> 'a t;
    of_seq : 'a t -> 'b;
  }

  type ('a,'b) addable = {
    empty : 'b;
    add : 'b -> 'a -> 'b;
  }

  type 'a monoid = ('a,'a) addable

  type ('a,'b) iterable = {
    iter : ('a -> unit) -> 'b -> unit;
  }

  (** {3 Instances} *)

  val sequenceable : ('a,'a t) sequenceable
  val iterable : ('a,'a t) iterable
  val monoid : 'a t monoid

  (** {3 Conversions} *)

  val of_iterable : ('a,'b) iterable -> 'b -> 'a t
  val to_addable : ('a,'b) addable -> 'a t -> 'b
end

(** {2 Infix functions} *)

module Infix : sig
  val (--) : int -> int -> int t

  val (|>) : 'a -> ('a -> 'b) -> 'b

  val (@@) : 'a t -> 'a t -> 'a t
end

(** {2 Pretty printing of sequences} *)

val pp_seq : ?sep:string -> (Format.formatter -> 'a -> unit) ->
             Format.formatter -> 'a t -> unit
  (** Pretty print a sequence of ['a], using the given pretty printer
      to print each elements. An optional separator string can be provided. *)
