
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Knuth-Morris-Pratt} *)

module type STRING = sig
  type t
  type char

  val length : t -> int
  val get : t -> int -> char
  val char_equal : char -> char -> bool
end

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit

module type S = sig
  type string

  type pattern
  (** Compiled pattern (needle: string to search in another string) *)

  val compile : string -> pattern
  (** Compile a string into a pattern *)

  val find : pattern:pattern -> string -> int -> int option
  (** [find ~pattern s i] finds the next occurrence of [pattern]
      in [s] starting at offset [i], and returns it,
      or returns [None] if the pattern doesn't occur. *)

  val search : pattern:pattern -> string -> int option
  (** [search ~pattern s] is a shortcut for [find ~pattern s 0]. *)

  val find_all : pattern:pattern -> string -> int -> int gen
  (** Generator on all occurrences of the pattern *)

  val seq : pattern:pattern -> string -> int -> int sequence
  (** iterate on matching positions *)

  (** {6 One-shot functions that compile the pattern on-the-fly} *)

  val search' : pattern:string -> string -> int option

  val find_all' : pattern:string -> string -> int gen

  val seq' : pattern:string -> string -> int sequence
end

module Make(Str : STRING) : S with type string = Str.t

include S with type string = string
