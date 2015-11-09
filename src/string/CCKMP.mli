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
