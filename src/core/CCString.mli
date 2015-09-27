
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

(** {1 Basic String Utils}

Consider using {!Containers_string.KMP} for pattern search, or Regex
libraries. *)

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

(** {2 Common Signature} *)

module type S = sig
  type t

  val length : t -> int

  val blit : t -> int -> Bytes.t -> int -> int -> unit
  (** Similar to {!String.blit}.
      Compatible with the [-safe-string] option.
      @raise Invalid_argument if indices are not valid *)

  (*
  val blit_immut : t -> int -> t -> int -> int -> string
  (** Immutable version of {!blit}, returning a new string.
      [blit a i b j len] is the same as [b], but in which
      the range [j, ..., j+len] is replaced by [a.[i], ..., a.[i + len]].
      @raise Invalid_argument if indices are not valid *)
     *)

  val fold : ('a -> char -> 'a) -> 'a -> t -> 'a
  (** Fold on chars by increasing index.
      @since 0.7 *)

  (** {2 Conversions} *)

  val to_gen : t -> char gen
  val to_seq : t -> char sequence
  val to_klist : t -> char klist
  val to_list : t -> char list

  val pp : Buffer.t -> t -> unit
  val print : Format.formatter -> t -> unit
end

(** {2 Strings} *)

val equal : string -> string -> bool

val compare : string -> string -> int

val hash : string -> int

val init : int -> (int -> char) -> string
(** Analog to [Array.init].
    @since 0.3.3 *)

(*$T
  init 3 (fun i -> [|'a'; 'b'; 'c'|].(i)) = "abc"
  init 0 (fun _ -> assert false) = ""
*)

val of_gen : char gen -> string
val of_seq : char sequence -> string
val of_klist : char klist -> string
val of_list : char list -> string
val of_array : char array -> string

(*$T
  of_list ['a'; 'b'; 'c'] = "abc"
  of_list [] = ""
*)

val to_array : string -> char array

val find : ?start:int -> sub:string -> string -> int
(** Find [sub] in string, returns its first index or [-1].
    Should only be used with very small [sub] *)

(*$T
  find ~sub:"bc" "abcd" = 1
  find ~sub:"bc" "abd" = ~-1
  find ~sub:"a" "_a_a_a_" = 1
*)

val mem : ?start:int -> sub:string -> string -> bool
(** [mem ~sub s] is true iff [sub] is a substring of [s]
    @since 0.12 *)

(*$T
   mem ~sub:"bc" "abcd"
   not (mem ~sub:"a b" "abcd")
*)

val rfind : sub:string -> string -> int
(** Find [sub] in string from the right, returns its first index or [-1].
    Should only be used with very small [sub]
    @since 0.12 *)

(*$T
  rfind ~sub:"bc" "abcd" = 1
  rfind ~sub:"bc" "abd" = ~-1
  rfind ~sub:"a" "_a_a_a_" = 5
  rfind ~sub:"bc" "abcdbcd" = 4
*)

val replace : ?which:[`Left|`Right|`All] -> sub:string -> by:string -> string -> string
(** [replace ~sub ~by s] replaces some occurrences of [sub] by [by] in [s]
    @param which decides whether the occurrences to replace are:
      {ul
        {il [`Left] first occurrence from the left (beginning)}
        {il [`Right] first occurrence from the right (end)}
        {il [`All] all occurrences (default)}
      }
    @since NEXT_RELEASE *)

(*$= & ~printer:CCFun.id
  (replace ~which:`All ~sub:"a" ~by:"b" "abcdabcd") "bbcdbbcd"
  (replace ~which:`Left ~sub:"a" ~by:"b" "abcdabcd") "bbcdabcd"
  (replace ~which:`Right ~sub:"a" ~by:"b" "abcdabcd") "abcdbbcd"
  (replace ~which:`All ~sub:"ab" ~by:"hello" "  abab cdabb a") \
    "  hellohello cdhellob a"
  (replace ~which:`Left ~sub:"ab" ~by:"nope" " a b c d ") " a b c d "
*)

val is_sub : sub:string -> int -> string -> int -> len:int -> bool
(** [is_sub ~sub i s j ~len] returns [true] iff the substring of
    [sub] starting at position [i] and of length [len] is a substring
    of [s] starting at position [j] *)

val repeat : string -> int -> string
(** The same string, repeated n times *)

val prefix : pre:string -> string -> bool
(** [prefix ~pre s] returns [true] iff [pre] is a prefix of [s] *)

(*$T
  prefix ~pre:"aab" "aabcd"
  not (prefix ~pre:"ab" "aabcd")
  not (prefix ~pre:"abcd" "abc")
*)

val suffix : suf:string -> string -> bool
(** [suffix ~suf s] returns [true] iff [suf] is a suffix of [s]
    @since 0.7 *)

(*$T
  suffix ~suf:"cd" "abcd"
  not (suffix ~suf:"cd" "abcde")
  not (suffix ~suf:"abcd" "cd")
*)

val lines : string -> string list
(** [lines s] returns a list of the lines of [s] (splits along '\n')
    @since 0.10 *)

val lines_gen : string -> string gen
(** [lines_gen s] returns a generator of the lines of [s] (splits along '\n')
    @since 0.10 *)

val concat_gen : sep:string -> string gen -> string
(** [concat_gen ~sep g] concatenates all strings of [g], separated with [sep].
    @since 0.10 *)

val unlines : string list -> string
(** [unlines l] concatenates all strings of [l], separated with '\n'
    @since 0.10 *)

val unlines_gen : string gen -> string
(** [unlines_gen g] concatenates all strings of [g], separated with '\n'
    @since 0.10 *)

(*$Q
  Q.printable_string (fun s -> unlines (lines s) = s)
*)

val set : string -> int -> char -> string
(** [set s i c] creates a new string which is a copy of [s], except
    for index [i], which becomes [c].
    @raise Invalid_argument if [i] is an invalid index
    @since 0.12 *)

(*$T
  set "abcd" 1 '_' = "a_cd"
  set "abcd" 0 '-' = "-bcd"
  (try ignore (set "abc" 5 '_'); false with Invalid_argument _ -> true)
*)

val iter : (char -> unit) -> string -> unit
(** Alias to {!String.iter}
    @since 0.12 *)

val iteri : (int -> char -> unit) -> string -> unit
(** iter on chars with their index
    @since 0.12 *)

val map : (char -> char) -> string -> string
(** map chars
    @since 0.12 *)

val mapi : (int -> char -> char) -> string -> string
(** map chars with their index
    @since 0.12 *)

val flat_map : ?sep:string -> (char -> string) -> string -> string
(** map each chars to a string, then concatenates them all
    @param sep optional separator between each generated string
    @since 0.12 *)

val for_all : (char -> bool) -> string -> bool
(** true for all chars?
    @since 0.12 *)

val exists : (char -> bool) -> string -> bool
(** true for some char?
    @since 0.12 *)

include S with type t := string

(** {2 Operations on 2 strings} *)

val map2 : (char -> char -> char) -> string -> string -> string
(** map pairs of chars
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

val iter2: (char -> char -> unit) -> string -> string -> unit
(** iterate on pairs of chars
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

val iteri2: (int -> char -> char -> unit) -> string -> string -> unit
(** iterate on pairs of chars with their index
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

val fold2: ('a -> char -> char -> 'a) -> 'a -> string -> string -> 'a
(** fold on pairs of chars
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

val for_all2 : (char -> char -> bool) -> string -> string -> bool
(** all pair of chars respect the predicate?
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

val exists2 : (char -> char -> bool) -> string -> string -> bool
(** exists a pair of chars?
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

(** {2 Splitting} *)

module Split : sig
  val list_ : by:string -> string -> (string*int*int) list
  (** split the given string along the given separator [by]. Should only
      be used with very small separators, otherwise
      use {!Containers_string.KMP}.
      @return a list of slices [(s,index,length)] that are
      separated by [by]. {!String.sub} can then be used to actually extract
      a string from the slice.
      @raise Failure if [by = ""] *)

  val gen : by:string -> string -> (string*int*int) gen

  val seq : by:string -> string -> (string*int*int) sequence

  val klist : by:string -> string -> (string*int*int) klist

  (** {6 Copying functions}

  Those split functions actually copy the substrings, which can be
  more convenient but less efficient in general *)

  val list_cpy : by:string -> string -> string list

  (*$T
    Split.list_cpy ~by:"," "aa,bb,cc" = ["aa"; "bb"; "cc"]
    Split.list_cpy ~by:"--" "a--b----c--" = ["a"; "b"; ""; "c"; ""]
    Split.list_cpy ~by:" " "hello  world aie" = ["hello"; ""; "world"; "aie"]
  *)

  val gen_cpy : by:string -> string -> string gen

  val seq_cpy : by:string -> string -> string sequence

  val klist_cpy : by:string -> string -> string klist

  val left : by:string -> string -> (string * string) option
  (** Split on the first occurrence of [by] from the left-most part of
      the string
      @since 0.12 *)

  (*$T
    Split.left ~by:" " "ab cde f g " = Some ("ab", "cde f g ")
    Split.left ~by:"_" "abcde" = None
  *)

  val right : by:string -> string -> (string * string) option
  (** Split on the first occurrence of [by] from the rightmost part of
      the string
      @since 0.12 *)

  (*$T
    Split.right ~by:" " "ab cde f g" = Some ("ab cde f", "g")
    Split.right ~by:"_" "abcde" = None
  *)
end

(** {2 Utils} *)

val compare_versions : string -> string -> int
(** [compare_versions a b] compares {i version strings} [a] and [b],
    considering that numbers are above text.
    @since 0.13 *)

(*$T
  compare_versions "0.1.3" "0.1" > 0
  compare_versions "10.1" "2.0" > 0
  compare_versions "0.1.alpha" "0.1" > 0
  compare_versions "0.3.dev" "0.4" < 0
  compare_versions "0.foo" "0.0" < 0
  compare_versions "1.2.3.4" "01.2.4.3" < 0
*)

(** {2 Slices} A contiguous part of a string *)

module Sub : sig
  type t = string * int * int
  (** A string, an offset, and the length of the slice *)

  val make : string -> int -> len:int -> t

  val full : string -> t
  (** Full string *)

  val copy : t -> string
  (** Make a copy of the substring *)

  val underlying : t -> string

  val sub : t -> int -> int -> t
  (** Sub-slice *)

  include S with type t := t

  (*$T
    let s = Sub.make "abcde" 1 3 in \
      Sub.fold (fun acc x -> x::acc) [] s = ['d'; 'c'; 'b']
    Sub.make "abcde" 1 3 |> Sub.copy = "bcd"
    Sub.full "abcde" |> Sub.copy = "abcde"
  *)
end
