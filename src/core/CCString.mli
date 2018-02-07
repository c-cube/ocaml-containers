
(* This file is free software, part of containers. See file "license" for more details. *)

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
  (** Return the length (number of characters) of the given string. *)

  val blit : t -> int -> Bytes.t -> int -> int -> unit
  (** Similar to {!String.blit}.
      Compatible with the [-safe-string] option.
      @raise Invalid_argument if indices are not valid. *)

  (*
  val blit_immut : t -> int -> t -> int -> int -> string
  (** Immutable version of {!blit}, returning a new string.
      [blit a i b j len] is the same as [b], but in which
      the range [j, ..., j+len] is replaced by [a.[i], ..., a.[i + len]].
      @raise Invalid_argument if indices are not valid. *)
     *)

  val fold : ('a -> char -> 'a) -> 'a -> t -> 'a
  (** Fold on chars by increasing index.
      @since 0.7 *)

  (** {2 Conversions} *)

  val to_gen : t -> char gen
  (** Return the [gen] of characters contained in the string *)

  val to_seq : t -> char sequence
  (** Return the [sequence] of characters contained in the string *)

  val to_klist : t -> char klist
  (** Return the [klist] of characters contained in the string *)

  val to_list : t -> char list
  (** Return the list of characters contained in the string. *)

  val pp_buf : Buffer.t -> t -> unit
  (** Renamed from [pp].
      @since 2.0 *)

  val pp : Format.formatter -> t -> unit
  (** Print the string within quotes.
      Renamed from [print].
      @since 2.0 *)
end

(** {2 Strings} *)

include module type of String

val equal : string -> string -> bool
(** Equality function on strings. *)

val compare : string -> string -> int

val is_empty : string -> bool
(** [is_empty s] returns [true] iff [s] is empty (i.e. its length is 0).
    @since 1.5 *)

val hash : string -> int

val init : int -> (int -> char) -> string
(** Analog to [Array.init].
    @since 0.3.3 *)

val rev : string -> string
(** [rev s] returns the reverse of [s].
    @since 0.17 *)

val pad : ?side:[`Left|`Right] -> ?c:char -> int -> string -> string
(** [pad n str] ensures that [str] is at least [n] bytes long,
    and pads it on the [side] with [c] if it's not the case.
    @param side determines where padding occurs (default: [`Left]).
    @param c the char used to pad (default: ' ').
    @since 0.17 *)

val of_char : char -> string
(** [of_char 'a'] is ["a"].
    @since 0.19 *)

val of_gen : char gen -> string
(** Convert a [gen] of characters to a string. *)

val of_seq : char sequence -> string
(** Convert a [sequence] of characters to a string. *)

val of_klist : char klist -> string
(** Convert a [klist] of characters to a string. *)

val of_list : char list -> string
(** Convert a list of characters to a string. *)

val of_array : char array -> string
(** Convert an array of characters to a string. *)

val to_array : string -> char array
(** Return the array of characters contained in the string. *)

val find : ?start:int -> sub:string -> string -> int
(** Find [sub] in string, returns its first index or [-1]. *)

val find_all : ?start:int -> sub:string -> string -> int gen
(** [find_all ~sub s] finds all occurrences of [sub] in [s], even overlapping
    instances.
    @param start starting position in [s].
    @since 0.17 *)

val find_all_l : ?start:int -> sub:string -> string -> int list
(** [find_all_l ~sub s] finds all occurrences of [sub] in [s] and returns
    them in a list.
    @param start starting position in [s].
    @since 0.17 *)

val mem : ?start:int -> sub:string -> string -> bool
(** [mem ~sub s] is true iff [sub] is a substring of [s].
    @since 0.12 *)

val rfind : sub:string -> string -> int
(** Find [sub] in string from the right, returns its first index or [-1].
    Should only be used with very small [sub].
    @since 0.12 *)

val replace : ?which:[`Left|`Right|`All] -> sub:string -> by:string -> string -> string
(** [replace ~sub ~by s] replaces some occurrences of [sub] by [by] in [s].
    @param which decides whether the occurrences to replace are:
      {ul
        {- [`Left] first occurrence from the left (beginning)}
        {- [`Right] first occurrence from the right (end)}
        {- [`All] all occurrences (default)}
      }
    @raise Invalid_argument if [sub = ""].
    @since 0.14 *)

val is_sub : sub:string -> int -> string -> int -> len:int -> bool
(** [is_sub ~sub i s j ~len] returns [true] iff the substring of
    [sub] starting at position [i] and of length [len] is a substring
    of [s] starting at position [j]. *)

val repeat : string -> int -> string
(** The same string, repeated n times. *)

val prefix : pre:string -> string -> bool
(** [prefix ~pre s] returns [true] iff [pre] is a prefix of [s]. *)

val suffix : suf:string -> string -> bool
(** [suffix ~suf s] returns [true] iff [suf] is a suffix of [s].
    @since 0.7 *)

val chop_prefix : pre:string -> string -> string option
(** [chop_prefix ~pre s] removes [pre] from [s] if [pre] really is a prefix
    of [s], returns [None] otherwise.
    @since 0.17 *)

val chop_suffix : suf:string -> string -> string option
(** [chop_suffix ~suf s] removes [suf] from [s] if [suf] really is a suffix
    of [s], returns [None] otherwise.
    @since 0.17 *)

val take : int -> string -> string
(** [take n s] keeps only the [n] first chars of [s].
    @since 0.17 *)

val drop : int -> string -> string
(** [drop n s] removes the [n] first chars of [s].
    @since 0.17 *)

val take_drop : int -> string -> string * string
(** [take_drop n s = take n s, drop n s].
    @since 0.17 *)

val lines : string -> string list
(** [lines s] returns a list of the lines of [s] (splits along '\n').
    @since 0.10 *)

val lines_gen : string -> string gen
(** [lines_gen s] returns a generator of the lines of [s] (splits along '\n').
    @since 0.10 *)

val concat_gen : sep:string -> string gen -> string
(** [concat_gen ~sep g] concatenates all strings of [g], separated with [sep].
    @since 0.10 *)

val unlines : string list -> string
(** [unlines l] concatenates all strings of [l], separated with '\n'.
    @since 0.10 *)

val unlines_gen : string gen -> string
(** [unlines_gen g] concatenates all strings of [g], separated with '\n'.
    @since 0.10 *)

val set : string -> int -> char -> string
(** [set s i c] creates a new string which is a copy of [s], except
    for index [i], which becomes [c].
    @raise Invalid_argument if [i] is an invalid index.
    @since 0.12 *)

val iter : (char -> unit) -> string -> unit
(** Alias to {!String.iter}.
    @since 0.12 *)

val iteri : (int -> char -> unit) -> string -> unit
(** Iter on chars with their index.
    @since 0.12 *)

val map : (char -> char) -> string -> string
(** Map chars.
    @since 0.12 *)

val mapi : (int -> char -> char) -> string -> string
(** Map chars with their index.
    @since 0.12 *)

val filter_map : (char -> char option) -> string -> string
(** [filter_map f s] calls [(f a0) (f a1) ... (f an)] where [a0 ... an] are the characters of s.
    It returns the string of characters [ci] such as [f ai = Some ci] (when [f] returns [None],
    the corresponding element of [s] is discarded).
    @since 0.17 *)

val filter : (char -> bool) -> string -> string
(** [filter f s] discards characters not satisfying [f].
    @since 0.17 *)

val flat_map : ?sep:string -> (char -> string) -> string -> string
(** Map each chars to a string, then concatenates them all.
    @param sep optional separator between each generated string.
    @since 0.12 *)

val for_all : (char -> bool) -> string -> bool
(** True for all chars?
    @since 0.12 *)

val exists : (char -> bool) -> string -> bool
(** True for some char?
    @since 0.12 *)

include S with type t := string

val ltrim : t -> t
(** Trim space on the left (see {!String.trim} for more details).
    @since 1.2 *)

val rtrim : t -> t
(** Trim space on the right (see {!String.trim} for more details).
    @since 1.2 *)

(** {2 Operations on 2 strings} *)

val map2 : (char -> char -> char) -> string -> string -> string
(** Map pairs of chars.
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

val iter2: (char -> char -> unit) -> string -> string -> unit
(** Iterate on pairs of chars.
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

val iteri2: (int -> char -> char -> unit) -> string -> string -> unit
(** Iterate on pairs of chars with their index.
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

val fold2: ('a -> char -> char -> 'a) -> 'a -> string -> string -> 'a
(** Fold on pairs of chars.
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

val for_all2 : (char -> char -> bool) -> string -> string -> bool
(** All pairs of chars respect the predicate?
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

val exists2 : (char -> char -> bool) -> string -> string -> bool
(** Exists a pair of chars?
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

(** {2 Ascii functions}

    Those functions are deprecated in {!String} since 4.03, so we provide
    a stable alias for them even in older versions. *)

val capitalize_ascii : string -> string
(** See {!String}. @since 0.18 *)

val uncapitalize_ascii : string -> string
(** See {!String}. @since 0.18 *)

val uppercase_ascii : string -> string
(** See {!String}. @since 0.18 *)

val lowercase_ascii : string -> string
(** See {!String}. @since 0.18 *)

val equal_caseless : string -> string -> bool
(** Comparison without respect to {b ascii} lowercase.
    @since 1.2 *)

(** {2 Finding}

    A relatively efficient algorithm for finding sub-strings.
    @since 1.0 *)

module Find : sig
  type _ pattern

  val compile : string -> [ `Direct ] pattern

  val rcompile : string -> [ `Reverse ] pattern

  val find : ?start:int -> pattern:[`Direct] pattern -> string -> int
  (** Search for [pattern] in the string, left-to-right.
      @return the offset of the first match, -1 otherwise.
      @param start offset in string at which we start. *)

  val rfind : ?start:int -> pattern:[`Reverse] pattern -> string -> int
  (** Search for [pattern] in the string, right-to-left.
      @return the offset of the start of the first match from the right, -1 otherwise.
      @param start right-offset in string at which we start. *)
end

(** {2 Splitting} *)

module Split : sig
  (** Specification of what to do with empty blocks, as in [split ~by:"-" "-a-b-"].

      - [{first=false; last=false}] will return [""; "a"; "b"; ""].
      - [{first=true; last=false}] will return ["a"; "b" ""].
      - [{first=false; last=true}] will return [""; "a"; "b"].
      - [{first=true; last=true}] will return ["a"; "b"].

      The default value of all remaining functions is [Drop_none].
      @since 1.5
  *)
  type drop_if_empty = {
    first: bool;
    last: bool;
  }

  val no_drop : drop_if_empty
  (** Do not drop any group, even empty and on borders.
      @since 1.5 *)

  val list_ : ?drop:drop_if_empty -> by:string -> string -> (string*int*int) list
  (** Split the given string along the given separator [by]. Should only
      be used with very small separators, otherwise
      use {!Containers_string.KMP}.
      @return a list of slices [(s,index,length)] that are
      separated by [by]. {!String.sub} can then be used to actually extract
      a string from the slice.
      @raise Failure if [by = ""]. *)

  val gen : ?drop:drop_if_empty -> by:string -> string -> (string*int*int) gen

  val seq : ?drop:drop_if_empty -> by:string -> string -> (string*int*int) sequence

  val klist : ?drop:drop_if_empty -> by:string -> string -> (string*int*int) klist

  (** {6 Copying functions}

      Those split functions actually copy the substrings, which can be
      more convenient but less efficient in general. *)

  val list_cpy : ?drop:drop_if_empty -> by:string -> string -> string list

  val gen_cpy : ?drop:drop_if_empty -> by:string -> string -> string gen

  val seq_cpy : ?drop:drop_if_empty -> by:string -> string -> string sequence

  val klist_cpy : ?drop:drop_if_empty -> by:string -> string -> string klist

  val left : by:string -> string -> (string * string) option
  (** Split on the first occurrence of [by] from the leftmost part of
      the string.
      @since 0.12 *)

  val left_exn : by:string -> string -> string * string
  (** Split on the first occurrence of [by] from the leftmost part of the string.
      @raise Not_found if [by] is not part of the string.
      @since 0.16 *)

  val right : by:string -> string -> (string * string) option
  (** Split on the first occurrence of [by] from the rightmost part of
      the string.
      @since 0.12 *)

  val right_exn : by:string -> string -> string * string
  (** Split on the first occurrence of [by] from the rightmost part of the string.
      @raise Not_found if [by] is not part of the string.
      @since 0.16 *)

end

val split_on_char : char -> string -> string list
(** Split the string along the given char.
    @since 1.2 *)

val split : by:string -> string -> string list
(** Alias to {!Split.list_cpy}.
    @since 1.2 *)

(** {2 Utils} *)

val compare_versions : string -> string -> int
(** [compare_versions a b] compares {i version strings} [a] and [b],
    considering that numbers are above text.
    @since 0.13 *)

val compare_natural : string -> string -> int
(** Natural Sort Order, comparing chunks of digits as natural numbers.
    https://en.wikipedia.org/wiki/Natural_sort_order
    @since 1.3 *)

val edit_distance : string -> string -> int
(** Edition distance between two strings. This satisfies the classical
    distance axioms: it is always positive, symmetric, and satisfies
    the formula [distance a b + distance b c >= distance a c]. *)

(** {2 Slices} A contiguous part of a string *)

module Sub : sig
  type t = string * int * int
  (** A string, an offset, and the length of the slice. *)

  val make : string -> int -> len:int -> t

  val full : string -> t
  (** Full string. *)

  val copy : t -> string
  (** Make a copy of the substring. *)

  val underlying : t -> string

  val sub : t -> int -> int -> t
  (** Sub-slice. *)

  val get : t -> int -> char
  (** [get s i] gets the [i]-th element, or fails.
      @raise Invalid_argument if the index is not within [0 ... length - 1].
      @since 1.2 *)

  include S with type t := t

end
