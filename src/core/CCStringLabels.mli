
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic String Utils} *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a gen = unit -> 'a option

include module type of struct include StringLabels end
(** {{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/StringLabels.html} Documentation for the standard StringLabels module} *)

val length : t -> int
(** [length s] returns the length (number of characters) of the given string [s]. *)

val blit : src:t -> src_pos:int -> dst:Bytes.t -> dst_pos:int -> len:int -> unit
(** [blit ~src ~src_pos ~dst ~dst_pos ~len] copies [len] characters from string [src] starting at character indice [src_pos],
    to the Bytes sequence [dst] starting at character indice [dst_pos].
    Like {!String.blit}.
    Compatible with the [-safe-string] option.
    @raise Invalid_argument if indices are not valid. *)

(*
val blit_immut : t -> int -> t -> int -> int -> string
(** Immutable version of {!blit}, returning a new string.
    [blit a i b j len] is the same as [b], but in which
    the range [j, …, j+len] is replaced by [a.[i], …, a.[i + len]].
    @raise Invalid_argument if indices are not valid. *)
   *)

val fold : f:('a -> char -> 'a) -> init:'a -> t -> 'a
(** [fold ~f ~init s] folds on chars by increasing index. Computes [f(… (f (f init s.[0]) s.[1]) …) s.[n-1]].
    @since 0.7 *)

(** {2 Conversions} *)

val to_gen : t -> char gen
(** [to_gen s] returns the [gen] of characters contained in the string [s]. *)

val to_iter : t -> char iter
(** [to_iter s] returns the [iter] of characters contained in the string [s].
    @since 2.8 *)

val to_seq : t -> char Seq.t
(** [to_seq s] returns the [Seq.t] of characters contained in the string [s].
    Renamed from [to std_seq] since 3.0.
    @since 3.0 *)

val to_list : t -> char list
(** [to_list s] returns the [list] of characters contained in the string [s]. *)

val pp_buf : Buffer.t -> t -> unit
(** [pp_buf buf s] prints [s] to the buffer [buf].
    Renamed from [pp] since 2.0. *)

val pp : Format.formatter -> t -> unit
(** [pp f s] prints the string [s] within quotes to the formatter [f].
    Renamed from [print] since 2.0. *)

(** {2 Strings} *)

val equal : string -> string -> bool
(** [equal s1 s2] returns [true] iff the strings [s1] and [s2] are equal. *)

val compare : string -> string -> int
(** [compare s1 s2] compares the strings [s1] and [s2] and returns an integer that indicates
    their relative position in the sort order. *)

val is_empty : string -> bool
(** [is_empty s] returns [true] iff [s] is empty (i.e. its length is 0).
    @since 1.5 *)

val hash : string -> int
(** [hash s] returns the hash value of [s]. *)

val rev : string -> string
(** [rev s] returns the reverse of [s].
    @since 0.17 *)

val pad : ?side:[`Left|`Right] -> ?c:char -> int -> string -> string
(** [pad ?side ?c n s] ensures that the string [s] is at least [n] bytes long,
    and pads it on the [side] with [c] if it's not the case.
    @param side determines where padding occurs (default: [`Left]).
    @param c the char used to pad (default: ' ').
    @since 0.17 *)

val of_char : char -> string
(** [of_char 'a'] is ["a"].
    @since 0.19 *)

val of_gen : char gen -> string
(** [of_gen gen] converts a [gen] of characters to a string. *)

val of_iter : char iter -> string
(** [of_iter iter] converts an [iter] of characters to a string.
    @since 2.8 *)

val of_seq : char Seq.t -> string
(** [of_seq seq] converts a [seq] of characters to a string.
    Renamed from [of_std_seq] since 3.0.
    @since 3.0 *)

val of_list : char list -> string
(** [of_list lc] converts a list of characters [lc] to a string. *)

val of_array : char array -> string
(** [of_array ac] converts an array of characters [ac] to a string. *)

val to_array : string -> char array
(** [to_array s] returns the array of characters contained in the string [s]. *)

val find : ?start:int -> sub:(string [@keep_label]) -> string -> int
(** [find ?start ~sub s] returns the starting index of the first occurrence of [sub] within [s] or [-1]. 
    @param start starting position in [s]. *)

val find_all : ?start:int -> sub:(string [@keep_label]) -> string -> int gen
(** [find_all ?start ~sub s] finds all occurrences of [sub] in [s], even overlapping instances
    and returns them in a generator [gen].
    @param start starting position in [s].
    @since 0.17 *)

val find_all_l : ?start:int -> sub:(string [@keep_label]) -> string -> int list
(** [find_all_l ?start ~sub s] finds all occurrences of [sub] in [s]
    and returns them in a list.
    @param start starting position in [s].
    @since 0.17 *)

val mem : ?start:int -> sub:(string [@keep_label]) -> string -> bool
(** [mem ?start ~sub s] is [true] iff [sub] is a substring of [s].
    @since 0.12 *)

val rfind : sub:(string [@keep_label]) -> string -> int
(** [rfind ~sub s] finds [sub] in string [s] from the right, returns its first index or [-1].
    Should only be used with very small [sub].
    @since 0.12 *)

val replace : ?which:[`Left|`Right|`All] -> sub:(string [@keep_label]) -> by:(string [@keep_label]) -> string -> string
(** [replace ?which ~sub ~by s] replaces some occurrences of [sub] by [by] in [s].
    @param which decides whether the occurrences to replace are:
      {ul
        {- [`Left] first occurrence from the left (beginning).}
        {- [`Right] first occurrence from the right (end).}
        {- [`All] all occurrences (default).}
      }
    @raise Invalid_argument if [sub = ""].
    @since 0.14 *)

val is_sub : sub:(string [@keep_label]) -> sub_pos:int -> string -> pos:int -> sub_len:(int [@keep_label]) -> bool
(** [is_sub ~sub ~sub_pos s ~pos ~sub_len] returns [true] iff the substring of [sub]
    starting at position [sub_pos] and of length [sub_len] is a substring of [s]
    starting at position [pos]. *)

val repeat : string -> int -> string
(** [repeat s n] creates a string by repeating the string [s] [n] times. *)

val prefix : pre:(string [@keep_label]) -> string -> bool
(** [prefix ~pre s] returns [true] iff [pre] is a prefix of [s]. *)

val suffix : suf:(string [@keep_label]) -> string -> bool
(** [suffix ~suf s] returns [true] iff [suf] is a suffix of [s].
    @since 0.7 *)

val chop_prefix : pre:(string [@keep_label]) -> string -> string option
(** [chop_prefix ~pre s] removes [pre] from [s] if [pre] really is a prefix
    of [s], returns [None] otherwise.
    @since 0.17 *)

val chop_suffix : suf:(string [@keep_label]) -> string -> string option
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
(** [take_drop n s] is [take n s, drop n s].
    @since 0.17 *)

val lines : string -> string list
(** [lines s] returns a list of the lines of [s] (splits along '\n').
    @since 0.10 *)

val lines_gen : string -> string gen
(** [lines_gen s] returns a generator [gen] of the lines of [s] (splits along '\n').
    @since 0.10 *)

val lines_iter : string -> string iter
(** [lines_iter s] returns the [iter] of the lines of [s] (splits along '\n').
    @since 3.2 *)

val lines_seq : string -> string Seq.t
(** [lines_seq s] returns the [Seq.t] of the lines of [s] (splits along '\n').
    @since 3.2 *)

val concat_iter : sep:string -> string iter -> string
(** [concat_iter ~sep iter] concatenates all strings of [iter], separated with [sep].
    @since 3.2 *)

val concat_gen : sep:(string [@keep_label]) -> string gen -> string
(** [concat_gen ~sep gen] concatenates all strings of [gen], separated with [sep].
    @since 0.10 *)

val concat_seq : sep:string -> string Seq.t -> string
(** [concat_seq ~sep seq] concatenates all strings of [seq], separated with [sep].
    @since 3.2 *)

val unlines : string list -> string
(** [unlines ls] concatenates all strings of [ls], separated with '\n'.
    @since 0.10 *)

val unlines_gen : string gen -> string
(** [unlines_gen gen] concatenates all strings of [gen], separated with '\n'.
    @since 0.10 *)

val unlines_iter : string iter -> string
(** [unlines_iter iter] concatenates all strings of [iter], separated with '\n'.
    @since 3.2 *)

val unlines_seq : string Seq.t -> string
(** [unlines_seq seq] concatenates all strings of [seq], separated with '\n'.
    @since 3.2 *)

val set : string -> int -> char -> string
(** [set s i c] creates a new string which is a copy of [s], except
    for index [i], which becomes [c].
    @raise Invalid_argument if [i] is an invalid index.
    @since 0.12 *)

val iter : f:(char -> unit) -> string -> unit
(** [iter ~f s] applies function [f] on each character of [s].
    Alias to {!String.iter}.
    @since 0.12 *)

val filter_map : f:(char -> char option) -> string -> string
(** [filter_map ~f s] calls [(f a0) (f a1) … (f an)] where [a0 … an] are the characters of s.
    It returns the string of characters [ci] such as [f ai = Some ci] (when [f] returns [None],
    the corresponding element of [s] is discarded).
    @since 0.17 *)

val filter : f:(char -> bool) -> string -> string
(** [filter ~f s] discards characters of [s] not satisfying [f].
    @since 0.17 *)

val flat_map : ?sep:string -> f:(char -> string) -> string -> string
(** [flat_map ?sep ~f s] maps each chars of [s] to a string, then concatenates them all.
    @param sep optional separator between each generated string.
    @since 0.12 *)

val for_all : f:(char -> bool) -> string -> bool
(** [for_all ~f s] is [true] iff all characters of [s] satisfy the predicate [f].
    @since 0.12 *)

val exists : f:(char -> bool) -> string -> bool
(** [exists ~f s] is [true] iff some character of [s] satisfy the predicate [f].
    @since 0.12 *)

val drop_while : f:(char -> bool) -> t -> t
(** [drop_while ~f s] discards any characters of [s] starting from the left,
    up to the first character [c] not satisfying [f c].
    @since 2.2 *)

val rdrop_while : f:(char -> bool) -> t -> t
(** [rdrop_while ~f s] discards any characters of [s] starting from the right,
    up to the first character [c] not satisfying [f c].
    @since 2.2 *)

val ltrim : t -> t
(** [ltrim s] trims space on the left (see {!String.trim} for more details).
    @since 1.2 *)

val rtrim : t -> t
(** [rtrim s] trims space on the right (see {!String.trim} for more details).
    @since 1.2 *)

(** {2 Operations on 2 strings} *)

val map2 : f:(char -> char -> char) -> string -> string -> string
(** [map2 ~f s1 s2] maps pairs of chars.
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

val iter2: f:(char -> char -> unit) -> string -> string -> unit
(** [iter2 ~f s1 s2] iterates on pairs of chars.
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

val iteri2: f:(int -> char -> char -> unit) -> string -> string -> unit
(** [iteri2 ~f s1 s2] iterates on pairs of chars with their index.
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

val fold2: f:('a -> char -> char -> 'a) -> init:'a -> string -> string -> 'a
(** [fold2 ~f ~init s1 s2] folds on pairs of chars.
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

val for_all2 : f:(char -> char -> bool) -> string -> string -> bool
(** [for_all2 ~f s1 s2] returns [true] iff all pairs of chars satisfy the predicate [f].
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

val exists2 : f:(char -> char -> bool) -> string -> string -> bool
(** [exists2 ~f s1 s2] returns [true] iff a pair of chars satisfy the predicate [f].
    @raise Invalid_argument if the strings have not the same length.
    @since 0.12 *)

(** {2 Ascii functions}

    Those functions are deprecated in {!String} since 4.03, so we provide
    a stable alias for them even in older versions. *)

val capitalize_ascii : string -> string
(** [capitalize_ascii s] returns a copy of [s] with the first character set to uppercase
    using the US-ASCII character set.
    See {!String}.
    @since 0.18 *)

val uncapitalize_ascii : string -> string
(** [uncapitalize_ascii s] returns a copy of [s] with the first character set to lowercase
    using the US-ASCII character set.
    See {!String}.
    @since 0.18 *)

val uppercase_ascii : string -> string
(** [uppercase_ascii s] returns a copy of [s] with all lowercase letters translated to uppercase
    using the US-ASCII character set.
    See {!String}.
    @since 0.18 *)

val lowercase_ascii : string -> string
(** [lowercase_ascii s] returns a copy of [s] with all uppercase letters translated to lowercase
    using the US-ASCII character set.
    See {!String}.
    @since 0.18 *)

val equal_caseless : string -> string -> bool
(** [equal_caseless s1 s2] compares [s1] and [s2] without respect to {b ascii} lowercase.
    @since 1.2 *)

(** {2 Finding}

    A relatively efficient algorithm for finding sub-strings.
    @since 1.0 *)

module Find : sig
  type _ pattern

  val compile : string -> [ `Direct ] pattern

  val rcompile : string -> [ `Reverse ] pattern

  val find : ?start:int -> pattern:(([`Direct] pattern) [@keep_label]) -> string -> int
  (** [find ?start ~pattern s] searches for [pattern] in the string [s], left-to-right.
      @return the offset of the first match, -1 otherwise.
      @param start offset in string at which we start. *)

  val rfind : ?start:int -> pattern:(([`Reverse] pattern) [@keep_label]) -> string -> int
  (** [rfind ?start ~pattern s] searches for [pattern] in the string [s], right-to-left.
      @return the offset of the start of the first match from the right, -1 otherwise.
      @param start right-offset in string at which we start. *)
end

(** {2 Splitting} *)

module Split : sig
  (** Specification of what to do with empty blocks, as in [split ~by:"-" "-a-b-"].

      - [{first=false; last=false}] will return [""; "a"; "b"; ""]
      - [{first=true; last=false}] will return ["a"; "b" ""]
      - [{first=false; last=true}] will return [""; "a"; "b"]
      - [{first=true; last=true}] will return ["a"; "b"]

      The default value of all remaining functions is [Drop_none].
      @since 1.5
  *)
  type drop_if_empty = {
    first: bool;
    last: bool;
  }

  val no_drop : drop_if_empty
  (** [no_drop] does not drop any group, even empty and on borders.
      @since 1.5 *)

  val list_ : ?drop:drop_if_empty -> by:(string [@keep_label]) -> string -> (string*int*int) list
  (** [list_ ?drop ~by s] splits the given string [s] along the given separator [by].
      Should only be used with very small separators, otherwise use {!Containers_string.KMP}.
      @return a [list] of slices [(s,index,length)] that are
      separated by [by]. {!String.sub} can then be used to actually extract
      a string from the slice.
      @raise Failure if [by = ""]. *)

  val gen : ?drop:drop_if_empty -> by:string -> string -> (string*int*int) gen
  (** [gen ?drop ~by s] splits the given string [s] along the given separator [by].
      Returns a [gen] of slices. *)

  val iter : ?drop:drop_if_empty -> by:string -> string -> (string*int*int) iter
  (** [iter ?drop ~by s] splits the given string [s] along the given separator [by].
      Returns an [iter] of slices.
      @since 2.8 *)

  val seq : ?drop:drop_if_empty -> by:string -> string -> (string*int*int) Seq.t
  (** [seq ?drop ~by s] splits the given string [s] along the given separator [by].
      Returns a [Seq.t] of slices.
      Renamed from [std_seq] since 3.0.
      @since 3.0 *)

  (** {4 Copying functions}

      Those split functions actually copy the substrings, which can be
      more convenient but less efficient in general. *)

  val list_cpy : ?drop:drop_if_empty -> by:string -> string -> string list
  (** [list_cpy ?drop ~by s] splits the given string [s] along the given separator [by].
      Returns a [list] of strings. *)

  val gen_cpy : ?drop:drop_if_empty -> by:string -> string -> string gen
 (** [gen_cpy ?drop ~by s] splits the given string [s] along the given separator [by].
      Returns a [gen] of strings. *)

  val iter_cpy : ?drop:drop_if_empty -> by:string -> string -> string iter
  (** [iter_cpy ?drop ~by s] splits the given string [s] along the given separator [by].
      Returns an [iter] of strings.
      @since 2.8 *)

  val seq_cpy : ?drop:drop_if_empty -> by:string -> string -> string Seq.t
  (** [seq_cpy ?drop ~by s] splits the given string [s] along the given separator [by].
      Returns a [Seq.t] of strings.
      Renamed from [std_seq_cpy] since 3.0.
      @since 3.0 *)

  val left : by:(string [@keep_label]) -> string -> (string * string) option
  (** [left ~by s] splits on the first occurrence of [by] from the leftmost part
      of the string [s].
      @since 0.12 *)

  val left_exn : by:(string [@keep_label]) -> string -> string * string
  (** [left_exn ~by s] splits on the first occurrence of [by] from the leftmost part
      of the string [s].
      @raise Not_found if [by] is not part of the string [s].
      @since 0.16 *)

  val right : by:(string [@keep_label]) -> string -> (string * string) option
  (** [right ~by s] splits on the first occurrence of [by] from the rightmost part
      of the string [s].
      @since 0.12 *)

  val right_exn : by:(string [@keep_label]) -> string -> string * string
  (** [right_exn ~by s] splits on the first occurrence of [by] from the rightmost part
      of the string [s].
      @raise Not_found if [by] is not part of the string [s].
      @since 0.16 *)

end

val split_on_char : by:char -> string -> string list
(** [split_on_char ~by s] splits the string [s] along the given char [by].
    @since 1.2 *)

val split : by:(string [@keep_label]) -> string -> string list
(** [split ~by s] splits the string [s] along the given string [by].
    Alias to {!Split.list_cpy}.
    @since 1.2 *)

(** {2 Utils} *)

val compare_versions : string -> string -> int
(** [compare_versions s1 s2] compares {i version strings} [s1] and [s2],
    considering that numbers are above text.
    @since 0.13 *)

val compare_natural : string -> string -> int
(** [compare_natural s1 s2] is the Natural Sort Order, comparing chunks of digits as natural numbers.
    https://en.wikipedia.org/wiki/Natural_sort_order
    @since 1.3 *)

val edit_distance : ?cutoff:int -> string -> string -> int
(** [edit_distance ?cutoff s1 s2] is the edition distance between the two strings [s1] and [s2].
    This satisfies the classical distance axioms: it is always positive, symmetric, and satisfies
    the formula [distance s1 s2 + distance s2 s3 >= distance s1 s3].
    @param cutoff if provided, it's a cap on both the number of iterations,
      and on the result. (since 3.0). This is useful if you just want to
    check whether the edit distance is less or equal than 2 (use cutoff of 3).
*)

(** {2 Infix operators}

    @since 3.0 *)

module Infix : sig
  val (=) : t -> t -> bool
  (** @since 3.0 *)

  val (<>) : t -> t -> bool
  (** @since 3.0 *)

  val (<) : t -> t -> bool
  (** @since 3.0 *)

  val (<=) : t -> t -> bool
  (** @since 3.0 *)

  val (>=) : t -> t -> bool
  (** @since 3.0 *)

  val (>) : t -> t -> bool
  (** @since 3.0 *)
end

include module type of Infix
