
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
  (** Print the string within quotes *)
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

val rev : string -> string
(** [rev s] returns the reverse of [s]
    @since 0.17 *)

(*$Q
  Q.printable_string (fun s -> s = rev (rev s))
  Q.printable_string (fun s -> length s = length (rev s))
*)

(*$=
  "abc" (rev "cba")
  "" (rev "")
  " " (rev " ")
*)

val pad : ?side:[`Left|`Right] -> ?c:char -> int -> string -> string
(** [pad n str] ensures that [str] is at least [n] bytes long,
    and pads it on the [side] with [c] if it's not the case.
    @param side determines where padding occurs (default: [`Left])
    @param c the char used to pad (default: ' ')
    @since 0.17 *)

(*$= & ~printer:Q.Print.string
  "  42" (pad 4 "42")
  "0042" (pad ~c:'0' 4 "42")
  "4200" (pad ~side:`Right ~c:'0' 4 "42")
  "hello" (pad 4 "hello")
  "aaa" (pad ~c:'a' 3 "")
  "aaa" (pad ~side:`Right ~c:'a' 3 "")
*)

val of_char : char -> string
(** [of_char 'a' = "a"]
    @since 0.19 *)

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

(*$= & ~printer:string_of_int
  1 (find ~sub:"bc" "abcd")
  ~-1 (find ~sub:"bc" "abd")
  1 (find ~sub:"a" "_a_a_a_")
  6 (find ~sub:"a" ~start:5 "a1a234a")
*)

(*$Q & ~count:10_000
  Q.(pair printable_string printable_string) (fun (s1,s2) -> \
    let i = find ~sub:s2 s1 in \
    i < 0 || String.sub s1 i (length s2) = s2)
*)

val find_all : ?start:int -> sub:string -> string -> int gen
(** [find_all ~sub s] finds all occurrences of [sub] in [s], even overlapping
    instances.
    @param start starting position in [s]
    @since 0.17 *)

val find_all_l : ?start:int -> sub:string -> string -> int list
(** [find_all ~sub s] finds all occurrences of [sub] in [s] and returns
    them in a list
    @param start starting position in [s]
    @since 0.17 *)

(*$= & ~printer:Q.Print.(list int)
  [1; 6] (find_all_l ~sub:"bc" "abc aabc  aab")
  [] (find_all_l ~sub:"bc" "abd")
  [76] (find_all_l ~sub:"aaaaaa" \
    "aabbaabbaaaaabbbbabababababbbbabbbabbaaababbbaaabaabbaabbaaaabbababaaaabbaabaaaaaabbbaaaabababaabaaabbaabaaaabbababbaabbaaabaabbabababbbaabababaaabaaababbbaaaabbbaabaaababbabaababbaabbaaaaabababbabaababbbaaabbabbabababaaaabaaababaaaaabbabbaabbabbbbbbbbbbbbbbaabbabbbbbabbaaabbabbbbabaaaaabbababbbaaaa")
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

(*$= & ~printer:string_of_int
  1 (rfind ~sub:"bc" "abcd")
  ~-1 (rfind ~sub:"bc" "abd")
  5 (rfind ~sub:"a" "_a_a_a_")
  4 (rfind ~sub:"bc" "abcdbcd")
  6 (rfind ~sub:"a" "a1a234a")
*)

(*$Q & ~count:10_000
  Q.(pair printable_string printable_string) (fun (s1,s2) -> \
    let i = rfind ~sub:s2 s1 in \
    i < 0 || String.sub s1 i (length s2) = s2)
*)

val replace : ?which:[`Left|`Right|`All] -> sub:string -> by:string -> string -> string
(** [replace ~sub ~by s] replaces some occurrences of [sub] by [by] in [s]
    @param which decides whether the occurrences to replace are:
      {ul
        {- [`Left] first occurrence from the left (beginning)}
        {- [`Right] first occurrence from the right (end)}
        {- [`All] all occurrences (default)}
      }
    @raise Invalid_argument if [sub = ""]
    @since 0.14 *)

(*$= & ~printer:CCFun.id
  (replace ~which:`All ~sub:"a" ~by:"b" "abcdabcd") "bbcdbbcd"
  (replace ~which:`Left ~sub:"a" ~by:"b" "abcdabcd") "bbcdabcd"
  (replace ~which:`Right ~sub:"a" ~by:"b" "abcdabcd") "abcdbbcd"
  (replace ~which:`All ~sub:"ab" ~by:"hello" "  abab cdabb a") \
    "  hellohello cdhellob a"
  (replace ~which:`Left ~sub:"ab" ~by:"nope" " a b c d ") " a b c d "
  (replace ~sub:"a" ~by:"b" "1aa234a") "1bb234b"
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

val chop_prefix : pre:string -> string -> string option
(** [chop_pref ~pre s] removes [pre] from [s] if [pre] really is a prefix
    of [s], returns [None] otherwise
    @since 0.17 *)

(*$= & ~printer:Q.Print.(option string)
  (Some "cd") (chop_prefix ~pre:"aab" "aabcd")
  None (chop_prefix ~pre:"ab" "aabcd")
  None (chop_prefix ~pre:"abcd" "abc")
*)

val chop_suffix : suf:string -> string -> string option
(** [chop_suffix ~suf s] removes [suf] from [s] if [suf] really is a suffix
    of [s], returns [None] otherwise
    @since 0.17 *)

(*$= & ~printer:Q.Print.(option string)
  (Some "ab") (chop_suffix ~suf:"cd" "abcd")
  None (chop_suffix ~suf:"cd" "abcde")
  None (chop_suffix ~suf:"abcd" "cd")
*)

val take : int -> string -> string
(** [take n s] keeps only the [n] first chars of [s]
    @since 0.17 *)

val drop : int -> string -> string
(** [drop n s] removes the [n] first chars of [s]
    @since 0.17 *)

val take_drop : int -> string -> string * string
(** [take_drop n s = take n s, drop n s]
    @since 0.17 *)

(*$=
  ("ab", "cd") (take_drop 2 "abcd")
  ("abc", "") (take_drop 3 "abc")
  ("abc", "") (take_drop 5 "abc")
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
  Q.printable_string (fun s -> unlines_gen (lines_gen s) = s)
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
(** Iter on chars with their index
    @since 0.12 *)

val map : (char -> char) -> string -> string
(** Map chars
    @since 0.12 *)

val mapi : (int -> char -> char) -> string -> string
(** Map chars with their index
    @since 0.12 *)

val filter_map : (char -> char option) -> string -> string
(** @since 0.17 *)

(*$= & ~printer:Q.Print.string
  "bcef" (filter_map \
     (function 'c' -> None | c -> Some (Char.chr (Char.code c + 1))) "abcde")
*)

val filter : (char -> bool) -> string -> string
(** @since 0.17 *)

(*$= & ~printer:Q.Print.string
  "abde" (filter (function 'c' -> false | _ -> true) "abcdec")
*)

(*$Q
  Q.printable_string (fun s -> filter (fun _ -> true) s = s)
*)

val flat_map : ?sep:string -> (char -> string) -> string -> string
(** Map each chars to a string, then concatenates them all
    @param sep optional separator between each generated string
    @since 0.12 *)

val for_all : (char -> bool) -> string -> bool
(** True for all chars?
    @since 0.12 *)

val exists : (char -> bool) -> string -> bool
(** True for some char?
    @since 0.12 *)

include S with type t := string

(** {2 Operations on 2 strings} *)

val map2 : (char -> char -> char) -> string -> string -> string
(** Map pairs of chars
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

val iter2: (char -> char -> unit) -> string -> string -> unit
(** Iterate on pairs of chars
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

val iteri2: (int -> char -> char -> unit) -> string -> string -> unit
(** Iterate on pairs of chars with their index
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

val fold2: ('a -> char -> char -> 'a) -> 'a -> string -> string -> 'a
(** Fold on pairs of chars
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

val for_all2 : (char -> char -> bool) -> string -> string -> bool
(** All pairs of chars respect the predicate?
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

val exists2 : (char -> char -> bool) -> string -> string -> bool
(** Exists a pair of chars?
    @raise Invalid_argument if the strings have not the same length
    @since 0.12 *)

(** {2 Ascii functions}

    Those functions are deprecated in {!String} since 4.03, so we provide
    a stable alias for them even in older versions *)

val capitalize_ascii : string -> string
(** See {!String}. @since 0.18 *)

val uncapitalize_ascii : string -> string
(** See {!String}. @since 0.18 *)

val uppercase_ascii : string -> string
(** See {!String}. @since 0.18 *)

val lowercase_ascii : string -> string
(** See {!String}. @since 0.18 *)


(** {2 Splitting} *)

module Split : sig
  val list_ : by:string -> string -> (string*int*int) list
  (** Eplit the given string along the given separator [by]. Should only
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
  (** Split on the first occurrence of [by] from the leftmost part of
      the string
      @since 0.12 *)

  val left_exn : by:string -> string -> string * string
  (** Split on the first occurrence of [by] from the leftmost part of the string
      @raise Not_found if [by] is not part of the string
      @since 0.16 *)

  (*$T
    Split.left ~by:" " "ab cde f g " = Some ("ab", "cde f g ")
    Split.left ~by:"__" "a__c__e_f" = Some ("a", "c__e_f")
    Split.left ~by:"_" "abcde" = None
    Split.left ~by:"bb" "abbc" = Some ("a", "c")
    Split.left ~by:"a_" "abcde" = None
  *)

  val right : by:string -> string -> (string * string) option
  (** Split on the first occurrence of [by] from the rightmost part of
      the string
      @since 0.12 *)

  val right_exn : by:string -> string -> string * string
  (** Split on the first occurrence of [by] from the rightmost part of the string
      @raise Not_found if [by] is not part of the string
      @since 0.16 *)

  (*$T
    Split.right ~by:" " "ab cde f g" = Some ("ab cde f", "g")
    Split.right ~by:"__" "a__c__e_f" = Some ("a__c", "e_f")
    Split.right ~by:"_" "abcde" = None
    Split.right ~by:"a_" "abcde" = None
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

(*$Q
  Q.(pair printable_string printable_string) (fun (a,b) -> \
    CCOrd.equiv (compare_versions a b) (CCOrd.opp compare_versions b a))
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

  (*$T
    let sub = Sub.make " abc " 1 ~len:3 in \
    "\"abc\"" = (CCFormat.to_string Sub.print sub)
  *)
end
