
(** Unicode String, in UTF8 *)

(** A unicode string represented by a utf8 bytestring. This representation
    is convenient for manipulating normal OCaml strings that are encoded
    in UTF8.

    We perform only basic decoding and encoding between codepoints and
    bytestrings.
    For more elaborate operations,
    please use the excellent {{: http://erratique.ch/software/uutf} Uutf}.

    {b status: experimental}

    @since 2.1
*)


type uchar = Uchar.t
type 'a gen = unit -> 'a option

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)


type t = private string
(** A UTF8 string *)

val equal : t -> t -> bool

val hash : t -> int

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val to_string : t -> string
(** Identity. *)

exception Malformed of string * int
(** Malformed string at given offset *)

val to_gen : ?idx:int -> t -> uchar gen
(** Generator of unicode codepoints.
    @param idx offset where to start the decoding. *)

val to_iter : ?idx:int -> t -> uchar iter
(** Iterator of unicode codepoints.
    @param idx offset where to start the decoding.
    @since 2.8 *)

val to_seq : ?idx:int -> t -> uchar Seq.t
(** Iter of unicode codepoints.
    Renamed from [to_std_seq] since 3.0.
    @param idx offset where to start the decoding.
    @since 3.0
*)

val to_list : ?idx:int -> t -> uchar list
(** List of unicode codepoints.
    @param idx offset where to start the decoding. *)

val fold : ?idx:int -> ('a -> uchar -> 'a) -> 'a -> t -> 'a

val iter : ?idx:int -> (uchar -> unit) -> t -> unit

val n_chars : t -> int
(** Number of characters. *)

val n_bytes : t -> int
(** Number of bytes. *)

val map : (uchar -> uchar) -> t -> t

val filter_map : (uchar -> uchar option) -> t -> t

val flat_map : (uchar -> t) -> t -> t

val empty : t
(** Empty string.
    @since 3.5 *)

val append : t -> t -> t
(** Append two string together. *)

val concat : t -> t list -> t
(** [concat sep l] concatenates each string in [l], inserting [sep]
    in between each string. Similar to {!String.concat}. *)

val of_uchar : uchar -> t
(** [of_char c] is a string with only one unicode char in it.
    @since 3.5 *)

val make : int -> uchar -> t
(** [make n c] makes a new string with [n] copies of [c] in it.
    @since 3.5 *)

val of_seq : uchar Seq.t -> t
(** Build a string from unicode codepoints
    Renamed from [of_std_seq] since 3.0.
    @since 3.0 *)

val of_iter : uchar iter -> t
(** Build a string from unicode codepoints
    @since 2.8 *)

val uchar_to_bytes : uchar -> char iter
(** Translate the unicode codepoint to a list of utf-8 bytes.
    This can be used, for example, in combination with {!Buffer.add_char}
    on a pre-allocated buffer to add the bytes one by one (despite its name,
      {!Buffer.add_char} takes individual bytes, not unicode codepoints).
    @since 3.2 *)

val of_gen : uchar gen -> t

val of_list : uchar list -> t

val of_string_exn : string -> t
(** Validate string by checking it is valid UTF8.
    @raise Invalid_argument if the string is not valid UTF8. *)

val of_string : string -> t option
(** Safe version of {!of_string_exn}. *)

val is_valid : string -> bool
(** Valid UTF8? *)

val unsafe_of_string : string -> t
(** Conversion from a string without validating.
    {b CAUTION} this is unsafe and can break all the other functions
    in this module. Use only if you're sure the string is valid UTF8.
    Upon iteration, if an invalid substring is met, Malformed will be raised.
*)

