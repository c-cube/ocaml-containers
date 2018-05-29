
(** {1 Unicode String, in UTF8} *)

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
type 'a sequence = ('a -> unit) -> unit

type t = private string
(** A UTF8 string *)

val equal : t -> t -> bool

val hash : t -> int

val compare : t -> t -> int

val leq : t -> t -> bool
(** Equivalent of [(<=)]
    Allows to be passed to {!Heap.Make} *)

val pp : Format.formatter -> t -> unit

val to_string : t -> string
(** Identity. *)

exception Malformed of string * int
(** Malformed string at given offset *)

val to_gen : ?idx:int -> t -> uchar gen
(** Generator of unicode codepoints.
    @param idx offset where to start the decoding. *)

val to_seq : ?idx:int -> t -> uchar sequence
(** Sequence of unicode codepoints.
    @param idx offset where to start the decoding. *)

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

val append : t -> t -> t

val concat : t -> t list -> t

val of_seq : uchar sequence -> t

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
    Upon iteration, if an invalid substring is met, Malformed will be raised. *)

