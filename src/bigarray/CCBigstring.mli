
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Interface to 1-dimension Bigarrays of bytes (char)}

    @deprecated use the package [bigstring] instead.

    {b status: deprecated, do not use anymore}

    @since 0.7 *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val create : int -> t
(** Create a new bigstring of the given size. *)

val empty : t
(** Empty string *)

val init : int -> (int -> char) -> t
(** Initialize with the given function (called at every index) *)

val fill : t -> char -> unit
(** Fill with a single byte *)

val size : t -> int
(** Number of bytes *)

val length : t -> int
(** Alias for [size].
    @since 0.8 *)

val get : t -> int -> char

val set : t -> int -> char -> unit

val blit : t -> int -> t -> int -> int -> unit
(** Blit a slice of the bigstring into another *)

val copy : t -> t
(** Copy of the string *)

val sub : t -> int -> int -> t
(** [sub s i len] takes a slice of length [len] from the string [s], starting
    at offset [i].
    @raise Invalid_argument if [i, len] doesn't designate a valid substring *)

val fold : ('a -> char -> 'a) -> 'a -> t -> 'a

val iter : (char -> unit) -> t -> unit

val equal : t -> t -> bool

val compare : t -> t -> int
(** Lexicographic order *)

(** {2 Conversions} *)

val to_bytes : t -> Bytes.t

val of_bytes : Bytes.t -> t

val of_bytes_slice : Bytes.t -> int -> int -> t

val sub_bytes : t -> int -> int -> Bytes.t

val blit_to_bytes : t -> int -> Bytes.t -> int -> int -> unit

val blit_of_bytes : Bytes.t -> int -> t -> int -> int -> unit

val to_string : t -> string

val of_string : string -> t

val of_string_slice : string -> int -> int -> t

val sub_string : t -> int -> int -> string

val blit_of_string : string -> int -> t -> int -> int -> unit

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

val to_seq : t -> char sequence

val to_gen : t -> char gen

val to_seq_slice : t -> int -> int -> char sequence

val to_gen_slice : t -> int -> int -> char gen

val print : t printer
(** @since 0.13 *)

(** {2 Memory-map} *)

val with_map_file :
  ?pos:int64 -> ?len:int -> ?mode:int -> ?flags:open_flag list -> ?shared:bool ->
  string -> (t -> 'a) -> 'a
(** [with_map_file name f] maps the file into memory, opening it, and
    call [f] with a slice [pos.... pos+len] of the bytes of the file
    where [len] is the length of the file if not provided.
    When [f] returns, the file is closed.
    @param pos offset in the file (default 0)
    @param shared if true, modifications are shared between processes that
      have mapped this file (requires the filedescr to be open in write mode).
    @param mode the mode for the file, if it's created
    @param flags opening flags (default rdonly)
    see {!Bigarray.Array1.map_file} for more details *)

val map_file_descr : ?pos:int64 -> ?shared:bool -> Unix.file_descr -> int -> t
(** [map_file_descr descr len] is a lower-level access to an underlying file descriptor.
    @param shared if true, modifications are shared between processes that
    have mapped this file (requires the filedescr to be open in write mode).
    see {!Bigarray.Array1.map_file} for more details *)
