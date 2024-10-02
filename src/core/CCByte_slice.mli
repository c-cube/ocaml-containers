(** A simple byte slice.

    @since 3.13.1 *)

type t = {
  bs: bytes;  (** The bytes, potentially shared between many slices *)
  mutable off: int;  (** Offset in [bs] *)
  mutable len: int;
      (** Length of the slice. Valid indices are [bs[off]…bs[off+len-1]],
          inclusive. *)
}

val show : t -> string
(** Simple printer (summary, doesn't show the content) *)

val pp : Format.formatter -> t -> unit
(** Simple printer (summary, doesn't show the content) *)

val create : ?off:int -> ?len:int -> bytes -> t
(** [create bs] creates a slice of [bs].
 @param off optional starting offset
 @param len length of the slice *)

val unsafe_of_string : ?off:int -> ?len:int -> string -> t
(** [unsafe_of_string s] makes a slice from a string.
    This is unsafe because mutating the bytes is forbidden
    (just like with {!Bytes.unsafe_of_string} *)

val len : t -> int
(** Access the length *)

val get : t -> int -> char
(** [get sl i] gets the [i]-th byte of the slice. Same as [sl.bs.[sl.off + i]].
 @raise Invalid_argument if out of bounds. *)

val set : t -> int -> char -> unit
(** [set sl i c] sets the [i]-th byte to [c].
 @raise Invalid_argument if out of bounds. *)

val consume : t -> int -> unit
(** [consume sl n] moves the offset forward by [n] bytes, and
    reduces [len] by [n] bytes. *)

val contents : t -> string
(** A copy of the contents of the slice. Allocates. *)

val sub : t -> int -> int -> t
(** [sub sl off len] makes a new slice with the same
    backing [bs]. *)
