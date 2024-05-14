(** Byte buffer.

    A dynamic vector of bytes that doesn't hide its internal from you.
    Same use case as [Buffer.t] but with more power.
    @since 3.7
*)

type t = {
  mutable bs: bytes;  (** The backing bytes buffer *)
  mutable len: int;
      (** Length of the "active" slice in [bs]. The actual content
      of the buffer is [bs[0]..bs[len-1]]. What comes after
      is undefined garbage. *)
}
(** The byte buffer.
    The definition is public since NEXT_RELEASE . *)

type 'a iter = ('a -> unit) -> unit

val create : ?cap:int -> unit -> t
(** Create a new buffer with given initial capacity. *)

val length : t -> int
(** Current length. *)

val is_empty : t -> bool
(** [is_empty b] is [length b=0] *)

val capacity : t -> int
(** Current capacity (size of the array returned by {!bytes}) *)

val bytes : t -> bytes
(** Access the underlying byte buffer. This buffer can change after
    operations that affect the capacity (e.g. {!add_char}). *)

val clear : t -> unit
(** [clear buf] sets [buf.len <- 0]. This doesn't resize the byte buffer. *)

val ensure_cap : t -> int -> unit
(** [ensure_cap self n] ensures that [capacity self >= n].
    @raise Invalid_argument if this requires the buffer to grow beyond system limits. *)

val ensure_free : t -> int -> unit
(** [ensure_free buf n] ensures that the free space at the end of the
    buffer is at least [n].
    @raise Invalid_argument if this requires the buffer to grow beyond system limits. *)

val shrink_to : t -> int -> unit
(** [shrink_to buf n] reduces [length buf] to at most [n].
    Does nothing if the length is already <= n. *)

val add_char : t -> char -> unit
(** Push a character at the end.
    @raise Invalid_argument if this requires the buffer to grow beyond system limits. *)

val append_bytes : t -> bytes -> unit
val append_subbytes : t -> bytes -> int -> int -> unit
val append_string : t -> string -> unit
val append_substring : t -> string -> int -> int -> unit
val append_buf : t -> Buffer.t -> unit
val append_iter : t -> char iter -> unit
val append_seq : t -> char Seq.t -> unit
val get : t -> int -> char
val unsafe_get : t -> int -> char
val set : t -> int -> char -> unit
val unsafe_set : t -> int -> char -> unit

val contents : t -> string
(** Copy the internal data to a string. Allocates. *)

val contents_bytes : t -> bytes
(** Copy the internal data to a {!bytes}. Allocates. *)

val iter : (char -> unit) -> t -> unit
val iteri : (int -> char -> unit) -> t -> unit
val fold_left : ('a -> char -> 'a) -> 'a -> t -> 'a
val of_iter : char iter -> t
val of_seq : char Seq.t -> t
val to_iter : t -> char iter
val to_seq : t -> char Seq.t
