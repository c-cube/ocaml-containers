
(** Byte buffer.

    A dynamic vector of bytes.
    @since NEXT_RELEASE
*)

type t

type 'a iter = ('a -> unit) -> unit

val create : ?cap:int -> unit -> t
(** Create a new buffer with given initial capacity. *)

val length : t -> int
(** Current length. *)

val capacity : t -> int
(** Current capacity (size of the array returned by {!bytes}) *)

val bytes : t -> bytes
(** Access the underlying byte buffer. This buffer can change after
    operations that affect the capacity (e.g. {!add_char}). *)

val clear : t -> unit

val ensure_cap : t -> int -> unit
(** [ensure_cap self n] ensures that [capacity self >= n]. *)

val shrink_to : t -> int -> unit
(** [shrink_to buf n] reduces [length buf] to at most [n].
    Does nothing if the length is already <= n. *)

val add_char : t -> char -> unit
(** Push a character at the end. *)

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

val iter : (char -> unit) -> t -> unit

val fold_left : ('a -> char -> 'a) -> 'a -> t -> 'a

val of_iter : char iter -> t
val of_seq : char Seq.t -> t

val to_iter : t -> char iter
val to_seq : t -> char Seq.t
