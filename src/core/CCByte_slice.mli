(** A simple byte slice.

    @since NEXT_RELEASE *)

type t = {
  bs: bytes;  (** The bytes, potentially shared between many slices *)
  mutable off: int;  (** Offset in [bs] *)
  mutable len: int;
      (** Length of the slice. Valid indices are [bs[off]…bs[off+len-1]],
          inclusive. *)
}
[@@deriving show]

val create : ?off:int -> ?len:int -> bytes -> t
val clear : t -> unit
val of_string : string -> t
val len : t -> int
val get : t -> int -> char
val set : t -> int -> char -> unit
val consume : t -> int -> unit
val contents : t -> string
val sub : t -> int -> int -> t
