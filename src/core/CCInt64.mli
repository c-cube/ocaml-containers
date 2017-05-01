(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Int64}

    Helpers for in64.

    @since 0.13 *)

type t = int64

val (+) : t -> t -> t

val (-) : t -> t -> t

val (~-) : t -> t

val ( * ) : t -> t -> t

val (/) : t -> t -> t

val (mod) : t -> t -> t

val abs : t -> t

val max_int : t

val min_int : t

val (land) : t -> t -> t

val (lor) : t -> t -> t

val (lxor) : t -> t -> t

val lnot : t -> t

val (lsl) : t -> int -> t

val (lsr) : t -> int -> t

val (asr) : t -> int -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val hash : t -> int

(** {2 Conversion} *)

val to_int : t -> int

val of_int : int -> t option

val of_int_exn : int -> t
(** Alias to {!Int64.of_int}
    @raise Failure in case of failure *)

val to_int32 : t -> int32

val of_int32 : int32 -> t option

val of_int32_exn : int32 -> t
(** Alias to {!Int64.of_int32}
    @raise Failure in case of failure *)

val to_nativeint : t -> nativeint

val of_nativeint : nativeint -> t option

val of_nativeint_exn : nativeint -> t
(** Alias to {!Int64.of_nativeint}
    @raise Failure in case of failure *)

val to_float : t -> float

val of_float : float -> t option

val of_float_exn : float -> t
(** Alias to {!Int64.of_float}
    @raise Failure in case of failure *)

val to_string : t -> string

val of_string : string -> t option

val of_string_exn : string -> t
