[@@@if ge 4.8]

(** Let operators on OCaml >= 4.08.0, nothing otherwise
  @since 2.8
  @inline *)
module type LET = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

[@@@endif]
