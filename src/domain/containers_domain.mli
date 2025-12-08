(** A partial stub for {!Domain}. *)

val is_main_domain : unit -> bool
val cpu_relax : unit -> unit

val relax_loop : int -> unit
(** Call {!cpu_relax} n times *)
