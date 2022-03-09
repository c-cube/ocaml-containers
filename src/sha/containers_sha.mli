

module type S = sig
  type t
  type ctx

  val create : unit -> ctx

  val add_string : ctx -> string -> unit

  val finalize : ctx -> t

  val to_hex : t -> string
end

module SHA256 : S
