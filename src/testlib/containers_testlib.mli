
type 'a eq = 'a -> 'a -> bool
type 'a print = 'a -> string


module Prelude : sig

  module Q = QCheck

  val t : __FILE__:string -> __LINE__:int -> (unit -> bool) -> unit
  val eq : __FILE__:string -> __LINE__:int -> ?cmp:'a eq -> ?printer:'a print -> 'a -> 'a -> unit
  val q : __FILE__:string -> __LINE__:int -> ?count:int -> 'a Q.arbitrary -> ('a -> bool) -> unit

  val assert_equal : ?printer:'a print -> ?cmp:'a eq -> 'a -> 'a -> unit
end

val run_all : ?seed:string -> descr:string -> unit -> unit
