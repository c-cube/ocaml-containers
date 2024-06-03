(* This file is free software, part of containers. See file "license" for more details. *)

let ( = ) : int -> int -> bool = Stdlib.( = )
let ( <> ) : int -> int -> bool = Stdlib.( <> )
let ( < ) : int -> int -> bool = Stdlib.( < )
let ( > ) : int -> int -> bool = Stdlib.( > )
let ( <= ) : int -> int -> bool = Stdlib.( <= )
let ( >= ) : int -> int -> bool = Stdlib.( >= )
let compare : int -> int -> int = Stdlib.compare

[@@@ifge 4.13]

let min : int -> int -> int = Int.min
let max : int -> int -> int = Int.max

[@@@else_]

let min : int -> int -> int = Stdlib.min
let max : int -> int -> int = Stdlib.max

[@@@endif]

let ( =. ) : float -> float -> bool = Stdlib.( = )
let ( <>. ) : float -> float -> bool = Stdlib.( <> )
let ( <. ) : float -> float -> bool = Stdlib.( < )
let ( >. ) : float -> float -> bool = Stdlib.( > )
let ( <=. ) : float -> float -> bool = Stdlib.( <= )
let ( >=. ) : float -> float -> bool = Stdlib.( >= )
let ( == ) = `Consider_using_CCEqual_physical
let ( != ) = `Consider_using_CCEqual_physical
