[@@@if ge 4.08]

include Unit

[@@@else_]

type t = unit

let[@inline] equal (_ : t) (_ : t) = true
let[@inline] compare (_ : t) (_ : t) = 0
let to_string () = "()"

[@@@endif]
