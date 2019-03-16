
(* This file is free software, part of containers. See file "license" for more details. *)

open CCMonomorphicShims_

let (=) : int -> int -> bool = Stdlib.(=)
let (<>) : int -> int -> bool  = Stdlib.(<>)
let (<) : int -> int -> bool = Stdlib.(<)
let (>) : int -> int -> bool = Stdlib.(>)
let (<=) : int -> int -> bool = Stdlib.(<=)
let (>=) : int -> int -> bool = Stdlib.(>=)

let compare : int -> int -> int = Stdlib.compare
let min : int -> int -> int = Stdlib.min
let max : int -> int -> int = Stdlib.max

let (=.) : float -> float -> bool = Stdlib.(=)
let (<>.) : float -> float -> bool = Stdlib.(<>)
let (<.) : float -> float -> bool = Stdlib.(<)
let (>.) : float -> float -> bool = Stdlib.(>)
let (<=.) : float -> float -> bool = Stdlib.(<=)
let (>=.) : float -> float -> bool = Stdlib.(>=)


let (==) = `Consider_using_CCEqual_physical
let (!=) = `Consider_using_CCEqual_physical
