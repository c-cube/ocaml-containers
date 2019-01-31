
(* This file is free software, part of containers. See file "license" for more details. *)

let (=) : int -> int -> bool = Pervasives.(=)
let (<>) : int -> int -> bool  = Pervasives.(<>)
let (<) : int -> int -> bool = Pervasives.(<)
let (>) : int -> int -> bool = Pervasives.(>)
let (<=) : int -> int -> bool = Pervasives.(<=)
let (>=) : int -> int -> bool = Pervasives.(>=)

let compare : int -> int -> int = Pervasives.compare
let min : int -> int -> int = Pervasives.min
let max : int -> int -> int = Pervasives.max

let (=.) : float -> float -> bool = Pervasives.(=)
let (<>.) : float -> float -> bool = Pervasives.(<>)
let (<.) : float -> float -> bool = Pervasives.(<)
let (>.) : float -> float -> bool = Pervasives.(>)
let (<=.) : float -> float -> bool = Pervasives.(<=)
let (>=.) : float -> float -> bool = Pervasives.(>=)


let (==) = `Consider_using_CCEqual_physical
let (!=) = `Consider_using_CCEqual_physical
