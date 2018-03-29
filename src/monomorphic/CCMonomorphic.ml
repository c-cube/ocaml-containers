
(* This file is free software, part of containers. See file "license" for more details. *)

include Pervasives

let (=.) : float -> float -> bool = (=)
let (<>.) : float -> float -> bool = (<>)
let (<.) : float -> float -> bool = (<)
let (>.) : float -> float -> bool = (>)
let (<=.) : float -> float -> bool = (<=)
let (>=.) : float -> float -> bool = (>=)

let (==) = `Consider_using_CCEqual_physical
let (!=) = `Consider_using_CCEqual_physical
