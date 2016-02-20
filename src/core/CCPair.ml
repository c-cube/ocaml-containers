
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Tuple Functions} *)

type ('a,'b) t = ('a * 'b)

let make x y = x,y

let map1 f (x,y) = f x,y

let map2 f (x,y) = x,f y

let map f g (x,y) = f x, g y

let map_same f (x,y) = f x, f y

let map_fst f (x,_) = f x
let map_snd f (_,x) = f x

let iter f (x,y) = f x y

let swap (x,y) = y, x

let (<<<) = map1

let (>>>) = map2

let ( *** ) = map

let ( &&& ) f g x = f x, g x

let merge f (x,y) = f x y
let fold = merge

let dup x = x,x
let dup_map f x = x, f x

let equal f g (x1,y1) (x2,y2) = f x1 x2 && g y1 y2

let compare f g (x1,y1) (x2,y2) =
  let c = f x1 x2 in
  if c <> 0 then c else g y1 y2

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

let pp pp_x pp_y buf (x,y) =
  Printf.bprintf buf "(%a, %a)" pp_x x pp_y y

let print pa pb fmt (x,y) =
  Format.fprintf fmt "(%a, %a)" pa x pb y
