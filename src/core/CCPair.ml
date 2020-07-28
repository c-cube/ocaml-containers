
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Tuple Functions} *)

type ('a,'b) t = ('a * 'b)

let make x y = x,y

let map_fst f (x,y) = f x,y

let map_snd f (x,y) = x,f y

let map f g (x,y) = f x, g y

let map_same f (x,y) = f x, f y

let map2 f g (a,b) (x,y) = f a x, g b y

let map_same2 f (a,b) (x,y) = f a x, f b y

let fst_map f (x,_) = f x
let snd_map f (_,x) = f x

let iter f (x,y) = f x y

let swap (x,y) = y, x

let (<<<) = map_fst

let (>>>) = map_snd

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

let to_string ?(sep=", ") a_to_string b_to_string (x,y) =
  Printf.sprintf "%s%s%s" (a_to_string x) sep (b_to_string y)

type 'a printer = Format.formatter -> 'a -> unit

let pp ?(pp_start=fun _ () -> ()) ?(pp_stop=fun _ () -> ())
    ?(pp_sep=fun out () -> Format.fprintf out ",@ ") pa pb out (x,y) =
  pp_start out ();
  pa out x;
  pp_sep out ();
  pb out y;
  pp_stop out ()
