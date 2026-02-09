(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Tuple Functions} *)

[@@@ifge 5.4]

include Pair

[@@@else_]

type ('a, 'b) t = 'a * 'b

let make x y = x, y
let fst = fst
let snd = snd
let swap (x, y) = y, x
let map_fst f (x, y) = f x, y
let map_snd f (x, y) = x, f y
let map f g (x, y) = f x, g y

[@@@endif]

let map_same f (x, y) = f x, f y
let map2 f g (a, b) (x, y) = f a x, g b y
let map_same2 f (a, b) (x, y) = f a x, f b y
let fst_map f (x, _) = f x
let snd_map f (_, x) = f x

[@@@iflt 5.4]

let iter f (x, y) = f x y

[@@@endif]

let ( <<< ) = map_fst
let ( >>> ) = map_snd
let ( *** ) = map
let ( &&& ) f g x = f x, g x
let merge f (x, y) = f x y

[@@@iflt 5.4]

let fold = merge

[@@@endif]

let dup x = x, x
let dup_map f x = x, f x

[@@@iflt 5.4]

let equal f g (x1, y1) (x2, y2) = f x1 x2 && g y1 y2

let compare f g (x1, y1) (x2, y2) =
  let c = f x1 x2 in
  if c <> 0 then
    c
  else
    g y1 y2

[@@@endif]

let to_string ?(sep = ", ") a_to_string b_to_string (x, y) =
  Printf.sprintf "%s%s%s" (a_to_string x) sep (b_to_string y)

type 'a printer = Format.formatter -> 'a -> unit

let pp ?(pp_start = fun _ () -> ()) ?(pp_stop = fun _ () -> ())
    ?(pp_sep = fun out () -> Format.fprintf out ",@ ") pa pb out (x, y) =
  pp_start out ();
  pa out x;
  pp_sep out ();
  pb out y;
  pp_stop out ()
