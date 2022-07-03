(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Immutable Arrays} *)

(* TODO: transient API? for batch modifications *)

type 'a t = 'a array

let empty = [||]
let length = Array.length
let singleton x = [| x |]
let doubleton x y = [| x; y |]
let make n x = Array.make n x
let init n f = Array.init n f
let get = Array.get

let set a n x =
  let a' = Array.copy a in
  a'.(n) <- x;
  a'

let sub = Array.sub
(* Would this not be better implemented with CCArray_slice *)

let map = Array.map
let mapi = Array.mapi

let append a b =
  let na = length a in
  Array.init
    (na + length b)
    (fun i ->
      if i < na then
        a.(i)
      else
        b.(i - na))

let iter = Array.iter
let iteri = Array.iteri
let fold = Array.fold_left

let foldi f acc a =
  let n = ref 0 in
  Array.fold_left
    (fun acc x ->
      let acc = f acc !n x in
      incr n;
      acc)
    acc a

exception ExitNow

let for_all p a =
  try
    Array.iter (fun x -> if not (p x) then raise ExitNow) a;
    true
  with ExitNow -> false

let exists p a =
  try
    Array.iter (fun x -> if p x then raise ExitNow) a;
    false
  with ExitNow -> true

(** {2 Conversions} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let of_list = Array.of_list
let to_list = Array.to_list
let of_array_unsafe a = a (* careful with that axe, Eugene *)

let to_iter a k = iter k a

let of_iter s =
  let l = ref [] in
  s (fun x -> l := x :: !l);
  Array.of_list (List.rev !l)

let rec gen_to_list_ acc g =
  match g () with
  | None -> List.rev acc
  | Some x -> gen_to_list_ (x :: acc) g

let of_gen g =
  let l = gen_to_list_ [] g in
  Array.of_list l

let to_gen a =
  let i = ref 0 in
  fun () ->
    if !i < Array.length a then (
      let x = a.(!i) in
      incr i;
      Some x
    ) else
      None

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

let pp ?(pp_start = fun _ () -> ()) ?(pp_stop = fun _ () -> ())
    ?(pp_sep = fun out () -> Format.fprintf out ",@ ") pp_item out a =
  pp_start out ();
  for k = 0 to Array.length a - 1 do
    if k > 0 then pp_sep out ();
    pp_item out a.(k)
  done;
  pp_stop out ()
