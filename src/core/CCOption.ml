(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Options} *)

type 'a t = 'a option

let[@inline] map f = function
  | None -> None
  | Some x -> Some (f x)

let map_or ~default f = function
  | None -> default
  | Some x -> f x

let map_lazy default_fn f = function
  | None -> default_fn ()
  | Some x -> f x

let is_some = function
  | None -> false
  | Some _ -> true

let is_none = function
  | None -> true
  | Some _ -> false

let compare f o1 o2 =
  match o1, o2 with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some x, Some y -> f x y

let equal f o1 o2 =
  match o1, o2 with
  | None, None -> true
  | Some _, None | None, Some _ -> false
  | Some x, Some y -> f x y

let return x = Some x
let some = return
let none = None

let[@inline] flat_map f o =
  match o with
  | None -> None
  | Some x -> f x

let[@inline] flat_map_l f o =
  match o with
  | None -> []
  | Some x -> f x

let[@inline] bind o f = flat_map f o
let ( >>= ) = bind
let pure x = Some x
let k_compose f g x = f x |> flat_map g
let ( >=> ) = k_compose
let ( <=< ) f g = g >=> f

let ( <*> ) f x =
  match f, x with
  | None, _ | _, None -> None
  | Some f, Some x -> Some (f x)

let or_ ~else_ a =
  match a with
  | None -> else_
  | Some _ -> a

let or_lazy ~else_ a =
  match a with
  | None -> else_ ()
  | Some _ -> a

let ( <+> ) a b = or_ ~else_:b a
let choice l = List.fold_left ( <+> ) None l

let map2 f o1 o2 =
  match o1, o2 with
  | None, _ | _, None -> None
  | Some x, Some y -> Some (f x y)

let filter p = function
  | Some x as o when p x -> o
  | _ -> None

let if_ p x =
  if p x then
    Some x
  else
    None

let exists p = function
  | None -> false
  | Some x -> p x

let for_all p = function
  | None -> true
  | Some x -> p x

let iter f o =
  match o with
  | None -> ()
  | Some x -> f x

let fold f acc o =
  match o with
  | None -> acc
  | Some x -> f acc x

let get_or ~default x =
  match x with
  | None -> default
  | Some y -> y

let apply_or f x =
  match f x with
  | None -> x
  | Some y -> y

let ( |?> ) x f = apply_or f x

let value x ~default =
  match x with
  | None -> default
  | Some y -> y

let get_exn = function
  | Some x -> x
  | None -> invalid_arg "CCOption.get_exn"

let get_exn_or msg = function
  | Some x -> x
  | None -> invalid_arg msg

let get_lazy default_fn x =
  match x with
  | None -> default_fn ()
  | Some y -> y

let sequence_l l =
  let rec aux acc l =
    match l with
    | [] -> Some (List.rev acc)
    | Some x :: l' -> aux (x :: acc) l'
    | None :: _ -> raise Exit
  in
  try aux [] l with Exit -> None

let wrap ?(handler = fun _ -> true) f x =
  try Some (f x)
  with e ->
    if handler e then
      None
    else
      raise e

let wrap2 ?(handler = fun _ -> true) f x y =
  try Some (f x y)
  with e ->
    if handler e then
      None
    else
      raise e

let to_list o =
  match o with
  | None -> []
  | Some x -> [ x ]

let of_list = function
  | x :: _ -> Some x
  | [] -> None

let to_result err = function
  | None -> Error err
  | Some x -> Ok x

let to_result_lazy err_fn = function
  | None -> Error (err_fn ())
  | Some x -> Ok x

let of_result = function
  | Error _ -> None
  | Ok x -> Some x

module Infix = struct
  let ( >|= ) x f = map f x
  let ( >>= ) = ( >>= )
  let ( <*> ) = ( <*> )
  let ( <$> ) = map
  let ( <+> ) = ( <+> )
  let ( |?> ) = ( |?> )
  let ( let+ ) = ( >|= )
  let ( let* ) = ( >>= )

  let[@inline] ( and+ ) o1 o2 =
    match o1, o2 with
    | Some x, Some y -> Some (x, y)
    | _ -> None

  let ( and* ) = ( and+ )
  let ( >=> ) = ( >=> )
  let ( <=< ) = ( <=< )
end

include Infix

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let random g st =
  if Random.State.bool st then
    Some (g st)
  else
    None

exception ExitChoice

let choice_iter s =
  let r = ref None in
  (try
     s (function
       | None -> ()
       | Some _ as o ->
         r := o;
         raise ExitChoice)
   with ExitChoice -> ());
  !r

let rec choice_seq s =
  match s () with
  | Seq.Nil -> None
  | Seq.Cons (Some x, _) -> Some x
  | Seq.Cons (None, tl) -> choice_seq tl

let to_gen o =
  match o with
  | None -> fun () -> None
  | Some _ ->
    let first = ref true in
    fun () ->
      if !first then (
        first := false;
        o
      ) else
        None

let to_iter o k =
  match o with
  | None -> ()
  | Some x -> k x

let to_seq o () =
  match o with
  | None -> Seq.Nil
  | Some x -> Seq.Cons (x, Seq.empty)

let pp ppx out = function
  | None -> Format.pp_print_string out "None"
  | Some x -> Format.fprintf out "@[Some %a@]" ppx x

let flatten = function
  | Some x -> x
  | None -> None

let return_if b x =
  if b then
    Some x
  else
    None
