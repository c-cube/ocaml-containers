
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Options} *)

type 'a t = 'a option

let[@inline] map f = function
  | None -> None
  | Some x -> Some (f x)

let[@inline] map_or ~default f = function
  | None -> default
  | Some x -> f x

let[@inline] map_lazy default_fn f = function
  | None -> default_fn ()
  | Some x -> f x

let[@inline] is_some = function
  | None -> false
  | Some _ -> true

let[@inline] is_none = function
  | None -> true
  | Some _ -> false

let[@inline] compare f o1 o2 = match o1, o2 with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some x, Some y -> f x y

let[@inline] equal f o1 o2 = match o1, o2 with
  | None, None -> true
  | Some _, None
  | None, Some _ -> false
  | Some x, Some y -> f x y

let[@inline] return x = Some x

let[@inline] (>|=) x f = map f x

let[@inline] (>>=) o f = match o with
  | None -> None
  | Some x -> f x

let[@inline] flat_map f o = match o with
  | None -> None
  | Some x -> f x

let pure = return

let[@inline] (<*>) f x = match f, x with
  | None, _
  | _, None -> None
  | Some f, Some x -> Some (f x)

let (<$>) = map

let[@inline] or_ ~else_ a = match a with
  | None -> else_
  | Some _ -> a

let[@inline] or_lazy ~else_ a = match a with
  | None -> else_ ()
  | Some _ -> a

let[@inline] (<+>) a b = or_ ~else_:b a

let[@inline] choice l = List.fold_left (<+>) None l

let[@inline] map2 f o1 o2 = match o1, o2 with
  | None, _ | _, None -> None
  | Some x, Some y -> Some (f x y)

let[@inline] filter p = function
  | Some x as o when p x -> o
  | _ -> None

(*$=
  None (filter ((=) 0) (Some 1))
  (Some 0) (filter ((=) 0) (Some 0))
  None (filter (fun _ -> true) None)
*)

let[@inline] if_ p x = if p x then Some x else None

let[@inline] exists p = function
  | None -> false
  | Some x -> p x

let[@inline] for_all p = function
  | None -> true
  | Some x -> p x

let[@inline] iter f o = match o with
  | None -> ()
  | Some x -> f x

let[@inline] fold f acc o = match o with
  | None -> acc
  | Some x -> f acc x

let[@inline] get_or ~default x = match x with
  | None -> default
  | Some y -> y

let[@inline] get_exn = function
  | Some x -> x
  | None -> invalid_arg "CCOpt.get_exn"

let[@inline] get_lazy default_fn x = match x with
  | None -> default_fn ()
  | Some y -> y

let sequence_l l =
  let rec aux acc l = match l with
    | [] -> Some (List.rev acc)
    | Some x :: l' -> aux (x::acc) l'
    | None :: _ -> raise Exit
  in
  try aux [] l with Exit -> None

(*$T
  sequence_l [None; Some 1; Some 2] = None
  sequence_l [Some 1; Some 2; Some 3] = Some [1;2;3]
  sequence_l [] = Some []
*)

let wrap ?(handler=fun _ -> true) f x =
  try Some (f x)
  with e ->
    if handler e then None else raise e

let wrap2 ?(handler=fun _ -> true) f x y =
  try Some (f x y)
  with e ->
    if handler e then None else raise e

let[@inline] to_list o = match o with
  | None -> []
  | Some x -> [x]

let[@inline] of_list = function
  | x::_ -> Some x
  | [] -> None

let[@inline] to_result err = function
  | None -> Error err
  | Some x -> Ok x

let[@inline] to_result_lazy err_fn = function
  | None -> Error (err_fn ())
  | Some x -> Ok x

let[@inline] of_result = function
  | Error _ -> None
  | Ok x -> Some x

module Infix = struct
  let (>|=) = (>|=)
  let (>>=) = (>>=)
  let (<*>) = (<*>)
  let (<$>) = (<$>)
  let (<+>) = (<+>)
end

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let random g st =
  if Random.State.bool st then Some (g st) else None

exception ExitChoice

let choice_seq s =
  let r = ref None in
  begin try
      s (function
        | None -> ()
        | (Some _) as o -> r := o; raise ExitChoice
      )
    with ExitChoice -> ()
  end;
  !r

(*$T
  choice_seq (Sequence.of_list [None; Some 1; Some 2]) = Some 1
  choice_seq Sequence.empty = None
  choice_seq (Sequence.repeat None |> Sequence.take 100) = None
*)

let to_gen o =
  match o with
    | None -> (fun () -> None)
    | Some _ ->
      let first = ref true in
      fun () -> if !first then (first:=false; o) else None

let to_seq o k = match o with
  | None -> ()
  | Some x -> k x

let pp ppx out = function
  | None -> Format.pp_print_string out "None"
  | Some x -> Format.fprintf out "@[Some %a@]" ppx x

let[@inline] flatten = function
  | Some x -> x
  | None -> None

(*$T
  flatten None = None
  flatten (Some None) = None
  flatten (Some (Some 1)) = Some 1
*)

let[@inline] return_if b x =
  if b then
    Some x
  else
    None

(*$T
  return_if false 1 = None
  return_if true 1 = Some 1
*)
