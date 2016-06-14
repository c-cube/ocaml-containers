
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Options} *)

type 'a t = 'a option

let map f = function
  | None -> None
  | Some x -> Some (f x)

let map_or ~default f = function
  | None -> default
  | Some x -> f x

let maybe f default = map_or ~default f

let is_some = function
  | None -> false
  | Some _ -> true

let is_none = function
  | None -> true
  | Some _ -> false

let compare f o1 o2 = match o1, o2 with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some x, Some y -> f x y

let equal f o1 o2 = match o1, o2 with
  | None, None -> true
  | Some _, None
  | None, Some _ -> false
  | Some x, Some y -> f x y

let return x = Some x

let (>|=) x f = map f x

let (>>=) o f = match o with
  | None -> None
  | Some x -> f x

let flat_map f o = match o with
  | None -> None
  | Some x -> f x

let pure x = Some x

let (<*>) f x = match f, x with
  | None, _
  | _, None -> None
  | Some f, Some x -> Some (f x)

let (<$>) = map

let (<+>) a b = match a with
  | None -> b
  | Some _ -> a

let choice l = List.fold_left (<+>) None l

let map2 f o1 o2 = match o1, o2 with
  | None, _
  | _, None -> None
  | Some x, Some y -> Some (f x y)

let filter p = function
  | Some x as o when p x -> o
  | o -> o

let if_ p x = if p x then Some x else None

let exists p = function
  | None -> false
  | Some x -> p x

let for_all p = function
  | None -> true
  | Some x -> p x

let iter f o = match o with
  | None -> ()
  | Some x -> f x

let fold f acc o = match o with
  | None -> acc
  | Some x -> f acc x

let get default x = match x with
  | None -> default
  | Some y -> y

let get_or ~default x = match x with
  | None -> default
  | Some y -> y

let get_exn = function
  | Some x -> x
  | None -> invalid_arg "CCOpt.get_exn"

let get_lazy default_fn x = match x with
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

let to_list o = match o with
  | None -> []
  | Some x -> [x]

let of_list = function
  | x::_ -> Some x
  | [] -> None

module Infix = struct
  let (>|=) = (>|=)
  let (>>=) = (>>=)
  let (<*>) = (<*>)
  let (<$>) = (<$>)
  let (<+>) = (<+>)
end

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Buffer.t -> 'a -> unit
type 'a fmt = Format.formatter -> 'a -> unit
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

let pp ppx buf o = match o with
  | None -> Buffer.add_string buf "None"
  | Some x -> Buffer.add_string buf "Some "; ppx buf x

let print ppx out = function
  | None -> Format.pp_print_string out "None"
  | Some x -> Format.fprintf out "@[Some %a@]" ppx x
