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

let[@inline] flat_map f o = match o with
  | None -> None
  | Some x -> f x

let[@inline] bind o f = flat_map f o

let (>>=) = bind

let pure x = Some x

let (<*>) f x = match f, x with
  | None, _
  | _, None -> None
  | Some f, Some x -> Some (f x)

let or_ ~else_ a = match a with
  | None -> else_
  | Some _ -> a

let or_lazy ~else_ a = match a with
  | None -> else_ ()
  | Some _ -> a

let (<+>) a b = or_ ~else_:b a

let choice l = List.fold_left (<+>) None l

let map2 f o1 o2 = match o1, o2 with
  | None, _
  | _, None -> None
  | Some x, Some y -> Some (f x y)

let filter p = function
  | Some x as o when p x -> o
  | _ -> None

(*$=
  None (filter ((=) 0) (Some 1))
  (Some 0) (filter ((=) 0) (Some 0))
  None (filter (fun _ -> true) None)
*)

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

let get_or ~default x = match x with
  | None -> default
  | Some y -> y

let value x ~default = match x with
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
  let (>|=) x f = map f x
  let (>>=) = (>>=)
  let (<*>) = (<*>)
  let (<$>) = map
  let (<+>) = (<+>)

  include CCShimsMkLet_.Make(struct
      type 'a t = 'a option
      let (>|=) = (>|=)
      let (>>=) = (>>=)
      let[@inline] monoid_product o1 o2 = match o1, o2 with
        | Some x, Some y -> Some (x,y)
        | _ -> None
    end)
end

include Infix

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let random g st =
  if Random.State.bool st then Some (g st) else None

exception ExitChoice

let choice_iter s =
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
  choice_iter (Iter.of_list [None; Some 1; Some 2]) = Some 1
  choice_iter Iter.empty = None
  choice_iter (Iter.repeat None |> Iter.take 100) = None
*)

let rec choice_seq s = match s() with
  | Seq.Nil -> None
  | Seq.Cons (Some x, _) -> Some x
  | Seq.Cons (None, tl) -> choice_seq tl

(*$T
  choice_seq (CCSeq.of_list [None; Some 1; Some 2]) = Some 1
  choice_seq CCSeq.empty = None
  choice_seq (CCSeq.repeat None |> CCSeq.take 100) = None
*)

let to_gen o =
  match o with
    | None -> (fun () -> None)
    | Some _ ->
      let first = ref true in
      fun () -> if !first then (first:=false; o) else None

let to_iter o k = match o with
  | None -> ()
  | Some x -> k x

let to_seq = to_iter

let to_seq o () = match o with
  | None -> Seq.Nil
  | Some x -> Seq.Cons (x, Seq.empty)

let pp ppx out = function
  | None -> Format.pp_print_string out "None"
  | Some x -> Format.fprintf out "@[Some %a@]" ppx x

let flatten = function
  | Some x -> x
  | None -> None

(*$T
  flatten None = None
  flatten (Some None) = None
  flatten (Some (Some 1)) = Some 1
*)

let return_if b x =
  if b then
    Some x
  else
    None

(*$T
  return_if false 1 = None
  return_if true 1 = Some 1
*)
