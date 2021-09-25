
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 List Zipper} *)

type 'a t = 'a list * 'a list

let empty = [], []

let is_empty = function
  | [], [] -> true
  | _ -> false

(*$T
  (is_empty empty)
  not ([42] |> make |> right |> is_empty)
*)

let to_list (l,r) = List.rev_append l r

let to_rev_list (l,r) = List.rev_append r l

(*$inject
  let zip_gen = Q.(pair (small_list int)(small_list int))
*)

(*$Q
  zip_gen (fun z -> \
    to_list z = List.rev (to_rev_list z))
*)

let make l = [], l

let left = function
  | x::l, r -> l, x::r
  | [], r -> [], r

let left_exn = function
  | x::l, r -> l, x::r
  | [], _ -> invalid_arg "zipper.left_exn"

let right = function
  | l, x::r -> x::l, r
  | l, [] -> l, []

let right_exn = function
  | l, x::r -> x::l, r
  | _, [] -> invalid_arg "zipper.right_exn"

let modify f z = match z with
  | l, [] ->
    begin match f None with
      | None -> z
      | Some x -> l, [x]
    end
  | l, x::r ->
    begin match f (Some x) with
      | None -> l,r
      | Some _ -> l, x::r
    end

let is_focused = function
  | _, _::_ -> true
  | _, [] -> false

let focused = function
  | _, x::_ -> Some x
  | _, [] -> None

(*$Q
  zip_gen (fun g -> \
    is_focused g = (focused g |> CCOption.is_some))
*)

let focused_exn = function
  | _, x::_ -> x
  | _, [] -> raise Not_found

let insert x (l,r) = l, x::r

let remove (l,r) = match r with
  | [] -> l, []
  | _ :: r' -> l, r'

(*$Q
  Q.(triple int (list small_int)(list small_int)) (fun (x,l,r) -> \
    insert x (l,r) |> remove = (l,r))
*)

let drop_before (_, r) = [], r

let drop_after (l, r) = match r with
  | [] -> l, []
  | x :: _ -> l, [x]

let drop_after_and_focused (l, _) = l, []

(*$=
  ([1], [2]) (drop_after ([1], [2;3]))
  ([1], []) (drop_after ([1], []))
  ([1], []) (drop_after_and_focused ([1], [2;3]))
*)
