
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Lazy List} *)

type +'a t = 'a node lazy_t
and +'a node =
  | Nil
  | Cons of 'a * 'a t

let empty = Lazy.from_val Nil

let return x = Lazy.from_val (Cons (x, empty))

let is_empty = function
  | lazy Nil -> true
  | lazy (Cons _) -> false

let cons x tl = Lazy.from_val (Cons (x,tl))

let head = function
  | lazy Nil -> None
  | lazy (Cons (x, tl)) -> Some (x,tl)

let length l =
  let rec aux acc l = match l with
    | lazy Nil -> acc
    | lazy (Cons (_, tl)) -> aux (acc+1) tl
  in
  aux 0 l

let rec map ~f l =
  lazy (
    match l with
      | lazy Nil -> Nil
      | lazy (Cons (x,tl)) -> Cons (f x, map ~f tl)
  )

let filter ~f l =
  let rec aux f l = match l with
    | lazy Nil -> Nil
    | lazy (Cons (x,tl)) when f x -> Cons (x, lazy (aux f tl))
    | lazy (Cons (_, tl)) ->  aux f tl
  in
  lazy (aux f l)

let rec take n l =
  lazy (
    match l with
      | _ when n=0 -> Nil
      | lazy Nil -> Nil
      | lazy (Cons (x,tl)) -> Cons (x, take (n-1) tl)
  )

let rec append a b =
  lazy (
    match a with
      | lazy Nil -> Lazy.force b
      | lazy (Cons (x,tl)) -> Cons (x, append tl b)
  )

let rec flat_map ~f l =
  lazy (
    match l with
      | lazy Nil -> Nil
      | lazy (Cons (x,tl)) ->
        let res = append (f x) (flat_map ~f tl) in
        Lazy.force res
  )

let default ~default l =
  lazy (
    match l with
      | lazy Nil -> Lazy.force default
      | lazy l -> l
  )

module Infix = struct
  let (>|=) x f = map ~f x
  let (>>=) x f = flat_map ~f x
  let (<|>) a b = default ~default:b a
end

include Infix

type 'a gen = unit -> 'a option

let rec of_gen g =
  lazy (
    match g()  with
      | None -> Nil
      | Some x -> Cons (x, of_gen g)
  )

let rec of_list = function
  | [] -> empty
  | x :: tl -> cons x (of_list tl)

let to_list_rev l =
  let rec aux acc = function
    | lazy Nil -> acc
    | lazy (Cons (x,tl)) -> aux (x::acc) tl
  in
  aux [] l

let to_list l = List.rev (to_list_rev l)

let to_gen l =
  let l = ref l in
  fun () -> match !l with
    | lazy Nil -> None
    | lazy (Cons (x,tl)) -> l := tl; Some x
