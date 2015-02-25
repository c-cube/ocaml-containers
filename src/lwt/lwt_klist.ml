
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Functional streams for Lwt} *)

type 'a t = [ `Nil | `Cons of 'a * 'a t ] Lwt.t
type 'a stream = 'a t

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let empty = Lwt.return `Nil

let cons x l = Lwt.return (`Cons (x, l))

let rec create f : 'a t =
  f () >|= function
  | None -> `Nil
  | Some x -> `Cons (x, create f)

let next l =
  l >|= function
  | `Nil -> None
  | `Cons (x, tl) -> Some (x, tl)

let next_exn l =
  l >>= function
  | `Nil -> Lwt.fail Not_found
  | `Cons (x, tl) -> Lwt.return (x, tl)

let rec map f l =
  l >|= function
  | `Nil -> `Nil
  | `Cons (x, tl) -> `Cons (f x, map f tl)

let rec map_s (f:'a -> 'b Lwt.t) l =
  l >>= function
  | `Nil -> empty
  | `Cons (x, tl) ->
      f x >|= fun y -> `Cons (y, map_s f tl)

let rec append l1 l2 =
  l1 >>= function
  | `Nil -> l2
  | `Cons (x, tl1) -> Lwt.return (`Cons (x, append tl1 l2))

let rec flat_map f l =
  l >>= function
  | `Nil -> empty
  | `Cons (x, tl) -> append (f x) (flat_map f tl)

let rec filter_map f l =
  l >>= function
  | `Nil -> empty
  | `Cons (x, tl) ->
    match f x with
    | None -> filter_map f tl
    | Some y -> Lwt.return (`Cons (y, filter_map f tl))

let rec filter_map_s f l =
  l >>= function
  | `Nil -> empty
  | `Cons (x, tl) ->
    f x >>= function
    | None -> filter_map_s f tl
    | Some y -> Lwt.return (`Cons (y, filter_map_s f tl))

let rec iter f l =
  l >>= function
  | `Nil -> Lwt.return_unit
  | `Cons (x, tl) -> f x; iter f tl

let rec iter_s f l =
  l >>= function
  | `Nil -> Lwt.return_unit
  | `Cons (x, tl) -> f x >>= fun () -> iter_s f tl

let rec fold f acc l =
  l >>= function
  | `Nil -> Lwt.return acc
  | `Cons (x, tl) ->
    let acc = f acc x in
    fold f acc tl

let rec fold_s f acc l =
  l >>= function
  | `Nil -> Lwt.return acc
  | `Cons (x, tl) -> f acc x >>= fun acc -> fold_s f acc tl

let rec take n l = match n with
  | 0 -> empty
  | _ ->
    l >>= function
    | `Nil -> empty
    | `Cons (x, tl) -> Lwt.return (`Cons (x, take (n-1) tl))

let rec take_while f l =
  l >>= function
  | `Cons (x, tl) when f x -> Lwt.return (`Cons (x, take_while f  tl))
  | `Nil
  | `Cons _ -> empty

let rec take_while_s f l =
  l >>= function
  | `Nil -> empty
  | `Cons (x, tl) ->
    f x >>= function
    | true -> Lwt.return (`Cons (x, take_while_s f  tl))
    | false -> empty

let rec drop n l = match n with
  | 0 -> l
  | _ ->
    l >>= function
    | `Nil -> empty
    | `Cons (_, tl) -> drop (n-1) tl

let rec drop_while f l =
  l >>= function
  | `Nil -> empty
  | `Cons (x, _) when f x -> l
  | `Cons (_, tl) -> drop_while f tl

let rec drop_while_s f l =
  l >>= function
  | `Nil -> empty
  | `Cons (x, tl) ->
    f x >>= function
    | false -> drop_while_s f tl
    | true -> l

let merge a b =
  let add_left = Lwt.map (fun y -> `Left y) in
  let add_right = Lwt.map (fun y -> `Right y) in
  let remove_side l =
    l >|= function
    | `Left x -> x
    | `Right x -> x
  in
  let rec merge' l r =
    Lwt.choose [l; r] >>= function
    | `Left `Nil -> remove_side r
    | `Left (`Cons (x, l')) ->
      Lwt.return (`Cons (x, merge' (add_left l') r))
    | `Right `Nil -> remove_side l
    | `Right (`Cons (x, r')) ->
      Lwt.return (`Cons (x, merge' l (add_right r')))
  in
  merge' (add_left a) (add_right b)

(** {2 Conversions} *)

type 'a gen = unit -> 'a option

let rec of_list l = match l with
  | [] -> empty
  | x :: tl -> Lwt.return (`Cons (x, of_list tl))

let rec of_array_rec a i =
  if i = Array.length a
  then empty
  else Lwt.return (`Cons (a.(i), of_array_rec a (i+1)))

let of_array a = of_array_rec a 0

let rec of_gen g = match g () with
  | None -> empty
  | Some x -> Lwt.return (`Cons (x, of_gen g))

let rec of_gen_s g = match g() with
  | None -> empty
  | Some x ->
    x >|= fun x -> `Cons (x, of_gen_s g)

let rec of_string_rec s i =
  if i = String.length s
  then empty
  else Lwt.return (`Cons (String.get s i, of_string_rec s (i+1)))

let of_string s : char t = of_string_rec s 0

let to_string l =
  let buf = Buffer.create 128 in
  iter (fun c -> Buffer.add_char buf c) l >>= fun () ->
  Lwt.return (Buffer.contents buf)

let to_rev_list l =
  fold (fun acc x -> x :: acc) [] l

let to_list l = to_rev_list l >|= List.rev

(*$Q
  (Q.list Q.int) (fun l -> Lwt_main.run (of_list l |> to_list) = l)
*)

