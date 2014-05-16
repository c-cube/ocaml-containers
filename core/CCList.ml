
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

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption) however
caused and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
*)

(** {1 complements to list} *)

type 'a t = 'a list

(* max depth for direct recursion *)
let _direct_depth = 500

let map f l =
  let rec direct f i l = match l with
    | [] -> []
    | _ when i=0 -> safe f l
    | x::l' ->
        let y = f x in
        y :: direct f (i-1) l'
  and safe f l =
    List.rev (List.rev_map f l)
  in
  direct f _direct_depth l

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let f x = x+1 in \
    List.rev (List.rev_map f l) = map f l)
*)

let append l1 l2 =
  let rec direct i l1 l2 = match l1 with
    | [] -> l2
    | _ when i=0 -> safe l1 l2
    | x::l1' -> x :: direct (i-1) l1' l2
  and safe l1 l2 =
    List.rev_append (List.rev l1) l2
  in
  direct _direct_depth l1 l2

let (@) = append

let rec compare f l1 l2 = match l1, l2 with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | x1::l1', x2::l2' ->
      let c = f x1 x2 in
      if c <> 0 then c else compare f l1' l2'

let rec equal f l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _ | _, [] -> false
  | x1::l1', x2::l2' -> f x1 x2 && equal f l1' l2'

(* difference list *)
type 'a dlist = 'a list -> 'a list

(* append difference lists *)
let _d_append f1 f2 =
  fun l -> f1 (f2 l)

let flat_map f l =
  let rec aux prefix f l = match l with
    | [] -> prefix []
    | x::l' ->
        let sublist = append (f x) in
        let prefix = _d_append prefix sublist in
        aux prefix f l'
  in
  aux (fun l->l) f l

(*$T
  flat_map (fun x -> [x+1; x*2]) [10;100] = [11;20;101;200]
*)

let flatten l = flat_map (fun l -> l) l

let product f l1 l2 =
  flat_map (fun x -> map (fun y -> f x y) l2) l1

let return x = [x]

let (>>=) l f = flat_map f l

let (<$>) = map

let (<*>) funs l = product (fun f x -> f x) funs l

(*$T
  List.sort Pervasives.compare ([(( * )2); ((+)1)] <*> [10;100]) \
    = [11; 20; 101; 200]
*)

let range i j =
  let rec up i j acc =
    if i=j then i::acc else up i (j-1) (j::acc)
  and down i j acc =
    if i=j then i::acc else down i (j+1) (j::acc)
  in
  if i<=j then up i j [] else down i j []

(*$T
  range 0 5 = [0;1;2;3;4;5]
  range 0 0 = [0]
  range 5 2 = [5;4;3;2]
*)

let (--) = range

(*$T
  append (range 0 100) (range 101 1000) = range 0 1000
  append (range 1000 500) (range 499 0) = range 1000 0
*)

let take n l =
  let rec direct i n l = match l with
    | [] -> []
    | _ when i=0 -> safe n [] l
    | x::l' -> x :: direct (i-1) (n-1) l'
  and safe n acc l = match l with
    | [] -> List.rev acc
    | _ when n=0 -> List.rev acc
    | x::l' -> safe (n-1) (x::acc) l'
  in
  direct _direct_depth n l

let rec drop n l = match l with
  | [] -> []
  | _ when n=0 -> l
  | _::l' -> drop (n-1) l'

let split n l = take n l, drop n l

(*$Q
  (Q.pair (Q.list Q.small_int) Q.int) (fun (l,i) -> \
    let i = abs i in \
    let l1, l2 = split i l in \
    l1 @ l2 = l )
*)

let last n l =
  let len = List.length l in
  if len < n then l else drop (len-n) l

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Buffer.t -> 'a -> unit

let to_seq l k = List.iter k l
let of_seq seq =
  let l = ref [] in
  seq (fun x -> l := x :: !l);
  List.rev !l

let to_gen l =
  let l = ref l in
  fun () ->
    match !l with
    | [] -> None
    | x::l' ->
        l := l'; Some x

let of_gen g =
  let rec direct i g =
    if i = 0 then safe [] g
    else match g () with
      | None -> []
      | Some x -> x :: direct (i-1) g
  and safe acc g = match g () with
    | None -> List.rev acc
    | Some x -> safe (x::acc) g
  in
  direct _direct_depth g

(** {2 IO} *)

let pp ?(start="[") ?(stop="]") ?(sep=", ") pp_item buf l =
  let rec print l = match l with
    | x::((y::xs) as l) ->
      pp_item buf x;
      Buffer.add_string buf sep;
      print l
    | x::[] -> pp_item buf x
    | [] -> ()
  in Buffer.add_string buf start; print l; Buffer.add_string buf stop

(*$T
  CCPrint.to_string (pp CCPrint.int) [1;2;3] = "[1, 2, 3]"
  *)
