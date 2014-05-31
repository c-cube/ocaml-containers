
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

let fold_right f l acc =
  let rec direct i f l acc = match l with
    | [] -> acc
    | _ when i=0 -> safe f (List.rev l) acc
    | x::l' ->
        let acc = direct (i-1) f l' acc in
        f x acc
  and safe f l acc = match l with
    | [] -> acc
    | x::l' ->
        let acc = f x acc in
        safe f l' acc
  in
  direct _direct_depth f l acc

(*$T
  fold_right (+) (1 -- 1_000_000) 0 = \
    List.fold_left (+) 0 (1 -- 1_000_000)
*)

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

let fold_product f acc l1 l2 =
  List.fold_left
    (fun acc x1 ->
      List.fold_left
        (fun acc x2 -> f acc x1 x2)
        acc l2
    ) acc l1

let diagonal l =
  let rec gen acc l = match l with
  | [] -> acc
  | x::l' ->
    let acc = List.fold_left (fun acc y -> (x,y) :: acc) acc l' in
    gen acc l'
  in
  gen [] l

let return x = [x]

let (>>=) l f = flat_map f l

let (<$>) = map

let (<*>) funs l = product (fun f x -> f x) funs l

let sorted_merge ?(cmp=Pervasives.compare) l1 l2 =
  let rec recurse cmp acc l1 l2 = match l1,l2 with
    | [], _ -> List.rev_append acc l2
    | _, [] -> List.rev_append acc l1
    | x1::l1', x2::l2' ->
      let c = cmp x1 x2 in
      if c < 0 then recurse cmp (x1::acc) l1' l2
      else if c > 0 then recurse cmp (x2::acc) l1 l2'
      else recurse cmp (x1::x2::acc) l1' l2'
  in
  recurse cmp [] l1 l2

(*$T
  List.sort Pervasives.compare ([(( * )2); ((+)1)] <*> [10;100]) \
    = [11; 20; 101; 200]
*)


let take n l =
  let rec direct i n l = match l with
    | [] -> []
    | _ when i=0 -> safe n [] l
    | x::l' ->
        if n > 0
        then x :: direct (i-1) (n-1) l'
        else []
  and safe n acc l = match l with
    | [] -> List.rev acc
    | _ when n=0 -> List.rev acc
    | x::l' -> safe (n-1) (x::acc) l'
  in
  direct _direct_depth n l

(*$T
  take 2 [1;2;3;4;5] = [1;2]
  take 10_000 (range 0 100_000) |> List.length = 10_000
  take 10_000 (range 0 2_000) = range 0 2_000
*)

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

let find p l =
  let rec search i l = match l with
  | [] -> None
  | x::_ when p x -> Some (i, x)
  | _::xs -> search (i+1) xs
  in search 0 l

let filter_map f l =
  let rec recurse acc l = match l with
  | [] -> List.rev acc
  | x::l' ->
    let acc' = match f x with | None -> acc | Some y -> y::acc in
    recurse acc' l'
  in recurse [] l

module Set = struct
  let mem ?(eq=(=)) x l =
    let rec search eq x l = match l with
      | [] -> false
      | y::l' -> eq x y || search eq x l'
    in search eq x l

  let subset ?(eq=(=)) l1 l2 =
    List.for_all
      (fun t -> mem ~eq t l2)
      l1

  let rec uniq ?(eq=(=)) l = match l with
    | [] -> []
    | x::xs when List.exists (eq x) xs -> uniq ~eq xs
    | x::xs -> x :: uniq ~eq xs

  let rec union ?(eq=(=)) l1 l2 = match l1 with
    | [] -> l2
    | x::xs when mem ~eq x l2 -> union ~eq xs l2
    | x::xs -> x::(union ~eq xs l2)

  let rec inter ?(eq=(=)) l1 l2 = match l1 with
    | [] -> []
    | x::xs when mem ~eq x l2 -> x::(inter ~eq xs l2)
    | _::xs -> inter ~eq xs l2
end

module Idx = struct
  let mapi f l =
    let r = ref 0 in
    map
      (fun x ->
        let y = f !r x in
        incr r; y
      ) l

  (*$T
    Idx.mapi (fun i x -> i*x) [10;10;10] = [0;10;20]
  *)

  let iteri f l =
    let rec aux f i l = match l with
      | [] -> ()
      | x::l' -> f i x; aux f (i+1) l'
    in aux f 0 l

  let foldi f acc l =
    let rec foldi f acc i l = match l with
    | [] -> acc
    | x::l' ->
      let acc = f acc i x in
      foldi f acc (i+1) l'
    in
    foldi f acc 0 l

  let rec get_exn l i = match l with
    | [] -> raise Not_found
    | x::_ when i=0 -> x
    | _::l' -> get_exn l' (i-1)

  let get l i =
    try Some (get_exn l i)
    with Not_found -> None

  (*$T
    Idx.get (range 0 10) 0 = Some 0
    Idx.get (range 0 10) 5 = Some 5
    Idx.get (range 0 10) 11 = None
    Idx.get [] 0 = None
  *)

  let set l0 i x =
    let rec aux l acc i = match l with
      | [] -> l0
      | _::l' when i=0 -> List.rev_append acc (x::l')
      | y::l' ->
          aux l' (y::acc) (i-1)
    in
    aux l0 [] i

  (*$T
    Idx.set [1;2;3] 0 10 = [10;2;3]
    Idx.set [1;2;3] 4 10 = [1;2;3]
    Idx.set [1;2;3] 1 10 = [1;10;3]
   *)

  let insert l i x =
    let rec aux l acc i x = match l with
      | [] -> List.rev_append acc [x]
      | y::l' when i=0 -> List.rev_append acc (x::y::l')
      | y::l' ->
          aux l' (y::acc) (i-1) x
    in
    aux l [] i x

  (*$T
    Idx.insert [1;2;3] 0 10 = [10;1;2;3]
    Idx.insert [1;2;3] 4 10 = [1;2;3;10]
    Idx.insert [1;2;3] 1 10 = [1;10;2;3]
   *)

  let remove l0 i =
    let rec aux l acc i = match l with
      | [] -> l0
      | _::l' when i=0 -> List.rev_append acc l'
      | y::l' ->
          aux l' (y::acc) (i-1)
    in
    aux l0 [] i

  (*$T
    Idx.remove [1;2;3;4] 0 = [2;3;4]
    Idx.remove [1;2;3;4] 3 = [1;2;3]
    Idx.remove [1;2;3;4] 5 = [1;2;3;4]
  *)
end

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

let replicate i x =
  let rec aux acc i =
    if i = 0 then acc
    else aux (x::acc) (i-1)
  in aux [] i

let repeat i l =
  let l' = List.rev l in
  let rec aux acc i =
    if i = 0 then List.rev acc
    else aux (List.rev_append l' acc) (i-1)
  in aux [] i

module Assoc = struct
  type ('a, 'b) t = ('a*'b) list

  let get_exn ?(eq=(=)) l x =
    let rec search eq l x = match l with
      | [] -> raise Not_found
      | (y,z)::l' ->
          if eq x y then z else search eq l' x
    in search eq l x

  let get ?eq l x =
    try Some (get_exn ?eq l x)
    with Not_found -> None

  (*$T
    Assoc.get [1, "1"; 2, "2"] 1 = Some "1"
    Assoc.get [1, "1"; 2, "2"] 2 = Some "2"
    Assoc.get [1, "1"; 2, "2"] 3 = None
    Assoc.get [] 42 = None
  *)

  let set ?(eq=(=)) l x y =
    let rec search eq acc l x y = match l with
      | [] -> (x,y)::acc
      | (x',y')::l' ->
          if eq x x' 
            then (x,y)::List.rev_append acc l'
            else search eq ((x',y')::acc) l' x y
    in search eq [] l x y

  (*$T
    Assoc.set [1,"1"; 2, "2"] 2 "two" |> List.sort Pervasives.compare \
      = [1, "1"; 2, "two"]
    Assoc.set [1,"1"; 2, "2"] 3 "3" |> List.sort Pervasives.compare \
      = [1, "1"; 2, "2"; 3, "3"]
  *)
end

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

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

let to_klist l =
  let rec make l () = match l with
    | [] -> `Nil
    | x::l' -> `Cons (x, make l')
  in make l

let of_klist l =
  let rec direct i g =
    if i = 0 then safe [] g
    else match l () with
      | `Nil -> []
      | `Cons (x,l') -> x :: direct (i-1) l'
  and safe acc l = match l () with
    | `Nil -> List.rev acc
    | `Cons (x,l') -> safe (x::acc) l'
  in
  direct _direct_depth l

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

let print ?(start="[") ?(stop="]") ?(sep=", ") pp_item fmt l =
  let rec print fmt l = match l with
    | x::((y::xs) as l) ->
      pp_item fmt x;
      Format.pp_print_string fmt sep;
      print fmt l
    | x::[] -> pp_item fmt x
    | [] -> ()
  in
  Format.fprintf fmt "@[%s%a%s@]" start print l stop
