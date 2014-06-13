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

(** {1 Array utils} *)

type 'a t = 'a array

let empty = [| |]

let map = Array.map

let foldi f acc a =
  let rec recurse acc i =
    if i = Array.length a then acc else recurse (f acc i a.(i)) (i+1)
  in recurse acc 0

let reverse_in_place a =
  if a = [| |] then ()
  else
    let n = Array.length a in
    for i = 0 to (n-1)/2 do
      let t = a.(i) in
      a.(i) <- a.(n-i-1);
      a.(n-i-1) <- t;
    done

(*$T
  reverse_in_place [| |]; true
  reverse_in_place [| 1 |]; true
  let a = [| 1; 2; 3; 4; 5 |] in \
    reverse_in_place a; \
    a = [| 5;4;3;2;1 |]
  let a = [| 1; 2; 3; 4; 5; 6 |] in \
    reverse_in_place a; \
    a = [| 6;5;4;3;2;1 |]
*)

let filter_map f a =
  let rec aux acc i =
    if i = Array.length a
    then (
      let a' = Array.of_list acc in
      reverse_in_place a';
      a'
    ) else match f a.(i) with
      | None -> aux acc (i+1)
      | Some x -> aux (x::acc) (i+1)
  in aux [] 0

(*$T
  filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
    [| 1; 2; 3; 4 |] = [| "2"; "4" |]
  filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
    [| 1; 2; 3; 4; 5; 6 |] \
    = [| "2"; "4"; "6" |]
*)

let filter p a =
  filter_map (fun x -> if p x then Some x else None) a

(* append [rev a] in front of [acc] *)
let rec __rev_append_list a acc i =
  if i = Array.length a
  then acc
  else
    __rev_append_list a (a.(i) :: acc) (i+1)

let flat_map f a =
  let rec aux acc i =
    if i = Array.length a
    then (
      let a' = Array.of_list acc in
      reverse_in_place a';
      a'
    )
    else
      let a' = f a.(i) in
      aux (__rev_append_list a' acc 0) (i+1)
  in aux [] 0

(*$T
  let a = [| 1; 3; 5 |] in \
  let a' = flat_map (fun x -> [| x; x+1 |]) a in \
  a' = [| 1; 2; 3; 4; 5; 6 |]
*)

let (>>=) a f = flat_map f a

let for_all p a =
  let rec check i =
    i = Array.length a || (p a.(i) && check (i+1))
  in check 0

let for_all2 p a1 a2 =
  let rec check i =
    i = Array.length a1 || (p a1.(i) a2.(i) && check (i+1))
  in
  if Array.length a1 <> Array.length a2
    then raise (Invalid_argument "forall2")
    else check 0

let exists p a =
  let rec check i =
    i < Array.length a && (p a.(i) || check (i+1))
  in check 0

let (--) i j =
  if i<=j
  then
    Array.init (j-i+1) (fun k -> i+k)
  else
    Array.init (i-j+1) (fun k -> i-k)

(** all the elements of a, but the i-th, into a list *)
let except_idx a i =
  foldi
    (fun acc j elt -> if i = j then acc else elt::acc)
    [] a

(* Randomly shuffle the array, in place.
   See http://en.wikipedia.org/wiki/Fisher-Yates_shuffle *)
let _shuffle _rand_int a = 
  for i = 1 to Array.length a - 1 do
    let j = _rand_int i in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp;
  done

let shuffle a = _shuffle Random.int a

let shuffle_with st a = _shuffle (Random.State.int st) a

(** print an array of items using the printing function *)
let pp ?(sep=", ") pp_item buf a =
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Buffer.add_string buf sep);
    pp_item buf a.(i)
  done

(** print an array of items using the printing function *)
let pp_i ?(sep=", ") pp_item buf a =
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Buffer.add_string buf sep);
    pp_item buf i a.(i)
  done

let print ?(sep=", ") pp_item fmt a =
  Array.iteri
    (fun i x ->
      if i > 0 then Format.pp_print_string fmt sep;
      pp_item fmt x
    ) a
