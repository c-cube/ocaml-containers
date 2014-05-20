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

let foldi f acc a =
  let rec recurse acc i =
    if i = Array.length a then acc else recurse (f acc i a.(i)) (i+1)
  in recurse acc 0

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

(** all the elements of a, but the i-th, into a list *)
let except_idx a i =
  foldi
    (fun acc j elt -> if i = j then acc else elt::acc)
    [] a

(* Randomly shuffle the array, in place.
   See http://en.wikipedia.org/wiki/Fisher-Yates_shuffle *)
let shuffle a = 
  for i = 1 to Array.length a - 1 do
    let j = Random.int i in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp;
  done

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

