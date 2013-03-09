(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
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

(** {1 Functional queues (fifo)} *)

type 'a t = {
  hd : 'a list;
  tl : 'a list;
} (** Queue containing elements of type 'a *)

let empty = {
  hd = [];
  tl = [];
}

let is_empty q = q.hd = [] && q.tl = []

let push q x = {q with tl = x :: q.tl; }

let rec list_last l = match l with
  | [] -> assert false
  | [x] -> x
  | _::l' -> list_last l'

let peek q =
  match q.hd, q.tl with
  | [], [] -> raise (Invalid_argument "Queue.peek")
  | [], _::_ ->
    list_last q.tl
  | x::_, _ -> x

(* pop first element of the queue *)
let pop q =
  match q.hd, q.tl with
  | [], [] -> raise (Invalid_argument "Queue.peek")
  | [], _::_ ->
    (match List.rev q.tl with
      | x::hd -> x, { hd; tl=[]; }
      | [] -> assert false)
  | x::_, _ ->
    let q' = {hd=List.tl q.hd; tl=q.tl; } in
    x, q'

let junk q = snd (pop q)

(** Append two queues. Elements from the second one come
    after elements of the first one *)
let append q1 q2 =
  { hd=q1.hd;
    tl=q2.tl @ (List.rev_append q2.hd q1.tl);
  }

let size q = List.length q.hd + List.length q.tl

let fold f acc q =
  let acc' = List.fold_left f acc q.hd in
  List.fold_right (fun x acc -> f acc x) q.tl acc'

let iter f q = fold (fun () x -> f x) () q

let to_seq q = fun k -> iter k q

let of_seq seq = Sequence.fold push empty seq
