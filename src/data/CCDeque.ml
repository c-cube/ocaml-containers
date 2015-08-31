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

(** {1 Imperative deque} *)

type 'a elt = {
  content : 'a;
  mutable prev : 'a elt;
  mutable next : 'a elt;
} (** A cell holding a single element *)

and 'a t = 'a elt option ref
  (** The deque, a double linked list of cells *)

exception Empty

let create () = ref None

let is_empty d =
  match !d with
  | None -> true
  | Some _ -> false

let push_front d x =
  match !d with
  | None ->
    let rec elt = {
      content = x; prev = elt; next = elt;
    } in
    d := Some elt
  | Some first ->
    let elt = { content = x; prev = first.prev; next=first; } in
    first.prev.next <- elt;
    first.prev <- elt;
    d := Some elt

let push_back d x =
  match !d with
  | None ->
    let rec elt = {
      content = x; prev = elt; next = elt; } in
    d := Some elt
  | Some first ->
    let elt = { content = x; next=first; prev=first.prev; } in
    first.prev.next <- elt;
    first.prev <- elt

let peek_front d =
  match !d with
  | None -> raise Empty
  | Some first -> first.content

let peek_back d =
  match !d with
  | None -> raise Empty
  | Some first -> first.prev.content

let take_back d =
  match !d with
  | None -> raise Empty
  | Some first when first == first.prev ->
    (* only one element *)
    d := None;
    first.content
  | Some first ->
    let elt = first.prev in
    elt.prev.next <- first;
    first.prev <- elt.prev;  (* remove [first.prev] from list *)
    elt.content

let take_front d =
  match !d with
  | None -> raise Empty
  | Some first when first == first.prev ->
    (* only one element *)
    d := None;
    first.content
  | Some first ->
    first.prev.next <- first.next; (* remove [first] from list *)
    first.next.prev <- first.prev;
    d := Some first.next;
    first.content

let iter f d =
  match !d with
  | None -> ()
  | Some first ->
    let rec iter elt =
      f elt.content;
      if elt.next != first then iter elt.next
    in
    iter first

(*$T
  let n = ref 0 in iter (fun _ -> incr n) (of_list [1;2;3]); !n = 3
*)

let append_front ~into q = iter (push_front into) q

let append_back ~into q = iter (push_back into) q

(*$R
  let q = of_list [3;4] in
  append_front ~into:q (of_list [2;1]);
  assert_equal [1;2;3;4] (to_list q);
  append_back ~into:q (of_list [5;6]);
  assert_equal [1;2;3;4;5;6] (to_list q);
*)

let fold f acc d =
  match !d with
  | None -> acc
  | Some first ->
    let rec aux acc elt =
      let acc = f acc elt.content in
      if elt.next != first then aux acc elt.next else acc
    in
    aux acc first

(*$T
  fold (+) 0 (of_list [1;2;3]) = 6
  fold (fun acc x -> x::acc) [] (of_list [1;2;3]) = [3;2;1]
*)

let length (d : _ t) =
  match !d with
  | None -> 0
  | Some _ ->
    let r = ref 0 in
    iter (fun _ -> incr r) d;
    !r

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let add_seq_back q seq = seq (fun x -> push_back q x)

let add_seq_front q seq = seq (fun x -> push_front q x)

(*$R
  let q = of_list [4;5] in
  add_seq_front q Sequence.(of_list [3;2;1]);
  assert_equal [1;2;3;4;5] (to_list q);
  add_seq_back q Sequence.(of_list [6;7]);
  assert_equal [1;2;3;4;5;6;7] (to_list q);
*)

let of_seq seq =
  let deque = create () in
  seq (fun x -> push_back deque x);
  deque

let to_seq d k = iter k d

let of_list l =
  let q = create() in
  List.iter (push_back q) l;
  q

(*$R
  let q = of_list [1;2;3] in
  assert_equal 1 (take_front q);
  assert_equal 3 (take_back q);
  assert_equal 2 (take_front q);
  assert_equal true (is_empty q)
*)

let to_rev_list q = fold (fun l x -> x::l) [] q

let to_list q = List.rev (to_rev_list q)

let gen_empty_ () = None
let rec gen_iter_ f g = match g() with
  | None -> ()
  | Some x -> f x; gen_iter_ f g

let of_gen g =
  let q = create () in
  gen_iter_ (fun x -> push_back q x) g;
  q

let to_gen q = match !q with
  | None -> gen_empty_
  | Some q ->
      let cur = ref q in
      let first = ref true in
      fun () ->
        let x = (!cur).content in
        if !cur == q && not !first then None
        else (
          first := false;
          cur := (!cur).next;
          Some x
        )

(*$T
  of_list [1;2;3] |> to_gen |> of_gen |> to_list = [1;2;3]
*)

(*$Q
  Q.(list int) (fun l -> \
    of_list l |> to_gen |> of_gen |> to_list = l)
*)

(* naive implem of copy, for now *)
let copy d =
  let d' = create () in
  iter (fun x -> push_back d' x) d;
  d'

let equal ?(eq=(=)) a b =
  let rec aux eq a b = match a() , b() with
    | None, None -> true
    | None, Some _
    | Some _, None -> false
    | Some x, Some y -> eq x y && aux eq a b
  in aux eq (to_gen a) (to_gen b)

let compare ?(cmp=Pervasives.compare) a b =
  let rec aux cmp a b = match a() , b() with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some x, Some y ->
        let c = cmp x y in
        if c=0 then aux cmp a b else c
  in aux cmp (to_gen a) (to_gen b)

type 'a printer = Format.formatter -> 'a -> unit

let print pp_x out d =
  let first = ref true in
  Format.fprintf out "@[<hov2>deque {";
  iter
    (fun x ->
      if !first then first:= false else Format.fprintf out ";@ ";
      pp_x out x
    ) d;
  Format.fprintf out "}@]"

