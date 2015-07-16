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

let length (d : _ t) =
  match !d with
  | None -> 0
  | Some _ ->
    let r = ref 0 in
    iter (fun _ -> incr r) d;
    !r

type 'a sequence = ('a -> unit) -> unit

let of_seq ?(deque=create ()) seq =
  seq (fun x -> push_back deque x);
  deque

let to_seq d k = iter k d

(* naive implem of copy, for now *)
let copy d =
  let d' = create () in
  iter (fun x -> push_back d' x) d;
  d'
