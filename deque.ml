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

(** Imperative deque *)

type 'a elt = {
  content : 'a;
  mutable prev : 'a elt;
  mutable next : 'a elt;
}

type 'a t = {
  mutable first : 'a elt;
  mutable length : int;
}

exception Empty

let create () = {
  first = Obj.magic None;
  length = 0;
}

let is_empty d = d.length = 0

let length d = d.length

let mk_elt x =
  let rec elt = {
    content = x;
    prev = elt;
    next = elt;
  } in elt

let push_front d x =
  let elt = mk_elt x in
  (if d.length > 0
    then begin 
      d.first.prev <- elt;
      let last = d.first.prev in
      last.next <- elt;
      elt.next <- d.first;
      elt.prev <- last;
    end);
  d.first <- elt;
  d.length <- d.length + 1

let push_back d x =
  let elt = mk_elt x in
  (if d.length > 0
    then begin 
      let last = d.first.prev in
      last.next <- elt;
      d.first.prev <- elt;
      elt.prev <- last;
      elt.next <- d.first;
    end else d.first <- elt);
  d.length <- d.length + 1

let take_back d =
  (if d.length = 0 then raise Empty);
  let elt = d.first.prev in
  let new_last = elt.prev in
  d.length <- d.length - 1;
  new_last.next <- d.first;
  d.first.next <- new_last;
  elt.content

let take_front d =
  (if d.length = 0 then raise Empty);
  let elt = d.first in
  let new_first = elt.next in
  d.length <- d.length - 1;
  let last = d.first.prev in
  new_first.prev <- last;
  last.next <- new_first;
  elt.content

