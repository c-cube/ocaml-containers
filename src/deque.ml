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

