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

(** {1 Restartable generators} *)

exception EOG
  (** End of Generation *)

type 'a t = unit -> 'a generator
  (** An enum is a generator of generators *)
and 'a generator = unit -> 'a
  (** A generator may be called several times, yielding the next value
      each time. It raises EOG when it reaches the end. *)

(** {2 Generator functions} *)

let start enum = enum ()

(** {2 Transient generators} *)

module Gen = struct
  let empty () = raise EOG

  let next gen = gen ()

  let junk gen = ignore (gen ())

  let fold f acc gen =
    let acc = ref acc in
    (try
      while true do acc := f !acc (gen ()) done
    with EOG -> ());
    !acc

  let iter f gen =
    try
      while true do f (gen ()) done
    with EOG ->
      ()

  let length gen =
    fold (fun acc _ -> acc + 1) 0 gen

  let of_list l =
    let l = ref l in
    fun () ->
      match !l with
      | [] -> raise EOG
      | x::l' -> l := l'; x

  (* non-tailrec construction of (small) list *)
  let to_list gen =
    let rec fold () =
      try
        let x = gen () in
        x :: fold ()
      with EOG -> []
    in fold ()

  let to_rev_list gen =
    fold (fun acc x -> x :: acc) [] gen

  let int_range i j =
    let r = ref i in
    fun () ->
      let x = !r in
      if x > j then raise EOG
        else begin
          incr r;
          x
        end
end

(** {2 Basic constructors} *)

let empty () = fun () -> raise EOG

let singleton x =
  fun () ->
    let stop = ref false in
    fun () ->
      if !stop
        then raise EOG
        else begin stop := true; x end

let repeat x =
  let f () = x in
  fun () -> f

(** [iterate x f] is [[x; f x; f (f x); f (f (f x)); ...]] *)
let iterate x f =
  fun () ->
    let acc = ref x in
    fun () ->
      let cur = !acc in
      acc := f cur;
      cur

(** Dual of {!fold}, with a deconstructing operation *)
let unfold f acc =
  fun () ->
    let acc = ref acc in
    fun () ->
      match f !acc with
      | None -> raise EOG
      | Some (x, acc') ->
        acc := acc';
        x

(** {2 Basic combinators} *)

let is_empty enum =
  try ignore ((enum ()) ()); false
  with EOG -> true

let fold f acc enum =
  Gen.fold f acc (enum ())

let fold2 f acc e1 e2 =
  let acc = ref acc in
  let gen1 = e1 () and gen2 = e2 () in
  (try
    while true do acc := f !acc (gen1 ()) (gen2 ()) done
  with EOG -> ());
  !acc

let reduce f enum =
  let gen = enum () in
  let acc = try gen () with EOG -> raise (Invalid_argument "reduce") in 
  Gen.fold f acc gen

(** Successive values of the accumulator *)
let scan f acc e =
  fun () ->
    let acc = ref acc in
    let first = ref true in
    let gen = e () in
    fun () ->
      if !first
        then (first := false; !acc)
        else begin
          acc := f !acc (gen ());
          !acc
        end

let iter f enum =
  Gen.iter f (enum ())

let iter2 f e1 e2 =
  let gen1 = e1 () and gen2 = e2 () in
  try
    while true do f (gen1 ()) (gen2 ()) done;
  with EOG -> ()

let length enum =
  Gen.length (enum ())
              
let map f enum =
  (* another enum *)
  fun () ->
    let gen = enum () in
    (* the mapped generator *)
    fun () ->
      f (gen ())

let append e1 e2 =
  fun () ->
    let gen = ref (e1 ()) in
    let first = ref true in
    (* get next element *)
    let rec next () =
      try !gen ()
      with EOG ->
        if !first then begin
          first := false;
          gen := e2 ();  (* switch to the second generator *)
          next ()
        end else raise EOG  (* done *)
    in next

let cycle enum =
  assert (not (is_empty enum));
  fun () ->
    let gen = ref (enum ()) in
    let rec next () =
      try !gen ()
      with EOG ->
        gen := enum ();
        next ()
    in next

let flatten enum =
  fun () ->
    let next_gen = enum () in
    let gen = ref Gen.empty in
    (* get next element *)
    let rec next () =
      try !gen ()
      with EOG ->
        (* jump to next sub-enum *)
        gen := (next_gen ()) ();
        next ()
    in next
      
let flatMap f enum =
  fun () ->
    let next_elem = enum () in
    let gen = ref Gen.empty in
    (* get next element *)
    let rec next () =
      try !gen ()
      with EOG ->
        (* enumerate f (next element) *)
        let x = next_elem () in
        gen := (f x) ();
        next ()  (* try again, with [gen = f x] *)
    in next

let mem ?(eq=(=)) x enum =
  try
    iter (fun y -> if eq x y then raise Exit) enum;
    false
  with Exit -> 
    true

let take n enum =
  assert (n >= 0);
  fun () ->
    let gen = enum () in
    let count = ref 0 in  (* how many yielded elements *)
    fun () ->
      if !count = n then raise EOG
      else begin incr count; gen () end

let drop n enum =
  assert (n >= 0);
  fun () ->
    let gen = enum () in
    let count = ref 0 in  (* how many droped elements? *)
    let rec next () =
      if !count < n
        then begin incr count; Gen.junk gen; next () end
        else gen ()
    in next

let nth n enum =
  assert (n>=0);
  let gen = enum () in
  let rec iter i =
    let x = gen () in
    if n = i then x else iter (i+1)
  in
  try iter 0
  with EOG -> raise Not_found

let filter p enum =
  fun () ->
    let gen = enum () in
    let rec next () =
      (* wrap exception into option, for next to be tailrec *)
      match (try Some (gen ()) with EOG -> None) with
      | None -> raise EOG
      | Some x ->
        if p x
          then x (* yield element *)
          else next ()  (* discard element *)
    in next

let takeWhile p enum =
  fun () ->
    let gen = enum () in
    let rec next () =
      let x = gen () in
      if p x then x else raise EOG
    in next

let dropWhile p enum =
  fun () ->
    let gen = enum () in
    let stop_drop = ref false in
    let rec next () =
      let x = gen () in
      if !stop_drop
        then x  (* yield *)
      else if p x
        then next ()  (* continue dropping *)
        else (stop_drop := true; x)  (* stop dropping *)
    in next

let filterMap f enum =
  fun () ->
    let gen = enum () in
    (* tailrec *)
    let rec next () =
      let x = gen () in
      match f x with
      | None -> next ()
      | Some y -> y
    in next

let zipWith f a b =
  fun () ->
    let gen_a = a () in
    let gen_b = b () in
    fun () ->
      f (gen_a ()) (gen_b ())  (* combine elements *)

let zip a b = zipWith (fun x y -> x,y) a b

let zipIndex enum =
  fun () ->
    let r = ref 0 in
    let gen = enum () in
    fun () ->
      let x = gen () in
      let n = !r in
      incr r;
      n, x

let unzip e =
  map fst e, map snd e

(** [partition p l] returns the elements that satisfy [p],
    and the elements that do not satisfy [p] *)
let partition p enum =
  filter p enum, filter (fun x -> not (p x)) enum

let for_all p enum =
  try
    iter (fun x -> if not (p x) then raise Exit) enum;
    true
  with Exit ->
    false

let exists p enum =
  try
    iter (fun x -> if p x then raise Exit) enum;
    false
  with Exit ->
    true

let for_all2 p e1 e2 =
  try
    iter2 (fun x y -> if not (p x y) then raise Exit) e1 e2;
    true
  with Exit ->
    false

let exists2 p e1 e2 =
  try
    iter2 (fun x y -> if p x y then raise Exit) e1 e2;
    false
  with Exit ->
    true

let min ?(lt=fun x y -> x < y) enum =
  let gen = enum () in
  let first = try gen () with EOG -> raise Not_found in
  Gen.fold (fun min x -> if lt x min then x else min) first gen

let max ?(lt=fun x y -> x < y) enum =
  let gen = enum () in
  let first = try gen () with EOG -> raise Not_found in
  Gen.fold (fun max x -> if lt max x then x else max) first gen

(** {2 Complex combinators} *)

(** Pick elements fairly in each sub-enum *)
let merge enum =
  (* list of sub-enums *)
  let l = fold (fun acc x -> x::acc) [] enum in
  let l = List.rev l in
  fun () ->
    let q = Queue.create () in
    List.iter (fun enum' -> Queue.push (enum' ()) q) l;
    (* recursive function to get next element *)
    let rec next () =
      if Queue.is_empty q
        then raise EOG
        else
          let gen = Queue.pop q in
          match (try Some (gen ()) with EOG -> None) with
          | None -> next ()  (* exhausted generator, drop it *)
          | Some x ->
            Queue.push gen q; (* put generator at the end, return x *)
            x
    in next

(** {3 Mutable heap (taken from heap.ml to avoid dependencies)} *)
module Heap = struct
  type 'a t = {
    mutable tree : 'a tree;
    cmp : 'a -> 'a -> int;
  } (** A splay tree heap with the given comparison function *)
  and 'a tree =
    | Empty
    | Node of ('a tree * 'a * 'a tree)
    (** A splay tree containing values of type 'a *)

  let empty ~cmp = {
    tree = Empty;
    cmp;
  }

  let is_empty h =
    match h.tree with
    | Empty -> true
    | Node _ -> false

  (** Partition the tree into (elements <= pivot, elements > pivot) *)
  let rec partition ~cmp pivot tree =
    match tree with
    | Empty -> Empty, Empty
    | Node (a, x, b) ->
      if cmp x pivot <= 0
        then begin
          match b with
          | Empty -> (tree, Empty)
          | Node (b1, y, b2) ->
            if cmp y pivot <= 0
              then
                let small, big = partition ~cmp pivot b2 in
                Node (Node (a, x, b1), y, small), big
              else
                let small, big = partition ~cmp pivot b1 in
                Node (a, x, small), Node (big, y, b2)
        end else begin
          match a with
          | Empty -> (Empty, tree)
          | Node (a1, y, a2) ->
            if cmp y pivot <= 0
              then
                let small, big = partition ~cmp pivot a2 in
                Node (a1, y, small), Node (big, x, b)
              else
                let small, big = partition ~cmp pivot a1 in
                small, Node (big, y, Node (a2, x, b))
        end

  (** Insert the element in the tree *)
  let insert h x =
    let small, big = partition ~cmp:h.cmp x h.tree in
    let tree' = Node (small, x, big) in
    h.tree <- tree'

  (** Get minimum value and remove it from the tree *)
  let pop h =
    let rec delete_min tree = match tree with
    | Empty -> raise Not_found
    | Node (Empty, x, b) -> x, b
    | Node (Node (Empty, x, b), y, c) ->
      x, Node (b, y, c)  (* rebalance *)
    | Node (Node (a, x, b), y, c) ->
      let m, a' = delete_min a in
      m, Node (a', x, Node (b, y, c))
    in
    let m, tree' = delete_min h.tree in
    h.tree <- tree';
    m
end

(** Binary sorted merge of two sorted sequences *)
let sorted_merge ?(cmp=compare) e1 e2 =
  fun () ->
    let gen1, gen2 = e1 (), e2 () in
    let next1 () = try Some (gen1 ()) with EOG -> None in
    let next2 () = try Some (gen2 ()) with EOG -> None in
    let x1 = ref (next1 ()) in
    let x2 = ref (next2 ()) in
    fun () ->
      match !x1, !x2 with
      | None, None -> raise EOG
      | Some y1, Some y2 ->
        if cmp y1 y2 <= 0
          then (x1 := next1 (); y1)
          else (x2 := next2 (); y2)
      | Some y1, None ->
        x1 := next1 ();
        y1
      | None, Some y2 ->
        x2 := next2 ();
        y2

(** Assuming subsequences are sorted in increasing order, merge them
    into an increasing sequence *)
let sorted_merge_n ?(cmp=compare) enum =
  fun () ->
    (* make a heap of (value, generator) *)
    let cmp (v1,_) (v2,_) = cmp v1 v2 in
    let heap = Heap.empty ~cmp in
    (* add initial values *)
    iter
      (fun enum' ->
        let gen = enum' () in
        try
          let x = gen () in
          Heap.insert heap (x, gen)
        with EOG -> ())
      enum;
    fun () ->
      if Heap.is_empty heap then raise EOG
      else begin
        let x, gen = Heap.pop heap in
        try
          let y = gen () in
          Heap.insert heap (y, gen);  (* insert next value *)
          x
        with EOG ->
          x  (* gen is empty *)
      end

(** {3 Mutable double-linked list, similar to {! Deque.t} *)
module MList = struct
  type 'a t = 'a node option ref
  and 'a node = {
    content : 'a;
    mutable prev : 'a node;
    mutable next : 'a node;
  }

  let create () = ref None

  let is_empty d =
    match !d with
    | None -> true
    | Some _ -> false

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

  (* conversion to enum *)
  let to_enum d =
    fun () ->
      match !d with
      | None -> (fun () -> raise EOG)
      | Some first ->
        let cur = ref first in (* current elemnt of the list *)
        let stop = ref false in (* are we done yet? *)
        (fun () ->
          (if !stop then raise EOG);
          let x = (!cur).content in
          cur := (!cur).next;
          (if !cur == first then stop := true); (* EOG, we made a full cycle *)
          x)
end

(** Store content of the generator in an enum *)
let persistent gen =
  let l = MList.create () in
  (try
    while true do MList.push_back l (gen ()); done
  with EOG ->
    ());
  (* done recursing through the generator *)
  MList.to_enum l

let round_robin ?(n=2) enum =
  fun () ->
    (* array of queues, together with their index *)
    let qs = Array.init n (fun i -> Queue.create ()) in
    let gen = enum () in  (* unique generator! *)
    let cur = ref 0 in
    (* get next element for the i-th queue *)
    let rec next i =
      let q = qs.(i) in
      if Queue.is_empty q
        then update_to_i i  (* consume generator *)
        else Queue.pop q
    (* consume [gen] until some element for [i]-th generator is
       available. It raises EOG if [gen] is exhausted before *)
    and update_to_i i =
      let x = gen () in
      let j = !cur in
      cur := (j+1) mod n;  (* move cursor to next generator *)
      let q = qs.(j) in
      if j = i
        then begin
          assert (Queue.is_empty q);
          x  (* return the element *)
        end else begin
          Queue.push x q;
          update_to_i i  (* continue consuming [gen] *)
        end
    in
    (* generator of generators *)
    let i = ref 0 in
    fun () ->
      let j = !i in
      if j = n then raise EOG else (incr i; fun () -> next j)

(** Duplicate the enum into [n] generators (default 2). The generators
    share the same underlying instance of the enum, so the optimal case is
    when they are consumed evenly *)
let tee ?(n=2) enum =
  fun () ->
    (* array of queues, together with their index *)
    let qs = Array.init n (fun i -> Queue.create ()) in
    let gen = enum () in  (* unique generator! *)
    let finished = ref false in (* is [gen] exhausted? *)
    (* get next element for the i-th queue *)
    let rec next i =
      if Queue.is_empty qs.(i)
        then
          if !finished then raise EOG
          else get_next i  (* consume generator *)
        else Queue.pop qs.(i)
    (* consume one more element *)
    and get_next i =
      try
        let x = gen () in
        for j = 0 to n-1 do
          if j <> i then Queue.push x qs.(j)
        done;
        x
      with EOG ->
        finished := true;
        raise EOG
    in
    (* generator of generators *)
    let i = ref 0 in
    fun () ->
      let j = !i in
      if j = n then raise EOG else (incr i; fun () -> next j)

(** Yield elements from a and b alternatively *)
let interleave a b =
  fun () ->
    let gen_a = a () in
    let gen_b = b () in
    let left = ref true in  (* left or right? *)
    fun () ->
      if !left
        then (left := false; gen_a ())
        else (left := true; gen_b ())

(** Put [x] between elements of [enum] *)
let intersperse x enum =
  fun () ->
    let next_elem = ref None in
    let gen = enum () in
    (* must see whether the gen is empty (first element must be from enum) *)
    try
      next_elem := Some (gen ());
      (* get next element *)
      let rec next () =
        match !next_elem with
        | None -> next_elem := Some (gen ()); x  (* yield x, gen is not exhausted *)
        | Some y -> next_elem := None; y (* yield element of gen *)
      in next
    with EOG ->
      fun () -> raise EOG

(** Cartesian product *)
let product a b =
  fun () ->
    if is_empty a || is_empty b then fun () -> raise EOG
    else
      (* [a] is the outer relation *)
      let gen_a = a () in
      (* current element of [a] *)
      let cur_a = ref (gen_a ()) in
      let gen_b = ref (b ()) in
      let rec next () =
        try !cur_a, !gen_b ()
        with EOG ->
          (* gen_b exhausted, get next elem of [a] *)
          cur_a := gen_a ();
          gen_b := b ();
          next ()
      in
      next

(** Group equal consecutive elements together. *)
let group ?(eq=(=)) enum =
  fun () ->
    let gen = enum () in
    try
      let cur = ref [gen ()] in
      let rec next () =
        (* try to get an element *)
        let next_x =
          if !cur = []
            then None
            else try Some (gen ()) with EOG -> None in
        match next_x, !cur with
        | None, [] -> raise EOG
        | None, l ->
          cur := [];
          l
        | Some x, y::_ when eq x y ->
          cur := x::!cur;
          next ()  (* same group *)
        | Some x, l ->
          cur := [x];
          l
      in next
    with EOG ->
      fun () -> raise EOG

let uniq ?(eq=(=)) enum =
  fun () ->
    let gen = enum () in
    let prev = ref (Obj.magic 0) in
    let first = ref true in
    let rec next () =
      let x = gen () in
      if !first then (first := false; prev := x; x)
      else if eq x !prev then next ()
      else (prev := x; x)
    in next

let sort ?(cmp=compare) enum =
  fun () ->
    (* build heap *)
    let h = Heap.empty ~cmp in
    iter (Heap.insert h) enum;
    fun () ->
      if Heap.is_empty h
        then raise EOG
        else Heap.pop h

let sort_uniq ?(cmp=compare) enum =
  uniq ~eq:(fun x y -> cmp x y = 0) (sort ~cmp enum)

(*
let permutations enum =
  failwith "not implemented" (* TODO *)

let combinations n enum =
  assert (n >= 0);
  failwith "not implemented" (* TODO *)

let powerSet enum =
  failwith "not implemented"
*)

(** {2 Basic conversion functions} *)

let to_list enum =
  Gen.to_list (enum ())
    
let of_list l =
  fun () ->
    Gen.of_list l

let to_rev_list enum =
  Gen.to_rev_list (enum ())

let int_range i j =
  fun () -> Gen.int_range i j

let pp ?(start="") ?(stop="") ?(sep=",") ?(horizontal=false) pp_elem formatter enum =
  (if horizontal
    then Format.pp_open_hbox formatter ()
    else Format.pp_open_hvbox formatter 0);
  Format.pp_print_string formatter start;
  let gen = enum () in
  let rec next is_first =
    let continue_ =
      try
        let x = gen () in
        (if not is_first
          then begin
            Format.pp_print_string formatter sep;
            Format.pp_print_space formatter ();
            pp_elem formatter x
          end else pp_elem formatter x);
        true
      with EOG -> false in
    if continue_ then next false
  in
  next true;
  Format.pp_print_string formatter stop;
  Format.pp_close_box formatter ()

module Infix = struct
  let (@@) = append

  let (>>=) e f = flatMap f e

  let (--) = int_range

  let (|>) x f = f x
end

include Infix
