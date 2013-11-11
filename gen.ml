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

(** {2 Global type declarations} *)

exception EOG
  (** End of Generation *)

type 'a t = unit -> 'a
  (** A generator may be called several times, yielding the next value
      each time. It raises EOG when it reaches the end. *)

type 'a gen = 'a t

(** {2 Common signature for transient and restartable generators} *)

module type S = sig
  type 'a t

  val empty : 'a t
    (** Empty generator, with no elements *)

  val singleton : 'a -> 'a t
    (** One-element generator *)

  val repeat : 'a -> 'a t
    (** Repeat same element endlessly *)

  val iterate : 'a -> ('a -> 'a) -> 'a t
    (** [iterate x f] is [[x; f x; f (f x); f (f (f x)); ...]] *)

  val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
    (** Dual of {!fold}, with a deconstructing operation. It keeps on
        unfolding the ['b] value into a new ['b], and a ['a] which is yielded,
        until [None] is returned. *)

  (** {2 Basic combinators} *)

  val is_empty : _ t -> bool
    (** Check whether the enum is empty. *)

  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
    (** Fold on the generator, tail-recursively *)

  val fold2 : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
    (** Fold on the two enums in parallel. Stops once one of the enums
        is exhausted. *)

  val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
    (** Fold on non-empty sequences (otherwise raise Invalid_argument) *)

  val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
    (** Like {!fold}, but keeping successive values of the accumulator *)

  val iter : ('a -> unit) -> 'a t -> unit
    (** Iterate on the enum *)

  val iteri : (int -> 'a -> unit) -> 'a t -> unit
    (** Iterate on elements with their index in the enum, from 0 *)

  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
    (** Iterate on the two sequences. Stops once one of them is exhausted.*)

  val length : _ t -> int
    (** Length of an enum (linear time) *)

  val map : ('a -> 'b) -> 'a t -> 'b t
    (** Lazy map. No iteration is performed now, the function will be called
        when the result is traversed. *)

  val append : 'a t -> 'a t -> 'a t
    (** Append the two enums; the result contains the elements of the first,
        then the elements of the second enum. *)

  val flatten : 'a gen t -> 'a t
    (** Flatten the enumeration of generators *)

  val flatMap : ('a -> 'b gen) -> 'a t -> 'b t
    (** Monadic bind; each element is transformed to a sub-enum
        which is then iterated on, before the next element is processed,
        and so on. *)

  val mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
    (** Is the given element, member of the enum? *)

  val take : int -> 'a t -> 'a t
    (** Take at most n elements *)

  val drop : int -> 'a t -> 'a t
    (** Drop n elements *)

  val nth : int -> 'a t -> 'a
    (** n-th element, or Not_found
        @raise Not_found if the generator contains less than [n] arguments *)

  val filter : ('a -> bool) -> 'a t -> 'a t
    (** Filter out elements that do not satisfy the predicate.  *)

  val takeWhile : ('a -> bool) -> 'a t -> 'a t
    (** Take elements while they satisfy the predicate *)

  val dropWhile : ('a -> bool) -> 'a t -> 'a t
    (** Drop elements while they satisfy the predicate *)

  val filterMap : ('a -> 'b option) -> 'a t -> 'b t
    (** Maps some elements to 'b, drop the other ones *)

  val zipWith : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** Combine common part of the enums (stops when one is exhausted) *)

  val zip : 'a t -> 'b t -> ('a * 'b) t
    (** Zip together the common part of the enums *)

  val zipIndex : 'a t -> (int * 'a) t
    (** Zip elements with their index in the enum *)

  val unzip : ('a * 'b) t -> 'a t * 'b t
    (** Unzip into two sequences, splitting each pair *)

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p l] returns the elements that satisfy [p],
        and the elements that do not satisfy [p] *)

  val for_all : ('a -> bool) -> 'a t -> bool
    (** Is the predicate true for all elements? *)

  val exists : ('a -> bool) -> 'a t -> bool
    (** Is the predicate true for at least one element? *)

  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

  val min : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
    (** Minimum element, according to the given comparison function *)

  val max : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
    (** Maximum element, see {!min} *)

  val eq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** Equality of generators. *)

  val lexico : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Lexicographic comparison of generators. If the common prefix is
        the same, the shortest one is considered as smaller than the other. *)

  val compare : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Synonym for {! lexico} *)

  (** {2 Complex combinators} *)

  val merge : 'a gen t -> 'a t
    (** Pick elements fairly in each sub-generator. The given enum
        must be finite (not its elements, though). The merge of enums
        [e1, e2, ... en] picks one element in [e1], then one element in [e2],
        then in [e3], ..., then in [en], and then starts again at [e1]. Once
        a generator is empty, it is skipped; when they are all empty,
        their merge is also empty. 
        For instance, [merge [1;3;5] [2;4;6]] will be [1;2;3;4;5;6]. *)

  val intersection : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
    (** Intersection of two sorted sequences. Only elements that occur in both
        inputs appear in the output *)

  val sorted_merge : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
    (** Merge two sorted sequences into a sorted sequence *)

  val sorted_merge_n : ?cmp:('a -> 'a -> int) -> 'a gen t -> 'a t
    (** Sorted merge of multiple sorted sequences *)

  val tee : ?n:int -> 'a t -> 'a gen list
    (** Duplicate the enum into [n] generators (default 2). The generators
        share the same underlying instance of the enum, so the optimal case is
        when they are consumed evenly *)

  val round_robin : ?n:int -> 'a t -> 'a gen list
    (** Split the enum into [n] generators in a fair way. Elements with
        [index = k mod n] with go to the k-th enum. [n] default value
        is 2. *)

  val interleave : 'a t -> 'a t -> 'a t
    (** [interleave a b] yields an element of [a], then an element of [b],
        and so on until the end of [a] or [b] is reached. *)

  val intersperse : 'a -> 'a t -> 'a t
    (** Put the separator element between all elements of the given enum *)

  val product : 'a t -> 'b t -> ('a * 'b) t
    (** Cartesian product, in no predictable order. Works even if some of the
        arguments are infinite. *)

  val group : ?eq:('a -> 'a -> bool) -> 'a t -> 'a list t
    (** Group equal consecutive elements together. *)

  val uniq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
    (** Remove consecutive duplicate elements. Basically this is
        like [fun e -> map List.hd (group e)]. *)

  val sort : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
    (** Sort according to the given comparison function. The enum must be finite. *)

  val sort_uniq : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
    (** Sort and remove duplicates. The enum must be finite. *)

  (* TODO later
  val permutations : 'a t -> 'a gen t
    (** Permutations of the enum. Each permutation becomes unavailable once
        the next one is produced. *)

  val combinations : int -> 'a t -> 'a t t
    (** Combinations of given length. *)

  val powerSet : 'a t -> 'a t t
    (** All subsets of the enum (in no particular order) *)
  *)

  (** {2 Basic conversion functions} *)

  val of_list : 'a list -> 'a t
    (** Enumerate elements of the list *)

  val to_list : 'a t -> 'a list
    (** non tail-call trasnformation to list, in the same order *)

  val to_rev_list : 'a t -> 'a list
    (** Tail call conversion to list, in reverse order (more efficient) *)

  val to_array : 'a t -> 'a array
    (** Convert the enum to an array (not very efficient) *)

  val of_array : ?start:int -> ?len:int -> 'a array -> 'a t
    (** Iterate on (a slice of) the given array *)

  val rand_int : int -> int t
    (** Random ints in the given range. *)

  val int_range : int -> int -> int t
    (** [int_range a b] enumerates integers between [a] and [b], included. [a]
        is assumed to be smaller than [b]. *)

  module Infix : sig
    val (--) : int -> int -> int t
      (** Synonym for {! int_range} *)

    val (>>=) : 'a t -> ('a -> 'b gen) -> 'b t
      (** Monadic bind operator *)
  end

  val (--) : int -> int -> int t
    (** Synonym for {! int_range} *)

  val (>>=) : 'a t -> ('a -> 'b gen) -> 'b t
    (** Monadic bind operator *)

  val pp : ?start:string -> ?stop:string -> ?sep:string -> ?horizontal:bool ->
           (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    (** Pretty print the content of the generator on a formatter. *)
end

(** {2 Transient generators} *)

let empty () = raise EOG

let singleton x =
  let first = ref true in
  fun () ->
    if !first then (first := false; x) else raise EOG

let rec repeat x () = x

let repeatedly f () = f ()

let iterate x f =
  let cur = ref x in
  fun () ->
    let x = !cur in
    cur := f !cur;
    x

let next gen = gen ()

let get gen = gen ()

let get_safe gen =
  try Some (gen ())
  with EOG -> None

let junk gen = ignore (gen ())

let rec fold f acc gen =
  let acc, stop =
    try f acc (gen ()), false
    with EOG -> acc, true
  in
  if stop then acc else fold f acc gen

let rec fold2 f acc e1 e2 =
  let acc, stop =
    try f acc (e1()) (e2()), false
    with EOG -> acc, true
  in
  if stop then acc else fold2 f acc e1 e2

let reduce f g =
  let acc = try g () with EOG -> raise (Invalid_argument "reduce") in 
  fold f acc g

(* Dual of {!fold}, with a deconstructing operation *)
let unfold f acc =
  let acc = ref acc in
  fun () ->
    match f !acc with
    | None -> raise EOG
    | Some (x, acc') ->
      acc := acc';
      x

let iter f gen =
  try
    while true do f (gen ()) done
  with EOG ->
    ()

let iteri f gen =
  let n = ref 0 in
  try
    while true do f !n (gen ()); incr n done
  with EOG ->
    ()

let is_empty enum =
  try ignore (enum ()); false
  with EOG -> true

let length gen =
  fold (fun acc _ -> acc + 1) 0 gen

let scan f acc g =
  let acc = ref acc in
  let first = ref true in
  fun () ->
    if !first
      then (first := false; !acc)
      else begin
        acc := f !acc (g ());
        !acc
      end

let iter2 f gen1 gen2 =
  try
    while true do f (gen1 ()) (gen2 ()) done;
  with EOG -> ()

(** {3 Lazy} *)

let map f gen () =
  f (gen ())

let append gen1 gen2 =
  let gen = ref gen1 in
  let first = ref true in
  (* get next element *)
  let rec next () =
    try !gen ()
    with EOG ->
      if !first then begin
        first := false;
        gen := gen2; (* switch to the second generator *)
        next ()
      end else raise EOG  (* done *)
  in next

let flatten next_gen =
  let gen = ref empty in
  (* get next element *)
  let rec next () =
    try !gen ()
    with EOG ->
      (* jump to next sub-enum *)
      gen := next_gen (); 
      next ()
  in next

let flatMap f next_elem =
  let gen = ref empty in
  (* get next element *)
  let rec next () =
    try !gen ()
    with EOG ->
      (* enumerate f (next element) *)
      let x = next_elem () in
      gen := f x;
      next ()  (* try again, with [gen = f x] *)
  in next

let mem ?(eq=(=)) x gen =
  try
    iter (fun y -> if eq x y then raise Exit) gen;
    false
  with Exit -> 
    true

let take n gen =
  assert (n >= 0);
  let count = ref 0 in  (* how many yielded elements *)
  fun () ->
    if !count = n then raise EOG
    else begin incr count; gen () end

let drop n gen =
  assert (n >= 0);
  let dropped = ref false in
  fun () ->
    if !dropped
      then gen()
      else begin
        (* drop [n] elements and yield the next element *)
        dropped := true;
        for i = 0 to n-1 do ignore (gen()) done;
        gen()
      end

let nth n gen =
  assert (n>=0);
  let rec iter i =
    let x = gen () in
    if n = i then x else iter (i+1)
  in
  try iter 0
  with EOG -> raise Not_found

let filter p gen =
  let rec next () =
    (* wrap exception into option, for next to be tailrec *)
    match (try Some (gen ()) with EOG -> None) with
    | None -> raise EOG
    | Some x ->
      if p x
        then x (* yield element *)
        else next ()  (* discard element *)
  in next

let takeWhile p gen =
  let rec next () =
    let x = gen () in
    if p x then x else raise EOG
  in next

let dropWhile p gen =
  let stop_drop = ref false in
  let rec next () =
    let x = gen () in
    if !stop_drop
      then x  (* yield *)
    else if p x
      then next ()  (* continue dropping *)
      else (stop_drop := true; x)  (* stop dropping *)
  in next

let filterMap f gen =
  (* tailrec *)
  let rec next () =
    let x = gen () in
    match f x with
    | None -> next ()
    | Some y -> y
  in next

let zipWith f a b =
  fun () -> f (a()) (b())

let zip a b =
  fun () -> a(), b()

let zipIndex gen =
  let r = ref 0 in
  fun () ->
    let x = gen () in
    let n = !r in
    incr r;
    n, x

let unzip gen =
  let stop = ref false in
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  let next_left () =
    if Queue.is_empty q1
      then if !stop then raise EOG
      else try
        let x, y = gen() in
        Queue.push y q2;
        x
      with EOG -> stop := true; raise EOG
    else Queue.pop q1
  in
  let next_right () =
    if Queue.is_empty q2
      then if !stop then raise EOG
      else try
        let x, y = gen() in
        Queue.push x q1;
        y
      with EOG -> stop := true; raise EOG
    else Queue.pop q2
  in
  next_left, next_right

(* [partition p l] returns the elements that satisfy [p],
   and the elements that do not satisfy [p] *)
let partition p gen =
  let qtrue = Queue.create () in
  let qfalse = Queue.create () in
  let stop = ref false in
  let rec nexttrue () =
    if Queue.is_empty qtrue
      then if !stop then raise EOG
      else try
        let x = gen() in
        if p x then x else (Queue.push x qfalse; nexttrue())
      with EOG -> stop:=true; raise EOG
    else Queue.pop qtrue
  and nextfalse() =
    if Queue.is_empty qfalse
      then if !stop then raise EOG
      else try
        let x = gen() in
        if p x then (Queue.push x qtrue; nextfalse()) else x
      with EOG -> stop:= true; raise EOG
    else Queue.pop qfalse
  in
  nexttrue, nextfalse

exception GenExit

let for_all p gen =
  try
    iter (fun x -> if not (p x) then raise GenExit) gen;
    true
  with GenExit ->
    false

let exists p gen =
  try
    iter (fun x -> if p x then raise GenExit) gen;
    false
  with GenExit ->
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

let min ?(lt=fun x y -> x < y) gen =
  let first = try gen () with EOG -> raise Not_found in
  fold (fun min x -> if lt x min then x else min) first gen

let max ?(lt=fun x y -> x < y) gen =
  let first = try gen () with EOG -> raise Not_found in
  fold (fun max x -> if lt max x then x else max) first gen

let eq ?(eq=(=)) gen1 gen2 =
  let rec check () =
    let x1 = try Some (gen1 ()) with EOG -> None in
    let x2 = try Some (gen2 ()) with EOG -> None in
    match x1, x2 with
    | None, None -> true
    | Some x1, Some x2 when eq x1 x2 -> check ()
    | _ -> false
  in
  check ()

let lexico ?(cmp=Pervasives.compare) gen1 gen2 =
  let rec lexico () =
    let x1 = try Some (gen1 ()) with EOG -> None in
    let x2 = try Some (gen2 ()) with EOG -> None in
    match x1, x2 with
    | None, None -> 0
    | Some x1, Some x2 ->
      let c = cmp x1 x2 in
      if c <> 0 then c else lexico ()
    | Some _, None -> 1
    | None, Some _ -> -1
  in lexico ()

let compare ?cmp gen1 gen2 = lexico ?cmp gen1 gen2

(** {3 Complex combinators} *)

let merge gen =
  (* list of sub-enums *)
  let l = fold (fun acc x -> x::acc) [] gen in
  let l = List.rev l in
  let q = Queue.create () in
  List.iter (fun gen' -> Queue.push gen' q) l;
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

let intersection ?(cmp=Pervasives.compare) gen1 gen2 =
  let next1 () = try Some (gen1 ()) with EOG -> None in
  let next2 () = try Some (gen2 ()) with EOG -> None in
  let x1 = ref (next1 ()) in
  let x2 = ref (next2 ()) in
  let rec next () =
    match !x1, !x2 with
    | None, None -> raise EOG
    | Some y1, Some y2 ->
      let c = cmp y1 y2 in
      if c = 0  (* equal elements, yield! *)
        then (x1 := next1 (); x2 := next2 (); y1)
      else if c < 0 (* drop y1 *)
        then (x1 := next1 (); next ())
      else (* drop y2 *)
        (x2 := next2 (); next ())
    | Some _, None
    | None, Some _ -> raise EOG
  in next

let sorted_merge ?(cmp=Pervasives.compare) gen1 gen2 =
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

(** {4 Mutable heap (taken from heap.ml to avoid dependencies)} *)
module Heap = struct
  type 'a t = {
    mutable tree : 'a tree;
    cmp : 'a -> 'a -> int;
  } (** A pairing tree heap with the given comparison function *)
  and 'a tree =
    | Empty
    | Node of 'a * 'a tree * 'a tree

  let empty ~cmp = {
    tree = Empty;
    cmp;
  }

  let is_empty h =
    match h.tree with
    | Empty -> true
    | Node _ -> false

  let rec union ~cmp t1 t2 = match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | Node (x1, l1, r1), Node (x2, l2, r2) ->
    if cmp x1 x2 <= 0
      then Node (x1, union ~cmp t2 r1, l1)
      else Node (x2, union ~cmp t1 r2, l2)

  let insert h x =
    h.tree <- union ~cmp:h.cmp (Node (x, Empty, Empty)) h.tree

  let pop h = match h.tree with
    | Empty -> raise Not_found
    | Node (x, l, r) ->
      h.tree <- union ~cmp:h.cmp l r;
      x
end

let sorted_merge_n ?(cmp=Pervasives.compare) gen =
  (* make a heap of (value, generator) *)
  let cmp (v1,_) (v2,_) = cmp v1 v2 in
  let heap = Heap.empty ~cmp in
  (* add initial values *)
  iter
    (fun gen' ->
      try
        let x = gen' () in
        Heap.insert heap (x, gen')
      with EOG -> ())
    gen;
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

let round_robin ?(n=2) gen =
  (* array of queues, together with their index *)
  let qs = Array.init n (fun i -> Queue.create ()) in
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
  (* generators *)
  let l = Array.mapi (fun i _ -> (fun () -> next i)) qs in
  Array.to_list l

(* Duplicate the enum into [n] generators (default 2). The generators
   share the same underlying instance of the enum, so the optimal case is
   when they are consumed evenly *)
let tee ?(n=2) gen =
    (* array of queues, together with their index *)
    let qs = Array.init n (fun i -> Queue.create ()) in
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
  (* generators *)
  let l = Array.mapi (fun i _ -> (fun () -> next i)) qs in
  Array.to_list l

let round_robin ?(n=2) gen =
  (* array of queues, together with their index *)
  let qs = Array.init n (fun i -> Queue.create ()) in
  let cur = ref 0 in
  let stop = ref false in
  (* get next element for the i-th queue *)
  let rec next i =
    let q = qs.(i) in
    if Queue.is_empty q
      then update_to_i i  (* consume generator *)
      else Queue.pop q
  (* consume [gen] until some element for [i]-th generator is
     available. It raises EOG if [gen] is exhausted before *)
  and update_to_i i =
    (if !stop then raise EOG);
    let x = try gen () with EOG -> stop := true; raise EOG in
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
  (* generators *)
  let l = Array.mapi (fun i _ -> fun () -> next i) qs in
  Array.to_list l

(* Yield elements from a and b alternatively *)
let interleave gen_a gen_b =
  let left = ref true in  (* left or right? *)
  fun () ->
    if !left
      then (left := false; gen_a ())
      else (left := true; gen_b ())

(* Put [x] between elements of [enum] *)
let intersperse x gen =
  let next_elem = ref None in
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

(* Cartesian product *)
let product gena genb =
  let all_a = ref [] in
  let all_b = ref [] in
  let cur = ref `GetLeft in
  let rec next () =
    match !cur with
    | `Stop -> raise EOG
    | `GetLeft ->
      let xa = try Some (gena()) with EOG -> None in
      begin match xa with
        | None -> cur := `GetRight
        | Some a -> all_a := a :: !all_a; cur := `ProdLeft (a, !all_b)
      end;
      next ()
    | `GetRight ->
      let xb = try Some (genb()) with EOG -> None in
      begin match xb with
        | None -> cur := `Stop; raise EOG
        | Some b -> all_b := b::!all_b; cur := `ProdRight (b, !all_a)
      end;
      next ()
    | `ProdLeft (_, []) ->
      cur := `GetRight;
      next()
    | `ProdLeft (x, y::l) ->
      cur := `ProdLeft (x, l);
      x, y
    | `ProdRight (_, []) ->
      cur := `GetLeft;
      next()
    | `ProdRight (y, x::l) ->
      cur := `ProdRight (y, l);
      x, y
  in
  next

(* Group equal consecutive elements together. *)
let group ?(eq=(=)) gen =
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

let uniq ?(eq=(=)) gen =
  let prev = ref (Obj.magic 0) in
  let first = ref true in
  let rec next () =
    let x = gen () in
    if !first then (first := false; prev := x; x)
    else if eq x !prev then next ()
    else (prev := x; x)
  in next

let sort ?(cmp=Pervasives.compare) gen =
  (* build heap *)
  let h = Heap.empty ~cmp in
  iter (Heap.insert h) gen;
  fun () ->
    if Heap.is_empty h
      then raise EOG
      else Heap.pop h

(* FIXME: use a set *)
let sort_uniq ?(cmp=Pervasives.compare) gen =
  uniq ~eq:(fun x y -> cmp x y = 0) (sort ~cmp gen)

(*
let permutations enum =
  failwith "not implemented" (* TODO *)

let combinations n enum =
  assert (n >= 0);
  failwith "not implemented" (* TODO *)

let powerSet enum =
  failwith "not implemented"
*)

(** {3 Conversion} *)

let of_list l =
  let l = ref l in
  fun () ->
    match !l with
    | [] -> raise EOG
    | x::l' -> l := l'; x

let to_rev_list gen =
  fold (fun acc x -> x :: acc) [] gen

let to_list gen = List.rev (to_rev_list gen)

let to_array gen =
  let l = to_rev_list gen in
  let a = Array.of_list l in
  let n = Array.length a in
  for i = 0 to (n-1) / 2 do
    let tmp = a.(i) in
    a.(i) <- a.(n-i-1);
    a.(n-i-1) <- tmp
  done;
  a

let of_array ?(start=0) ?len a =
  let len = match len with
  | None -> Array.length a - start
  | Some n -> assert (n + start < Array.length a); n in
  let i = ref start in
  fun () ->
    if !i >= start + len
      then raise EOG
      else (let x = a.(!i) in incr i; x)

let rand_int i =
  repeatedly (fun () -> Random.int i)

let int_range i j =
  let r = ref i in
  fun () ->
    let x = !r in
    if x > j then raise EOG
      else begin
        incr r;
        x
      end

let pp ?(start="") ?(stop="") ?(sep=",") ?(horizontal=false) pp_elem formatter gen =
  (if horizontal
    then Format.pp_open_hbox formatter ()
    else Format.pp_open_hvbox formatter 0);
  Format.pp_print_string formatter start;
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
  let (--) = int_range

  let (>>=) x f = flatMap f x
end

include Infix

module Restart = struct
  type 'a t = unit -> 'a gen

  type 'a restartable = 'a t

  let lift f e = f (e ())
  let lift2 f e1 e2 = f (e1 ()) (e2 ())

  let empty () = empty

  let singleton x () = singleton x

  let iterate x f () = iterate x f

  let repeat x () = repeat x

  let repeatedly f () = repeatedly f

  let unfold f acc () = unfold f acc 

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

  let is_empty e = is_empty (e ())

  let fold f acc e = fold f acc (e ())

  let fold2 f acc e1 e2 = fold2 f acc (e1 ()) (e2 ())

  let reduce f e = reduce f (e ())

  let scan f acc e () = scan f acc (e ())

  let iter f e = iter f (e ())

  let iteri f e = iteri f (e ())

  let iter2 f e1 e2 = iter2 f (e1 ()) (e2 ())

  let length e = length (e ())

  let map f e () = map f (e ())

  let append e1 e2 () = append (e1 ()) (e2 ())

  let flatten e () = flatten (e ())

  let flatMap f e () = flatMap f (e ())

  let mem ?eq x e = mem ?eq x (e ())

  let take n e () = take n (e ())

  let drop n e () = drop n (e ())

  let nth n e = nth n (e ())

  let filter p e () = filter p (e ())

  let takeWhile p e () = takeWhile p (e ())

  let dropWhile p e () = dropWhile p (e ())

  let filterMap f e () = filterMap f (e ())

  let zipWith f e1 e2 () = zipWith f (e1 ()) (e2 ())

  let zip e1 e2 () = zip (e1 ()) (e2 ())

  let zipIndex e () = zipIndex (e ())

  let unzip e = map fst e, map snd e

  let partition p e =
    filter p e, filter (fun x -> not (p x)) e

  let for_all p e =
    for_all p (e ())

  let exists p e =
    exists p (e ())

  let for_all2 p e1 e2 =
    for_all2 p (e1 ()) (e2 ())

  let exists2 p e1 e2 =
    exists2 p (e1 ()) (e2 ())

  let min ?lt e = min ?lt (e ())

  let max ?lt e = max ?lt (e ())

  let ___eq = eq
  let eq ?eq e1 e2 = ___eq ?eq (e1 ()) (e2 ())

  let lexico ?cmp e1 e2 = lexico ?cmp (e1 ()) (e2 ())

  let compare ?cmp e1 e2 = compare ?cmp (e1 ()) (e2 ())

  let merge e () = merge (e ())

  let intersection ?cmp e1 e2 () =
    intersection ?cmp (e1 ()) (e2 ())

  let sorted_merge ?cmp e1 e2 () =
    sorted_merge ?cmp (e1 ()) (e2 ())

  let sorted_merge_n ?cmp e () =
    sorted_merge_n ?cmp (e ())

  let tee ?n e = tee ?n (e ())

  let round_robin ?n e = round_robin ?n (e ())

  let interleave e1 e2 () = interleave (e1 ()) (e2 ())

  let intersperse x e () = intersperse x (e ())

  let product e1 e2 () = product (e1 ()) (e2 ())

  let group ?eq e () = group ?eq (e ())

  let uniq ?eq e () = uniq ?eq (e ())

  let sort ?(cmp=Pervasives.compare) enum =
    fun () ->
      (* build heap *)
      let h = Heap.empty ~cmp in
      iter (Heap.insert h) enum;
      fun () ->
        if Heap.is_empty h
          then raise EOG
          else Heap.pop h

  let sort_uniq ?(cmp=Pervasives.compare) e =
    let e' = sort ~cmp e in
    uniq ~eq:(fun x y -> cmp x y = 0) e'

  let of_list l () = of_list l

  let to_rev_list e = to_rev_list (e ())
  
  let to_list e = to_list (e ())

  let to_array e = to_array (e ())

  let of_array ?start ?len a () = of_array ?start ?len a

  let rand_int i () = rand_int i

  let int_range i j () = int_range i j

  module Infix = struct
    let (--) = int_range

    let (>>=) x f = flatMap f x
  end

  include Infix

  let pp ?start ?stop ?sep ?horizontal pp_elem fmt e =
    pp ?start ?stop ?sep ?horizontal pp_elem fmt (e ())
end

(** {2 Generator functions} *)

let start g = g ()

(** {4 Mutable double-linked list, similar to {! Deque.t} *)
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
