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

type 'a t = unit -> 'a option

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

  val init : ?limit:int -> (int -> 'a) -> 'a t
    (** Calls the function, starting from 0, on increasing indices.
        If [limit] is provided and is a positive int, iteration will
        stop at the limit (excluded).
        For instance [init ~limit:4 id] will yield 0, 1, 2, and 3. *)

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

  val min : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
    (** Minimum element, according to the given comparison function.
        @raise Invalid_argument if the generator is empty *)

  val max : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
    (** Maximum element, see {!min}
        @raise Invalid_argument if the generator is empty *)

  val eq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** Equality of generators. *)

  val lexico : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Lexicographic comparison of generators. If a generator is a prefix
        of the other one, it is considered smaller. *)

  val compare : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Synonym for {! lexico} *)

  val find : ('a -> bool) -> 'a t -> 'a option
    (** [find p e] returns the first element of [e] to satisfy [p],
        or None. *)

  val sum : int t -> int
    (** Sum of all elements *)

  (** {2 Multiple iterators} *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit

  val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc

  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** Succeeds if all pairs of elements satisfy the predicate.
        Ignores elements of an iterator if the other runs dry. *)

  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** Succeeds if some pair of elements satisfy the predicate.
        Ignores elements of an iterator if the other runs dry. *)

  val zipWith : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** Combine common part of the enums (stops when one is exhausted) *)

  val zip : 'a t -> 'b t -> ('a * 'b) t
    (** Zip together the common part of the enums *)

  (** {2 Complex combinators} *)

  val merge : 'a gen t -> 'a t
    (** Pick elements fairly in each sub-generator. The merge of enums
        [e1, e2, ... ] picks elements in [e1], [e2],
        in [e3], [e1], [e2] .... Once a generator is empty, it is skipped;
        when they are all empty, and none remains in the input,
        their merge is also empty. 
        For instance, [merge [1;3;5] [2;4;6]] will be, in disorder, [1;2;3;4;5;6]. *)

  val intersection : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
    (** Intersection of two sorted sequences. Only elements that occur in both
        inputs appear in the output *)

  val sorted_merge : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
    (** Merge two sorted sequences into a sorted sequence *)

  val sorted_merge_n : ?cmp:('a -> 'a -> int) -> 'a t list -> 'a t
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
        and so on. When a generator is exhausted, this behaves like the
        other generator. *)

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

  val chunks : int -> 'a t -> 'a array t
    (** [chunks n e] returns a generator of arrays of length [n], composed
        of successive elements of [e]. The last array may be smaller
        than [n] *)

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

let empty () = None

let singleton x =
  let first = ref true in
  fun () ->
    if !first then (first := false; Some x) else None

let rec repeat x () = Some x

let repeatedly f () = Some (f ())

let iterate x f =
  let cur = ref x in
  fun () ->
    let x = !cur in
    cur := f !cur;
    Some x

let next gen = gen ()

let get gen = gen ()

let get_exn gen =
  match gen () with
  | Some x -> x
  | None -> raise (Invalid_argument "Gen.get_exn")

let junk gen = ignore (gen ())

let rec fold f acc gen =
  match gen () with
  | None -> acc
  | Some x -> fold f (f acc x) gen

let reduce f g =
  let acc = match g () with
    | None -> raise (Invalid_argument "reduce")
    | Some x -> x
  in 
  fold f acc g

(* Dual of {!fold}, with a deconstructing operation *)
let unfold f acc =
  let acc = ref acc in
  fun () ->
    match f !acc with
    | None -> None
    | Some (x, acc') ->
      acc := acc';
      Some x

let init ?(limit=max_int) f =
  let r = ref 0 in
  fun () ->
    if !r >= limit
    then None
    else
      let x = f !r in
      let _ = incr r in
      Some x

let rec iter f gen =
  match gen() with
  | None -> ()
  | Some x -> f x; iter f gen

let iteri f gen =
  let rec iteri i = match gen() with
  | None -> ()
  | Some x -> f i x; iteri (i+1)
  in
  iteri 0

let is_empty gen = match gen () with
  | None -> true
  | Some _ -> false

let length gen =
  fold (fun acc _ -> acc + 1) 0 gen

(* useful state *)
type 'a run_state =
  | Init
  | Run of 'a
  | Stop

let scan f acc g =
  let state = ref Init in
  fun () ->
    match !state with
    | Init ->
        state := Run acc;
        Some acc
    | Stop -> None
    | Run acc ->
        match g() with
        | None -> state := Stop; None
        | Some x ->
            let acc' = f acc x in
            state := Run acc';
            Some acc'

let rec iter2 f gen1 gen2 =
  match gen1(), gen2() with
  | Some x, Some y -> f x y; iter2 f gen1 gen2
  | _ -> ()

(** {3 Lazy} *)

let map f gen =
  let stop = ref false in
  fun () ->
    if !stop then None
    else match gen() with
    | None -> stop:= true; None
    | Some x -> Some (f x)

let append gen1 gen2 =
  let first = ref true in
  let rec next() =
    if !first
    then match gen1() with
    | (Some _) as x -> x
    | None -> first:=false; next()
    else gen2()
  in next

let flatten next_gen =
  let state = ref Init in
  (* get next element *)
  let rec next () =
    match !state with
    | Init -> get_next_gen()
    | Run gen ->
      begin match gen () with
      | None -> get_next_gen ()
      | (Some _) as x -> x
      end
    | Stop -> None
  and get_next_gen() = match next_gen() with
    | None -> state := Stop; None
    | Some gen -> state := Run gen; next()
  in
  next

let flatMap f next_elem =
  let state = ref Init in
  let rec next() =
    match !state with
    | Init -> get_next_gen()
    | Run gen ->
      begin match gen () with
      | None -> get_next_gen ()
      | (Some _) as x -> x
      end
    | Stop -> None
  and get_next_gen() = match next_elem() with
    | None -> state:=Stop; None
    | Some x ->
        try state := Run (f x); next()
        with e -> state := Stop; raise e
  in
  next

let mem ?(eq=(=)) x gen =
  let rec mem eq x gen =
    match gen() with
    | Some y -> eq x y || mem eq x gen
    | None -> false
  in mem eq x gen

let take n gen =
  assert (n >= 0);
  let count = ref 0 in  (* how many yielded elements *)
  fun () ->
    if !count = n || !count = ~-1
    then None
    else match gen() with
      | None -> count := ~-1; None   (* indicate stop *)
      | (Some _) as x -> incr count; x

(* call [gen] at most [n] times, and stop *)
let rec __drop n gen =
  if n = 0 then ()
  else match gen() with
    | Some _ -> __drop (n-1) gen
    | None -> ()

let drop n gen =
  assert (n >= 0);
  let dropped = ref false in
  fun () ->
    if !dropped
      then gen()
      else begin
        (* drop [n] elements and yield the next element *)
        dropped := true;
        __drop n gen;
        gen()
      end

let nth n gen =
  assert (n>=0);
  __drop n gen;
  match gen () with
  | None -> raise Not_found
  | Some x -> x

let filter p gen =
  let rec next () =
    (* wrap exception into option, for next to be tailrec *)
    match gen() with
    | None -> None
    | (Some x) as res ->
      if p x
        then res (* yield element *)
        else next ()  (* discard element *)
  in next

let takeWhile p gen =
  let stop = ref false in
  let rec next () =
    if !stop
    then None
    else match gen() with
    | (Some x) as res ->
        if p x then res else (stop := true; None)
    | None -> stop:=true; None
  in next

module DropWhileState = struct
  type t =
    | Stop
    | Drop
    | Yield
end

let dropWhile p gen =
  let open DropWhileState in
  let state = ref Stop in
  let rec next () =
    match !state with
    | Stop -> None
    | Drop ->
        begin match gen () with
        | None -> state := Stop; None
        | (Some x) as res ->
            if p x then next() else (state:=Yield; res)
        end
    | Yield ->
        begin match gen () with
        | None -> state := Stop; None
        | (Some x) as res -> res
        end
  in next

let filterMap f gen =
  (* tailrec *)
  let rec next () =
    match gen() with
    | None -> None
    | Some x ->
        match f x with
        | None -> next()
        | (Some _) as res -> res
  in next

let zipIndex gen =
  let r = ref ~-1 in
  fun () ->
    match gen() with
    | None -> None
    | Some x ->
        incr r;
        Some (!r, x)

let unzip gen =
  let stop = ref false in
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  let next_left () =
    if Queue.is_empty q1
      then if !stop then None
      else match gen() with
      | Some (x,y) ->
        Queue.push y q2;
        Some x
      | None -> stop := true; None
    else Some (Queue.pop q1)
  in
  let next_right () =
    if Queue.is_empty q2
      then if !stop then None
      else match gen() with
      | Some (x,y) ->
        Queue.push x q1;
        Some y
      | None -> stop := true; None
    else Some (Queue.pop q2)
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
      then if !stop then None
      else match gen() with
      | (Some x) as res ->
        if p x then res else (Queue.push x qfalse; nexttrue())
      | None -> stop:=true; None
    else Some (Queue.pop qtrue)
  and nextfalse() =
    if Queue.is_empty qfalse
      then if !stop then None
      else match gen() with
      | (Some x) as res ->
        if p x then (Queue.push x qtrue; nextfalse()) else res
      | None -> stop:= true; None
    else Some (Queue.pop qfalse)
  in
  nexttrue, nextfalse

let rec for_all p gen =
  match gen() with
  | None -> true
  | Some x -> p x && for_all p gen

let rec exists p gen =
  match gen() with
  | None -> false
  | Some x -> p x || exists p gen

let min ?(lt=fun x y -> x < y) gen =
  let first = match gen () with
    | Some x -> x
    | None -> raise (Invalid_argument "min")
  in
  fold (fun min x -> if lt x min then x else min) first gen

let max ?(lt=fun x y -> x < y) gen =
  let first = match gen () with
    | Some x -> x
    | None -> raise (Invalid_argument "max")
  in
  fold (fun max x -> if lt max x then x else max) first gen

let eq ?(eq=(=)) gen1 gen2 =
  let rec check () =
    match gen1(), gen2() with
    | None, None -> true
    | Some x1, Some x2 when eq x1 x2 -> check ()
    | _ -> false
  in
  check ()

let lexico ?(cmp=Pervasives.compare) gen1 gen2 =
  let rec lexico () =
    match gen1(), gen2() with
    | None, None -> 0
    | Some x1, Some x2 ->
      let c = cmp x1 x2 in
      if c <> 0 then c else lexico ()
    | Some _, None -> 1
    | None, Some _ -> -1
  in lexico ()

let compare ?cmp gen1 gen2 = lexico ?cmp gen1 gen2

let rec find p e = match e () with
  | None -> None
  | Some x when p x -> Some x
  | Some _ -> find p e

let sum e =
  let rec sum acc = match e() with
  | None -> acc
  | Some x -> sum (x+acc)
  in sum 0

(** {2 Multiple Iterators} *)

let map2 f e1 e2 =
  fun () -> match e1(), e2() with
  | Some x, Some y -> Some (f x y)
  | _ -> None

let rec iter2 f e1 e2 =
  match e1(), e2() with
  | Some x, Some y -> f x y; iter2 f e1 e2
  | _ -> ()

let rec fold2 f acc e1 e2 =
  match e1(), e2() with
  | Some x, Some y -> fold2 f (f acc x y) e1 e2
  | _ -> acc

let rec for_all2 p e1 e2 =
  match e1(), e2() with
  | Some x, Some y -> p x y && for_all2 p e1 e2
  | _ -> true

let rec exists2 p e1 e2 =
  match e1(), e2() with
  | Some x, Some y -> p x y || exists2 p e1 e2
  | _ -> false

let zipWith f a b =
  let stop = ref false in
  fun () ->
    if !stop then None
    else match a(), b() with
    | Some xa, Some xb -> Some (f xa xb)
    | _ -> stop:=true; None

let zip a b = zipWith (fun x y -> x,y) a b

(** {3 Complex combinators} *)

module MergeState = struct
  type 'a t = {
    gens : 'a gen Queue.t;
    mutable state : my_state;
  }

  and my_state =
    | NewGen
    | YieldAndNew
    | Yield
    | Stop
end

(* TODO tests *)
(* state machine:
    (NewGen -> YieldAndNew)* // then no more generators in next_gen, so
    -> Yield* -> Stop *)
let merge next_gen =
  let open MergeState in
  let state = {gens = Queue.create(); state=NewGen;}in
  (* recursive function to get next element *)
  let rec next () =
    match state.state with
    | Stop -> None
    | Yield ->  (* only yield from generators in state.gens *)
        if Queue.is_empty state.gens
        then (state.state <- Stop; None)
        else
          let gen = Queue.pop state.gens in
          begin match gen () with
          | None -> next()
          | (Some _) as res ->
              Queue.push gen state.gens;  (* put gen back in queue *)
              res
        end
    | NewGen ->
        begin match next_gen() with
        | None ->
            state.state <- Yield;  (* exhausted *)
            next()
        | Some gen ->
            Queue.push gen state.gens;
            state.state <- YieldAndNew;
            next()
        end
    | YieldAndNew -> (* yield element from queue, then get a new generator *)
        if Queue.is_empty state.gens
        then (state.state <- NewGen; next())
        else
          let gen = Queue.pop state.gens in
          begin match gen () with
          | None -> state.state <- NewGen; next()
          | (Some _) as res ->
              Queue.push gen state.gens;
              state.state <- NewGen;
              res
          end
  in next

let intersection ?(cmp=Pervasives.compare) gen1 gen2 =
  let x1 = ref (gen1 ()) in
  let x2 = ref (gen2 ()) in
  let rec next () =
    match !x1, !x2 with
    | Some y1, Some y2 ->
      let c = cmp y1 y2 in
      if c = 0  (* equal elements, yield! *)
        then (x1 := gen1(); x2 := gen2(); Some y1)
      else if c < 0 (* drop y1 *)
        then (x1 := gen1 (); next ())
      else (* drop y2 *)
        (x2 := gen2(); next ())
    | _ -> None
  in next

let sorted_merge ?(cmp=Pervasives.compare) gen1 gen2 =
  let x1 = ref (gen1 ()) in
  let x2 = ref (gen2 ()) in
  fun () ->
    match !x1, !x2 with
    | None, None -> None
    | (Some y1)as r1, ((Some y2) as r2) ->
      if cmp y1 y2 <= 0
        then (x1 := gen1 (); r1)
        else (x2 := gen2 (); r2)
    | (Some _)as r, None ->
      x1 := gen1 ();
      r
    | None, ((Some _)as r) ->
      x2 := gen2 ();
      r

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

let sorted_merge_n ?(cmp=Pervasives.compare) l =
  (* make a heap of (value, generator) *)
  let cmp (v1,_) (v2,_) = cmp v1 v2 in
  let heap = Heap.empty ~cmp in
  (* add initial values *)
  List.iter
    (fun gen' -> match gen'() with
    | Some x -> Heap.insert heap (x, gen')
    | None -> ())
    l;
  fun () ->
    if Heap.is_empty heap then None
    else begin
      let x, gen = Heap.pop heap in
      match gen() with
      | Some y ->
        Heap.insert heap (y, gen);  (* insert next value *)
        Some x
      | None -> Some x (* gen empty, drop it *)
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
      else Some(Queue.pop q)
  (* consume [gen] until some element for [i]-th generator is
     available. *)
  and update_to_i i =
    match gen() with
    | None -> None
    | Some x ->
      let j = !cur in
      cur := (j+1) mod n;  (* move cursor to next generator *)
      let q = qs.(j) in
      if j = i
        then begin
          assert (Queue.is_empty q);
          Some x  (* return the element *)
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
        if !finished then None
        else get_next i  (* consume generator *)
      else Queue.pop qs.(i)
  (* consume one more element *)
  and get_next i = match gen() with
    | (Some x) as res ->
      for j = 0 to n-1 do
        if j <> i then Queue.push res qs.(j)
      done;
      res
    | None -> finished := true; None
  in
  (* generators *)
  let l = Array.mapi (fun i _ -> (fun () -> next i)) qs in
  Array.to_list l

module InterleaveState = struct
  type 'a t =
    | Only of 'a gen 
    | Both of 'a gen * 'a gen * bool ref
    | Stop
end

(* Yield elements from a and b alternatively *)
let interleave gen_a gen_b =
  let open InterleaveState in
  let state = ref (Both (gen_a, gen_b, ref true)) in
  let rec next() = match !state with
  | Stop -> None
  | Only g ->
      begin match g() with
        | None -> state := Stop; None
        | (Some _) as res -> res
      end
  | Both (g1, g2, r) ->
      match (if !r then g1() else g2()) with
      | None ->
          state := if !r then Only g2 else Only g1;
          next()
      | (Some _) as res ->
          r := not !r; (* swap *)
          res
  in next

module IntersperseState = struct
  type 'a t =
    | Start
    | YieldElem of 'a option
    | YieldSep of 'a option  (* next val *)
    | Stop
end

(* Put [x] between elements of [enum] *)
let intersperse x gen =
  let open IntersperseState in
  let state = ref Start in
  let rec next() = match !state with
    | Stop -> None
    | YieldElem res ->
        begin match gen() with
        | None -> state := Stop
        | Some _ as res' -> state := YieldSep res'
        end;
        res
    | YieldSep res ->
        state := YieldElem res;
        Some x
    | Start ->
        match gen() with
        | None -> state := Stop; None
        | Some _ as res -> state := YieldElem res; next()
  in next

(* Cartesian product *)
let product gena genb =
  let all_a = ref [] in
  let all_b = ref [] in
  (* cur: current state, i.e., what we have to do next. Can be stop,
    getLeft/getRight (to obtain next element from first/second generator),
    or prodLeft/prodRIght to compute the product of an element with a list
    of already met elements *)
  let cur = ref `GetLeft in
  let rec next () =
    match !cur with
    | `Stop -> None
    | `GetLeft ->
      begin match gena() with
        | None -> cur := `GetRightOrStop
        | Some a -> all_a := a :: !all_a; cur := `ProdLeft (a, !all_b)
      end;
      next ()
    | `GetRight | `GetRightOrStop ->  (* TODO: test *)
      begin match genb() with
        | None when !cur = `GetRightOrStop -> cur := `Stop
        | None -> cur := `GetLeft
        | Some b -> all_b := b::!all_b; cur := `ProdRight (b, !all_a)
      end;
      next ()
    | `ProdLeft (_, []) ->
      cur := `GetRight;
      next()
    | `ProdLeft (x, y::l) ->
      cur := `ProdLeft (x, l);
      Some (x, y)
    | `ProdRight (_, []) ->
      cur := `GetLeft;
      next()
    | `ProdRight (y, x::l) ->
      cur := `ProdRight (y, l);
      Some (x, y)
  in
  next

(* Group equal consecutive elements together. *)
let group ?(eq=(=)) gen =
  match gen() with
  | None -> fun () -> None
  | Some x ->
    let cur = ref [x] in
    let rec next () =
      (* try to get an element *)
      let next_x = if !cur = [] then None else gen() in
      match next_x, !cur with
      | None, [] -> None
      | None, l ->
        cur := [];  (* stop *)
        Some l
      | Some x, y::_ when eq x y ->
        cur := x::!cur;
        next ()  (* same group *)
      | Some x, l ->
        cur := [x];
        Some l
    in next

let uniq ?(eq=(=)) gen =
  let state = ref Init in
  let rec next() = match !state with
    | Stop -> None
    | Init ->
        begin match gen() with
        | None -> state:= Stop; None
        | (Some x) as res -> state := Run x; res
        end
    | Run x ->
        begin match gen() with
        | None -> state:= Stop; None
        | (Some y) as res ->
            if eq x y
            then next()   (* ignore duplicate *)
            else (state := Run y; res)
        end
  in next

let sort ?(cmp=Pervasives.compare) gen =
  (* build heap *)
  let h = Heap.empty ~cmp in
  iter (Heap.insert h) gen;
  fun () ->
    if Heap.is_empty h
      then None
      else Some (Heap.pop h)

(* NOTE: using a set is not really possible, because once we have built the
  set there is no simple way to iterate on it *)
let sort_uniq ?(cmp=Pervasives.compare) gen =
  uniq ~eq:(fun x y -> cmp x y = 0) (sort ~cmp gen)

let chunks n e =
  let rec next () =
    match e() with
    | None -> None
    | Some x ->
        let a = Array.make n x in
        fill a (n-1)

  and fill a i =
    (* fill the array. [i] elements remain to fill *)
    if i = n
    then Some a
    else match e() with
    | None -> Some (Array.sub a 0 i)  (* last array is not full *)
    | Some x ->
        a.(i) <- x;
        fill a (i+1)
  in
  next

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
    | [] -> None
    | x::l' -> l := l'; Some x

let to_rev_list gen =
  fold (fun acc x -> x :: acc) [] gen

let to_list gen = List.rev (to_rev_list gen)

let to_array gen =
  let l = to_rev_list gen in
  let a = Array.of_list l in
  let n = Array.length a in
  (* reverse array *)
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
      then None
      else (let x = a.(!i) in incr i; Some x)

let rand_int i =
  repeatedly (fun () -> Random.int i)

let int_range i j =
  let r = ref i in
  fun () ->
    let x = !r in
    if x > j then None
      else begin
        incr r;
        Some x
      end

let pp ?(start="") ?(stop="") ?(sep=",") ?(horizontal=false) pp_elem formatter gen =
  (if horizontal
    then Format.pp_open_hbox formatter ()
    else Format.pp_open_hvbox formatter 0);
  Format.pp_print_string formatter start;
  let rec next is_first =
    match gen() with
    | Some x ->
      if not is_first
        then begin
          Format.pp_print_string formatter sep;
          Format.pp_print_space formatter ();
          pp_elem formatter x
        end else pp_elem formatter x;
        next false
    | None -> ()
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

  let init ?limit f () = init ?limit f

  let cycle enum =
    assert (not (is_empty (enum ())));
    fun () ->
      let gen = ref (enum ()) in  (* start cycle *)
      let rec next () =
        match (!gen) () with
        | (Some _) as res -> res
        | None -> gen := enum(); next()
      in next

  let is_empty e = is_empty (e ())

  let fold f acc e = fold f acc (e ())

  let reduce f e = reduce f (e ())

  let scan f acc e () = scan f acc (e ())

  let iter f e = iter f (e ())

  let iteri f e = iteri f (e ())

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

  let map2 f e1 e2 () =
    map2 f (e1()) (e2())

  let iter2 f e1 e2 =
    iter2 f (e1()) (e2())

  let fold2 f acc e1 e2 =
    fold2 f acc (e1()) (e2())

  let min ?lt e = min ?lt (e ())

  let max ?lt e = max ?lt (e ())

  let ___eq = eq
  let eq ?eq e1 e2 = ___eq ?eq (e1 ()) (e2 ())

  let lexico ?cmp e1 e2 = lexico ?cmp (e1 ()) (e2 ())

  let compare ?cmp e1 e2 = compare ?cmp (e1 ()) (e2 ())

  let sum e = sum (e())

  let find f e = find f (e())

  let merge e () = merge (e ())

  let intersection ?cmp e1 e2 () =
    intersection ?cmp (e1 ()) (e2 ())

  let sorted_merge ?cmp e1 e2 () =
    sorted_merge ?cmp (e1 ()) (e2 ())

  let sorted_merge_n ?cmp l () =
    sorted_merge_n ?cmp (List.map (fun g -> g()) l)

  let tee ?n e = tee ?n (e ())

  let round_robin ?n e = round_robin ?n (e ())

  let interleave e1 e2 () = interleave (e1 ()) (e2 ())

  let intersperse x e () = intersperse x (e ())

  let product e1 e2 () = product (e1 ()) (e2 ())

  let group ?eq e () = group ?eq (e ())

  let uniq ?eq e () = uniq ?eq (e ())

  let sort ?(cmp=Pervasives.compare) enum =
    fun () -> sort ~cmp (enum ())

  let sort_uniq ?(cmp=Pervasives.compare) e =
    let e' = sort ~cmp e in
    uniq ~eq:(fun x y -> cmp x y = 0) e'

  let chunks n e () = chunks n (e())

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
      | None -> (fun () -> None)
      | Some first ->
        let cur = ref first in (* current element of the list *)
        let stop = ref false in (* are we done yet? *)
        fun () ->
          if !stop then None
          else begin
            let x = (!cur).content in
            cur := (!cur).next;
            (if !cur == first then stop := true); (* EOG, we made a full cycle *)
            Some x
          end
end

(** Store content of the generator in an enum *)
let persistent gen =
  let l = MList.create () in
  iter (MList.push_back l) gen;
  MList.to_enum l
