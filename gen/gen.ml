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

module type S = Gen_intf.S

(** {2 Transient generators} *)

let empty () = None

(*$T empty
  empty |> to_list = []
*)

let singleton x =
  let first = ref true in
  fun () ->
    if !first then (first := false; Some x) else None

(*T singleton
  singleton 1 |> to_list = [1]
  singleton "foo" |> to_list = ["foo"]
*)

let repeat x () = Some x

(*$T repeat
  repeat 42 |> take 3 |> to_list = [42; 42; 42]
*)

let repeatedly f () = Some (f ())

(*$T repeatedly
  repeatedly (let r = ref 0 in fun () -> incr r; !r) \
    |> take 5 |> to_list = [1;2;3;4;5]
*)

let iterate x f =
  let cur = ref x in
  fun () ->
    let x = !cur in
    cur := f !cur;
    Some x

(*$T iterate
  iterate 0 ((+)1) |> take 5 |> to_list = [0;1;2;3;4]
*)

let next gen = gen ()

let get gen = gen ()

let get_exn gen =
  match gen () with
  | Some x -> x
  | None -> raise (Invalid_argument "Gen.get_exn")

(*$R get_exn
  let g = of_list [1;2;3] in
  assert_equal 1 (get_exn g);
  assert_equal 2 (get_exn g);
  assert_equal 3 (get_exn g);
  assert_raises (Invalid_argument "Gen.get_exn") (fun () -> get_exn g)
*)

let junk gen = ignore (gen ())

let rec fold f acc gen =
  match gen () with
  | None -> acc
  | Some x -> fold f (f acc x) gen

(*$Q
  (Q.list Q.small_int) (fun l -> \
    of_list l |> fold (fun l x->x::l) [] = List.rev l)
*)

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

(*$T unfold
  unfold (fun (prev,cur) -> Some (prev, (cur,prev+cur))) (0,1) \
    |> take 7 |> to_list = [0; 1; 1; 2; 3; 5; 8]
*)

let init ?(limit=max_int) f =
  let r = ref 0 in
  fun () ->
    if !r >= limit
    then None
    else
      let x = f !r in
      let _ = incr r in
      Some x

(*$T init
  init ~limit:5 (fun i->i) |> to_list = [0;1;2;3;4]
*)

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

(*$T
  is_empty empty
  not (is_empty (singleton 2))
*)

let length gen =
  fold (fun acc _ -> acc + 1) 0 gen

(*$Q
  (Q.list Q.small_int) (fun l -> \
    of_list l |> length = List.length l)
*)

(* useful state *)
module RunState = struct
  type 'a t =
    | Init
    | Run of 'a
    | Stop
end

let scan f acc g =
  let open RunState in
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

(*$T scan
  scan (fun acc x -> x+1::acc) [] (1--5) |> to_list \
    = [[]; [2]; [3;2]; [4;3;2]; [5;4;3;2]; [6;5;4;3;2]]
*)

let unfold_scan f acc g =
  let open RunState in
  let state = ref (Run acc) in
  fun () ->
    match !state with
    | Init -> assert false
    | Stop -> None
    | Run acc ->
        match g() with
        | None -> state := Stop; None
        | Some x ->
            let acc', y = f acc x in
            state := Run acc';
            Some y

(*$T unfold_scan
  unfold_scan (fun acc x -> x+acc,acc) 0 (1--5) |> to_list \
    = [0; 1; 3; 6; 10]
*)

(** {3 Lazy} *)

let map f gen =
  let stop = ref false in
  fun () ->
    if !stop then None
    else match gen() with
    | None -> stop:= true; None
    | Some x -> Some (f x)

(*$Q map
  (Q.list Q.small_int) (fun l -> \
    let f x = x*2 in \
    of_list l |> map f |> to_list = List.map f l)
*)

let append gen1 gen2 =
  let first = ref true in
  let rec next() =
    if !first
    then match gen1() with
    | (Some _) as x -> x
    | None -> first:=false; next()
    else gen2()
  in next

(*$Q
  (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
    append (of_list l1) (of_list l2) |> to_list = l1 @ l2)
*)

let flatten next_gen =
  let open RunState in
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

let flat_map f next_elem =
  let open RunState in
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

(*$Q flat_map
  (Q.list Q.small_int) (fun l -> \
    let f x = of_list [x;x*2] in \
    eq (map f (of_list l) |> flatten) (flat_map f (of_list l)))
*)

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

(*$Q
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (n,l) -> \
    of_list l |> take n |> length = Pervasives.min n (List.length l))
*)

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

(*$Q
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (n,l) -> \
    let g1,g2 = take n (of_list l), drop n (of_list l) in \
    append g1 g2 |> to_list = l)
*)

let nth n gen =
  assert (n>=0);
  __drop n gen;
  match gen () with
  | None -> raise Not_found
  | Some x -> x

(*$= nth & ~printer:string_of_int
  4 (nth 4 (0--10))
  8 (nth 8 (0--10))
*)

(*$T
  (try ignore (nth 11 (1--10)); false with Not_found -> true)
*)

let take_nth n gen =
  assert (n>=1);
  let i = ref n in
  let rec next() =
    match gen() with
    | None -> None
    | (Some _) as res when !i = n -> i:=1; res
    | Some _ -> incr i; next()
  in next

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

(*$T
  filter (fun x ->x mod 2 = 0) (1--10) |> to_list = [2;4;6;8;10]
*)

let take_while p gen =
  let stop = ref false in
  fun () ->
    if !stop
    then None
    else match gen() with
    | (Some x) as res ->
        if p x then res else (stop := true; None)
    | None -> stop:=true; None

(*$T
  take_while (fun x ->x<10) (1--1000) |> eq (1--9)
*)

module DropWhileState = struct
  type t =
    | Stop
    | Drop
    | Yield
end

let drop_while p gen =
  let open DropWhileState in
  let state = ref Drop in
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
        | Some _ as res -> res
        end
  in next

(*$T
  drop_while (fun x-> x<10) (1--20) |> eq (10--20)
*)

let filter_map f gen =
  (* tailrec *)
  let rec next () =
    match gen() with
    | None -> None
    | Some x ->
        match f x with
        | None -> next()
        | (Some _) as res -> res
  in next

(*$T
  filter_map (fun x-> if x mod 2 = 0 then Some (string_of_int x) else None) (1--10) \
    |> to_list = List.map string_of_int [2;4;6;8;10]
*)

let zip_index gen =
  let r = ref ~-1 in
  fun () ->
    match gen() with
    | None -> None
    | Some x ->
        incr r;
        Some (!r, x)

(*$T
  zip_index (1--5) |> to_list = [0,1; 1,2; 2,3; 3,4; 4,5]
*)

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

(*$T
  unzip (of_list [1,2;3,4]) |> (fun (x,y)-> to_list x, to_list y) \
    = ([1;3], [2;4])
*)

(*$Q
  (Q.list (Q.pair Q.small_int Q.small_int)) (fun l -> \
    of_list l |> unzip |> (fun (x,y) -> to_list x,to_list y) = \
    List.split l)
*)

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

(*$T
  partition (fun x -> x mod 2 = 0) (1--10) |> \
    (fun (x,y)->to_list x, to_list y) = ([2;4;6;8;10], [1;3;5;7;9])
*)

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

(*$T
  min (of_list [1;4;6;0;11; -2]) = ~-2
  (try ignore (min empty); false with Invalid_argument _ -> true)
*)

let max ?(lt=fun x y -> x < y) gen =
  let first = match gen () with
    | Some x -> x
    | None -> raise (Invalid_argument "max")
  in
  fold (fun max x -> if lt max x then x else max) first gen

(*$T
  max (of_list [1;4;6;0;11; -2]) = 11
  (try ignore (max empty); false with Invalid_argument _ -> true)
*)

let eq ?(eq=(=)) gen1 gen2 =
  let rec check () =
    match gen1(), gen2() with
    | None, None -> true
    | Some x1, Some x2 when eq x1 x2 -> check ()
    | _ -> false
  in
  check ()

(*$Q
  (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
    eq (of_list l1)(of_list l2) = (l1 = l2))
*)

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

(*$Q
  (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
    let sign x = if x < 0 then -1 else if x=0 then 0 else 1 in \
    sign (compare (of_list l1)(of_list l2)) = sign (Pervasives.compare l1 l2))
*)

let rec find p e = match e () with
  | None -> None
  | Some x when p x -> Some x
  | Some _ -> find p e

(*$T
   find (fun x -> x>=5) (1--10) = Some 5
   find (fun x -> x>5) (1--4) = None
*)

let sum e =
  let rec sum acc = match e() with
  | None -> acc
  | Some x -> sum (x+acc)
  in sum 0

(*$T
  sum (1--10) = 55
*)

(** {2 Multiple Iterators} *)

let map2 f e1 e2 =
  fun () -> match e1(), e2() with
  | Some x, Some y -> Some (f x y)
  | _ -> None

(*$T
  map2 (+) (1--5) (1--4) |> eq (of_list [2;4;6;8])
  map2 (+) (1--5) (repeat 0) |> eq (1--5)
*)

let rec iter2 f e1 e2 =
  match e1(), e2() with
  | Some x, Some y -> f x y; iter2 f e1 e2
  | _ -> ()

(*$T iter2
  let r = ref 0 in iter2 (fun _ _ -> incr r) (1--10) (4--6); !r = 3
*)

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

let zip_with f a b =
  let stop = ref false in
  fun () ->
    if !stop then None
    else match a(), b() with
    | Some xa, Some xb -> Some (f xa xb)
    | _ -> stop:=true; None

let zip a b = zip_with (fun x y -> x,y) a b

(*$Q
  (Q.list Q.small_int) (fun l -> \
    zip_with (fun x y->x,y) (of_list l) (of_list l) \
      |> unzip |> fst |> to_list = l)
*)

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

(*$T
  merge (of_list [of_list [1;3;5]; of_list [2;4;6]; of_list [7;8;9]]) \
    |> to_list |> List.sort Pervasives.compare = [1;2;3;4;5;6;7;8;9]
*)

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

(*$T
  intersection (of_list [1;1;2;3;4;8]) (of_list [1;2;4;5;6;7;8;9]) \
    |> to_list = [1;2;4;8]
*)

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

(*$T
  sorted_merge (of_list [1;2;2;3;5;10;100]) (of_list [2;4;5;6;11]) \
    |> to_list = [1;2;2;2;3;4;5;5;6;10;11;100]
*)

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

(*$T
  sorted_merge_n [of_list [1;2;2;3;5;10;100]; of_list [2;4;5;6;11]; (6--10)] \
    |> to_list = [1;2;2;2;3;4;5;5;6;6;7;8;9;10;10;11;100]
*)

let round_robin ?(n=2) gen =
  (* array of queues, together with their index *)
  let qs = Array.init n (fun _ -> Queue.create ()) in
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

(*$T
  round_robin ~n:3 (1--12) |> List.map to_list = \
    [[1;4;7;10]; [2;5;8;11]; [3;6;9;12]]
*)

(* Duplicate the enum into [n] generators (default 2). The generators
   share the same underlying instance of the enum, so the optimal case is
   when they are consumed evenly *)
let tee ?(n=2) gen =
  (* array of queues, together with their index *)
  let qs = Array.init n (fun _ -> Queue.create ()) in
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
    | Some _ as res ->
      for j = 0 to n-1 do
        if j <> i then Queue.push res qs.(j)
      done;
      res
    | None -> finished := true; None
  in
  (* generators *)
  let l = Array.mapi (fun i _ -> (fun () -> next i)) qs in
  Array.to_list l

(*$T
  tee ~n:3 (1--12) |> List.map to_list = \
    [to_list (1--12); to_list (1--12); to_list (1--12)]
*)


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

(*$T
  interleave (repeat 0) (1--5) |> take 10 |> to_list = \
    [0;1;0;2;0;3;0;4;0;5]
*)

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

(*$T
  intersperse 0 (1--5) |> to_list = [1;0;2;0;3;0;4;0;5]
*)

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

(*$T
  product (1--3) (of_list ["a"; "b"]) |> to_list \
    |> List.sort Pervasives.compare = \
      [1, "a"; 1, "b"; 2, "a"; 2, "b"; 3, "a"; 3, "b"]
*)

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

(*$T
  group (of_list [0;0;0;1;0;2;2;3;4;5;5;5;5;10]) |> to_list = \
    [[0;0;0];[1];[0];[2;2];[3];[4];[5;5;5;5];[10]]
*)

let uniq ?(eq=(=)) gen =
  let open RunState in
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

(*$T
  uniq (of_list [0;0;0;1;0;2;2;3;4;5;5;5;5;10]) |> to_list = \
    [0;1;0;2;3;4;5;10]
*)

let sort ?(cmp=Pervasives.compare) gen =
  (* build heap *)
  let h = Heap.empty ~cmp in
  iter (Heap.insert h) gen;
  fun () ->
    if Heap.is_empty h
      then None
      else Some (Heap.pop h)
(*$T
  sort (of_list [0;0;0;1;0;2;2;3;4;5;5;5;-42;5;10]) |> to_list = \
    [-42;0;0;0;0;1;2;2;3;4;5;5;5;5;10]
*)


(* NOTE: using a set is not really possible, because once we have built the
  set there is no simple way to iterate on it *)
let sort_uniq ?(cmp=Pervasives.compare) gen =
  uniq ~eq:(fun x y -> cmp x y = 0) (sort ~cmp gen)

(*$T
  sort_uniq (of_list [0;0;0;1;0;2;2;3;4;5;42;5;5;42;5;10]) |> to_list = \
    [0;1;2;3;4;5;10;42]
*)

let chunks n e =
  let rec next () =
    match e() with
    | None -> None
    | Some x ->
        let a = Array.make n x in
        fill a 1

  and fill a i =
    (* fill the array. [i]: current index to fill *)
    if i = n
    then Some a
    else match e() with
    | None -> Some (Array.sub a 0 i)  (* last array is not full *)
    | Some x ->
        a.(i) <- x;
        fill a (i+1)
  in
  next

(*$T
  chunks 25 (0--100) |> map Array.to_list |> to_list = \
    List.map to_list [(0--24); (25--49);(50--74);(75--99);(100--100)]
*)

(* state of the permutation machine. One machine manages one element [x],
  and depends on a deeper machine [g] that generates permutations of the
  list minus this element (down to the empty list).
  The machine can do two things:
    - insert the element in the current list of [g], at any position
    - obtain the next list of [g]
*)

module PermState = struct
  type 'a state =
    | Done
    | Base (* bottom machine, yield [] *)
    | Insert of 'a insert_state
  and 'a insert_state = {
    x : 'a;
    mutable l : 'a list;
    mutable n : int; (* idx for insertion *)
    len : int; (* len of [l] *)
    sub : 'a t;
  }
  and 'a t = {
    mutable st : 'a state;
  }
end

let permutations g =
  let open PermState in
  (* make a machine for n elements. Invariant: n=len(l) *)
  let rec make_machine n l = match l with
    | [] -> assert (n=0); {st=Base}
    | x :: tail ->
        let sub = make_machine (n-1) tail in
        let st = match next sub () with
          | None -> Done
          | Some l -> Insert {x;n=0;l;len=n;sub}
        in
        {st;}
  (* next element of the machine *)
  and next m () = match m.st with
    | Done -> None
    | Base -> m.st <- Done; Some []
    | Insert ({x;len;n;l;sub} as state) ->
        if n=len
          then match next sub () with
            | None -> m.st <- Done; None
            | Some l ->
                state.l <- l;
                state.n <- 0;
                next m ()
          else (
            state.n <- state.n + 1;
            Some (insert x n l)
          )
  and insert x n l = match n, l with
    | 0, _ -> x::l
    | _, [] -> assert false
    | _, y::tail -> y :: insert x (n-1) tail
  in
  let l = fold (fun acc x->x::acc) [] g in
  next (make_machine (List.length l) l)

(*$T permutations
  permutations (1--3) |> to_list |> List.sort Pervasives.compare = \
    [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]]
  permutations empty |> to_list = [[]]
  permutations (singleton 1) |> to_list = [[1]]
*)

module CombState = struct
  type 'a state =
    | Done
    | Base
    | Add of 'a * 'a t * 'a t (* add x at beginning of first; then switch to second *)
    | Follow of 'a t  (* just forward *)
  and 'a t = {
    mutable st : 'a state
  }
end

let combinations n g =
  let open CombState in
  assert (n >= 0);
  let rec make_state n l = match n, l with
    | 0, _ -> {st=Base}
    | _, [] -> {st=Done}
    | _, x::tail ->
        let m1 = make_state (n-1) tail in
        let m2 = make_state n tail in
        {st=Add(x,m1,m2)}
  and next m () = match m.st with
    | Done -> None
    | Base -> m.st <- Done; Some []
    | Follow m ->
        begin match next m () with
        | None -> m.st <- Done; None
        | Some _ as res -> res
        end
    | Add (x, m1, m2) ->
        match next m1 () with
        | None ->
            m.st <- Follow m2;
            next m ()
        | Some l -> Some (x::l)
  in
  let l = fold (fun acc x->x::acc) [] g in
  next (make_state n l)

(*$T
  combinations 2 (1--4) |> map (List.sort Pervasives.compare) \
    |> to_list |> List.sort Pervasives.compare = \
    [[1;2]; [1;3]; [1;4]; [2;3]; [2;4]; [3;4]]
  combinations 0 (1--4) |> to_list = [[]]
  combinations 1 (singleton 1) |> to_list = [[1]]
*)

module PowerSetState = struct
  type 'a state =
    | Done
    | Base
    | Add of 'a * 'a t (* add x before any result of m *)
    | AddTo of 'a list * 'a * 'a t (* yield x::list, then back to Add(x,m) *)
  and 'a t = {
    mutable st : 'a state
  }
end

let power_set g =
  let open PowerSetState in
  let rec make_state l = match l with
    | [] -> {st=Base}
    | x::tail ->
        let m = make_state tail in
        {st=Add(x,m)}
  and next m () = match m.st with
    | Done -> None
    | Base -> m.st <- Done; Some []
    | Add (x,m') ->
        begin match next m' () with
        | None -> m.st <- Done; None
        | Some l as res -> m.st <- AddTo(l,x,m'); res
        end
    | AddTo (l, x, m') ->
        m.st <- Add (x,m');
        Some (x::l)
  in
  let l = fold (fun acc x->x::acc) [] g in
  next (make_state l)

(*$T
  power_set (1--3) |> map (List.sort Pervasives.compare) \
    |> to_list |> List.sort Pervasives.compare = \
    [[]; [1]; [1;2]; [1;2;3]; [1;3]; [2]; [2;3]; [3]]
  power_set empty |> to_list = [[]]
  power_set (singleton 1) |> map (List.sort Pervasives.compare) \
    |> to_list |> List.sort Pervasives.compare = [[]; [1]]
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

(*$Q
  (Q.list Q.small_int) (fun l -> \
    to_rev_list (of_list l) = List.rev l)
*)

let to_list gen = List.rev (to_rev_list gen)

let to_array gen =
  let l = to_rev_list gen in
  match l with
  | [] -> [| |]
  | _ ->
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

(*$Q
  (Q.array Q.small_int) (fun a -> \
    of_array a |> to_array = a)
*)

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

  let (>>=) x f = flat_map f x
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

  let unfold_scan f acc e () = unfold_scan f acc (e())

  let iter f e = iter f (e ())

  let iteri f e = iteri f (e ())

  let length e = length (e ())

  let map f e () = map f (e ())

  let append e1 e2 () = append (e1 ()) (e2 ())

  let flatten e () = flatten (e ())

  let flat_map f e () = flat_map f (e ())

  let mem ?eq x e = mem ?eq x (e ())

  let take n e () = take n (e ())

  let drop n e () = drop n (e ())

  let nth n e = nth n (e ())

  let take_nth n e () = take_nth n (e ())

  let filter p e () = filter p (e ())

  let take_while p e () = take_while p (e ())

  let drop_while p e () = drop_while p (e ())

  let filter_map f e () = filter_map f (e ())

  let zip_with f e1 e2 () = zip_with f (e1 ()) (e2 ())

  let zip e1 e2 () = zip (e1 ()) (e2 ())

  let zip_index e () = zip_index (e ())

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

  let permutations g () = permutations (g ())

  let combinations n g () = combinations n (g())

  let power_set g () = power_set (g())

  let of_list l () = of_list l

  let to_rev_list e = to_rev_list (e ())

  let to_list e = to_list (e ())

  let to_array e = to_array (e ())

  let of_array ?start ?len a () = of_array ?start ?len a

  let rand_int i () = rand_int i

  let int_range i j () = int_range i j

  module Infix = struct
    let (--) = int_range

    let (>>=) x f = flat_map f x
  end

  include Infix

  let pp ?start ?stop ?sep ?horizontal pp_elem fmt e =
    pp ?start ?stop ?sep ?horizontal pp_elem fmt (e ())
end

(** {2 Generator functions} *)

let start g = g ()

(** {6 Unrolled mutable list} *)
module MList = struct
  type 'a node =
    | Nil
    | Cons of 'a array * int ref * 'a node ref
    | Suspend of 'a gen

  type 'a t = {
    start : 'a node ref; (* first node. *)
    mutable chunk_size : int;
    max_chunk_size : int;
  }

  let _make ~max_chunk_size gen = {
    start = ref (Suspend gen);
    chunk_size = 8;
    max_chunk_size;
  }

  (* increment the size of chunks *)
  let _incr_chunk_size mlist =
    if mlist.chunk_size < mlist.max_chunk_size
      then mlist.chunk_size <- 2 * mlist.chunk_size

  (* read one chunk of input; return the corresponding node.
    will potentially change [mlist.chunk_size]. *)
  let _read_chunk mlist gen =
    match gen() with
    | None -> Nil  (* done *)
    | Some x ->
      (* new list node *)
        let r = ref 1 in
        let a = Array.make mlist.chunk_size x in
        let tail = ref (Suspend gen) in
        let stop = ref false in
        let node = Cons (a, r, tail) in
        (* read the rest of the chunk *)
        while not !stop && !r < mlist.chunk_size do
          match gen() with
          | None ->
              tail := Nil;
              stop := true
          | Some x ->
              a.(!r) <- x;
              incr r;
        done;
        _incr_chunk_size mlist;
        node

  (* eager construction *)
  let of_gen gen =
    let mlist = _make ~max_chunk_size:4096 gen in
    let rec _fill prev = match _read_chunk mlist gen with
      | Nil -> prev := Nil
      | Suspend _ -> assert false
      | Cons (_, _, prev') as node ->
          prev := node;
          _fill prev'
    in
    _fill mlist.start;
    mlist

  (* lazy construction *)
  let of_gen_lazy gen =
    let mlist = _make ~max_chunk_size:32 gen in
    mlist

  let to_gen l () =
    let cur = ref l.start in
    let i = ref 0 in
    let rec next() = match ! !cur with
    | Nil -> None
    | Cons (a,n,l') ->
        if !i = !n
        then begin
          cur := l';
          i := 0;
          next()
        end else begin
          let y = a.(!i) in
          incr i;
          Some y
        end
    | Suspend gen ->
        let node = _read_chunk l gen in
        !cur := node;
        next()
    in
    next
end

(** Store content of the generator in an enum *)
let persistent gen =
  let l = MList.of_gen gen in
  MList.to_gen l

(*$T
  let g = 1--10 in let g' = persistent g in \
    Restart.to_list g' = Restart.to_list g'
  let g = 1--10 in let g' = persistent g in \
    Restart.to_list g' = [1;2;3;4;5;6;7;8;9;10]
*)

let persistent_lazy gen =
  let l = MList.of_gen_lazy gen in
  MList.to_gen l

(*$T
  let g = 1--1_000_000_000 in let g' = persistent_lazy g in \
    (g' () |> take 100 |> to_list = (1--100 |> to_list)) && \
    (g' () |> take 200 |> to_list = (1--200 |> to_list))
*)
