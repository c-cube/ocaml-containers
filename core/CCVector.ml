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

(** {1 Growable, mutable vector} *)

type rw = [`RW]
type ro = [`RO]

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

(** a vector of 'a. *)
type ('a,'mut) t = {
  mutable size : int;
  mutable vec : 'a array;
}

type 'a vector = ('a, rw) t

let freeze v = {
  size=v.size;
  vec=v.vec;
}

let freeze_copy v = {
  size=v.size;
  vec=Array.sub v.vec 0 v.size;
}

let create () = {
  size = 0;
  vec = [| |];
}

let create_with ?(capacity=128) x = {
  size = 0;
  vec = Array.make capacity x;
}

(*$T
  (create_with ~capacity:200 1 |> capacity) >= 200
*)

let make n x = {
  size=n;
  vec=Array.make n x;
}

let init n f = {
  size=n;
  vec=Array.init n f;
}

(* is the underlying empty? *)
let _empty_array v =
  Array.length v.vec = 0

(* assuming the underlying array isn't empty, resize it *)
let _resize v newcapacity =
  assert (newcapacity >= v.size);
  assert (not (_empty_array v));
  let new_vec = Array.make newcapacity v.vec.(0) in
  Array.blit v.vec 0 new_vec 0 v.size;
  v.vec <- new_vec;
  ()

(*$T
  (let v = create_with ~capacity:10 1 in ensure v 200; capacity v >= 200)
*)

(* grow the array, using [x] as a filler if required *)
let _grow v x =
  if _empty_array v
    then v.vec <- Array.make 32 x
    else
      let n = Array.length v.vec in
      let size = min (n + n/2 + 10) Sys.max_array_length in
      _resize v size

let ensure v size =
  if Array.length v.vec = 0
    then ()
  else if v.size < size
    then
      let size' = min size Sys.max_array_length in
      _resize v size'

let clear v =
  v.size <- 0

let is_empty v = v.size = 0

let push v x =
  if v.size = Array.length v.vec
    then _grow v x;
  Array.unsafe_set v.vec v.size x;
  v.size <- v.size + 1

(** add all elements of b to a *)
let append a b =
  if _empty_array a
  then if _empty_array b
    then ()
    else (
      a.vec <- Array.copy b.vec;
      a.size <- b.size
    )
  else (
    ensure a (a.size + b.size);
    assert (Array.length a.vec >= a.size + b.size);
    Array.blit b.vec 0 a.vec a.size b.size;
    a.size <- a.size + b.size
  )

let get v i =
  if i < 0 || i >= v.size then failwith "Vector.get";
  Array.unsafe_get v.vec i

let set v i x =
  if i < 0 || i >= v.size then failwith "Vector.set";
  Array.unsafe_set v.vec i x

let remove v i =
  if i < 0 || i >= v.size then failwith "Vector.remove";
  (* if v.(i) not the last element, then put last element at index i *)
  if i < v.size - 1
    then v.vec.(i) <- v.vec.(v.size - 1);
  (* remove one element *)
  v.size <- v.size - 1

let append_seq a seq =
  seq (fun x -> push a x)

let append_array a b =
  Array.iter (push a) b

let equal eq v1 v2 =
  let n = min v1.size v2.size in
  let rec check i =
    if i = n
      then v1.size = v2.size
      else eq (get v1 i) (get v2 i) && check (i+1)
  in check 0

let compare cmp v1 v2 =
  let n = min v1.size v2.size in
  let rec check i =
    if i = n
      then Pervasives.compare v1.size v2.size
      else
        let c = cmp (get v1 i) (get v2 i) in
        if c = 0 then check (i+1) else c
  in check 0

let pop_exn v =
  if v.size = 0
    then failwith "Vector.pop on empty vector";
  v.size <- v.size - 1;
  let x = v.vec.(v.size) in
  x

let pop v =
  try Some (pop_exn v)
  with Failure _ -> None

let copy v = {
  size = v.size;
  vec = Array.sub v.vec 0 v.size;
}

(*$T
  (let v = of_list [1;2;3] in let v' = copy v in \
    to_list v' = [1;2;3])
  create () |> copy |> is_empty
*)

let shrink v n =
  if n < v.size then v.size <- n

let sort' cmp v =
  (* possibly copy array (to avoid junk at its end), then sort the array *)
  let a =
    if Array.length v.vec = v.size then v.vec
    else Array.sub v.vec 0 v.size
  in
  Array.fast_sort cmp a;
  v.vec <- a

let sort cmp v =
  let v' = {
    size=v.size;
    vec=Array.sub v.vec 0 v.size;
  } in
  Array.sort cmp v'.vec;
  v'

let uniq_sort cmp v =
  sort' cmp v;
  let n = v.size in
  (* traverse to remove duplicates. i= current index,
     j=current append index, j<=i. new_size is the size
     the vector will have after removing duplicates. *)
  let rec traverse prev i j =
    if i >= n then () (* done traversing *)
    else if cmp prev v.vec.(i) = 0
      then (
        v.size <- v.size - 1;
        traverse prev (i+1) j
      ) (* duplicate, remove it *)
      else (
        v.vec.(j) <- v.vec.(i);
        traverse v.vec.(i) (i+1) (j+1)
      ) (* keep it *)
  in
  if v.size > 0
    then traverse v.vec.(0) 1 1
    (* start at 1, to get the first element in hand *)

let iter k v =
  for i = 0 to v.size -1 do
    k (Array.unsafe_get v.vec i)
  done

let iteri k v =
  for i = 0 to v.size -1 do
    k i (Array.unsafe_get v.vec i)
  done

let map f v =
  if _empty_array v
  then create ()
  else {
    size=v.size;
    vec=Array.map f v.vec
  }

let filter' p v =
  let i = ref (v.size - 1) in
  while !i >= 0 do
    if not (p v.vec.(! i))
      (* remove i-th item! *)
      then remove v !i;
    decr i
  done

(*$T
  let v = 1 -- 10 in filter' (fun x->x<4) v; \
   to_list v |> List.sort Pervasives.compare = [1;2;3]
*)

let filter p v =
  if _empty_array v
  then create ()
  else (
    let v' = create_with ~capacity:v.size v.vec.(0) in
    Array.iter
      (fun x -> if p x then push v' x)
      v.vec;
    v'
  )

(*$T
  filter (fun x-> x mod 2=0) (of_list [1;2;3;4;5]) |> to_list = [2;4]
*)

let fold f acc v =
  let rec fold acc i =
    if i = v.size then acc
    else
      let x = Array.unsafe_get v.vec i in
      fold (f acc x) (i+1)
  in fold acc 0

(*$T
  fold (+) 0 (of_list [1;2;3;4;5]) = 15
  fold (+) 0 (create ()) = 0
*)

let exists p v =
  let n = v.size in
  let rec check i =
    if i = n then false
    else p v.vec.(i) || check (i+1)
  in check 0

let for_all p v =
  let n = v.size in
  let rec check i =
    if i = n then true
    else p v.vec.(i) && check (i+1)
  in check 0

let member ?(eq=(=)) x v =
  exists (eq x) v

let find_exn p v =
  let n = v.size in
  let rec check i =
    if i = n then raise Not_found
    else if p v.vec.(i) then v.vec.(i)
    else check (i+1)
  in check 0

let find p v =
  try Some (find_exn p v)
  with Not_found -> None

let filter_map f v =
  let v' = create () in
  iter
    (fun x -> match f x with
      | None -> ()
      | Some y -> push v' y
    ) v;
  v'

let flat_map f v =
  let v' = create () in
  iter (fun x -> iter (push v') (f x)) v;
  v'

let flat_map' f v =
  let v' = create () in
  iter
    (fun x ->
      let seq = f x in
      seq (fun y -> push v' y)
    ) v;
  v'

let (>>=) x f = flat_map f x

let (>|=) x f = map f x

let rev' v =
  if v.size > 0
  then (
    let n = v.size in
    let vec = v.vec in
    for i = 0 to (n-1)/2 do
      let x = Array.unsafe_get vec i in
      let y = Array.unsafe_get vec (n-i-1) in
      Array.unsafe_set vec i y;
      Array.unsafe_set vec (n-i-1) x;
    done
  )

let rev v =
  let v' = copy v in
  rev' v';
  v'

(*$T
  rev (of_list [1;2;3;4]) |> to_list = [4;3;2;1]
  rev (of_list [1;2;3;4;5]) |> to_list = [5;4;3;2;1]
  rev (create ()) |> to_list = []
*)

let size v = v.size

let length v = v.size

let capacity v = Array.length v.vec

let unsafe_get_array v = v.vec

let of_seq ?(init=create ()) seq =
  append_seq init seq;
  init

(*$T
  of_seq CCSequence.(1 -- 10) |> to_list = CCList.(1 -- 10)
*)

let to_seq v k = iter k v

let slice_seq v start len =
  assert (start >= 0 && len >= 0);
  fun k ->
    assert (start+len < v.size);
    for i = start to start+len-1 do
      let x = Array.unsafe_get v.vec i in
      k x
    done

(*$T
  slice_seq (of_list [0;1;2;3;4]) 1 3 |> CCList.of_seq = [1;2;3]
*)

let slice v = (v.vec, 0, v.size)

let (--) i j =
  if i>j
    then init (i-j+1) (fun k -> i-k)
    else init (j-i+1) (fun k -> i+k)

(*$T
  (1 -- 4) |> to_list = [1;2;3;4]
  (4 -- 1) |> to_list = [4;3;2;1]
  (0 -- 0) |> to_list = [0]
*)

let of_array a =
  if Array.length a = 0
  then create ()
  else {
    size=Array.length a;
    vec=Array.copy a;
  }

let of_list l = match l with
  | [] -> create()
  | x::l' ->
      let v = create_with ~capacity:(List.length l + 5) x in
      List.iter (push v) l;
      v

let to_array v =
  Array.sub v.vec 0 v.size

let to_list v =
  List.rev (fold (fun acc x -> x::acc) [] v)

let of_gen ?(init=create ()) g =
  let rec aux g = match g() with
    | None -> init
    | Some x -> push init x; aux g
  in aux g

let to_gen v =
  let i = ref 0 in
  fun () ->
    if !i < v.size
      then (
        let x = v.vec.( !i ) in
        incr i;
        Some x
      ) else None

let of_klist ?(init=create ()) l =
  let rec aux l = match l() with
    | `Nil -> init
    | `Cons (x,l') -> push init x; aux l'
  in aux l

let to_klist v =
  let rec aux i () =
    if i=v.size then `Nil
    else `Cons (v.vec.(i), aux (i+1))
  in aux 0

let pp ?(start="[") ?(stop="]") ?(sep=", ") pp_item buf v =
  Buffer.add_string buf start;
  iteri
    (fun i x ->
      if i > 0 then Buffer.add_string buf sep;
      pp_item buf x
    ) v;
  Buffer.add_string buf stop

let print ?(start="[") ?(stop="]") ?(sep=", ") pp_item fmt v =
  Format.pp_print_string fmt start;
  iteri
    (fun i x ->
      if i > 0 then (Format.pp_print_string fmt sep; Format.pp_print_cut fmt());
      pp_item fmt x
    ) v;
  Format.pp_print_string fmt stop
