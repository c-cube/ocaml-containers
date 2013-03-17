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

(** a vector of 'a. *)
type 'a t = {
  mutable size : int;
  mutable vec : 'a array;
}

let create i =
  let i = max i 3 in
  { size = 0;
    vec = Array.create i (Obj.magic None);
  }

(** resize the underlying array so that it can contains the
    given number of elements *)
let resize v newcapacity =
  let newcapacity = min newcapacity Sys.max_array_length in
  if newcapacity <= Array.length v.vec
    then ()  (* already big enough *)
    else begin
      assert (newcapacity >= v.size);
      let new_vec = Array.create newcapacity (Obj.magic None) in
      Array.blit v.vec 0 new_vec 0 v.size;
      v.vec <- new_vec;
    end

let clear v =
  v.size <- 0;
  if Array.length v.vec > 1024  (* shrink if too large *)
    then (v.vec <- Array.create 10 (Obj.magic None))

let is_empty v = v.size = 0

let push v x =
  (if Array.length v.vec = v.size
    then resize v (2 * v.size));
  v.vec.(v.size) <- x;
  v.size <- v.size + 1

(** add all elements of b to a *)
let append a b =
  resize a (a.size + b.size);
  Array.blit b.vec 0 a.vec a.size b.size;
  a.size <- a.size + b.size

let append_array a b =
  resize a (a.size + Array.length b);
  Array.blit b 0 a.vec a.size (Array.length b);
  a.size <- a.size + Array.length b

let pop v =
  (if v.size = 0 then failwith "Vector.pop on empty vector");
  v.size <- v.size - 1;
  let x = v.vec.(v.size) in
  x

let copy v =
  let v' = create v.size in
  Array.blit v.vec 0 v'.vec 0 v.size;
  v'.size <- v.size;
  v'

let shrink v n =
  if n > v.size then failwith "cannot shrink to bigger size" else v.size <- n

let member ?(eq=(=)) v x =
  let n = v.size in
  let rec check i =
    if i = n then false
    else if eq x v.vec.(i) then true
    else check (i+1)
  in check 0

let sort ?(cmp=compare) v =
  (* copy array (to avoid junk in it), then sort the array *)
  let a = Array.sub v.vec 0 v.size in
  Array.fast_sort cmp a;
  v.vec <- a

let uniq_sort ?(cmp=compare) v =
  sort ~cmp v;
  let n = v.size in
  (* traverse to remove duplicates. i= current index,
     j=current append index, j<=i. new_size is the size
     the vector will have after removing duplicates. *)
  let rec traverse prev i j =
    if i >= n then () (* done traversing *)
    else if cmp prev v.vec.(i) = 0
      then (v.size <- v.size - 1; traverse prev (i+1) j) (* duplicate, remove it *)
      else (v.vec.(j) <- v.vec.(i); traverse v.vec.(i) (i+1) (j+1)) (* keep it *)
  in
  if v.size > 0
    then traverse v.vec.(0) 1 1 (* start at 1, to get the first element in hand *)

let iter v k =
  for i = 0 to v.size -1 do
    k v.vec.(i)
  done

let iteri v k =
  for i = 0 to v.size -1 do
    k i v.vec.(i)
  done

let map v f =
  let v' = create v.size in
  for i = 0 to v.size - 1 do
    push v' (f v.vec.(i));
  done;
  v'

let filter v f =
  let v' = create v.size in
  for i = 0 to v.size - 1 do
    if f v.vec.(i) then push v' v.vec.(i);
  done;
  v'

let fold v acc f =
  let acc = ref acc in
  for i = 0 to v.size - 1 do
    acc := f !acc v.vec.(i);
  done;
  !acc

let exists v p =
  let n = v.size in
  let rec check i =
    if i = n then false
    else if p v.vec.(i) then true
    else check (i+1)
  in check 0

let for_all v p =
  let n = v.size in
  let rec check i =
    if i = n then true
    else if not (p v.vec.(i)) then false
    else check (i+1)
  in check 0

let find v p =
  let n = v.size in
  let rec check i =
    if i = n then raise Not_found
    else if p v.vec.(i) then v.vec.(i)
    else check (i+1)
  in check 0

let get v i =
  (if i < 0 || i >= v.size then failwith "Vector.get");
  v.vec.(i)

let set v i x =
  (if i < 0 || i >= v.size then failwith "Vector.set");
  v.vec.(i) <- x

let size v = v.size

let unsafe_get_array v = v.vec

let of_seq ?(init=create 10) seq =
  Sequence.iter (fun x -> push init x) seq;
  init

let to_seq t =
  Sequence.from_iter (fun k -> iter t k)

let from_array a =
  let c = Array.length a in
  let v = create c in
  Array.blit a 0 v.vec 0 c;
  v.size <- c;
  v

let from_list l =
  let v = create 10 in
  List.iter (push v) l;
  v

let to_array v =
  Array.sub v.vec 0 v.size

let to_list v =
  let l = ref [] in
  for i = 0 to v.size - 1 do
    l := get v i :: !l;
  done;
  List.rev !l
