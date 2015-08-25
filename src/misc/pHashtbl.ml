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

(** {1 Open addressing hashtable (robin hood hashing)} *)

type 'a sequence = ('a -> unit) -> unit

type ('a, 'b) t = {
  mutable buckets : ('a, 'b) bucket array;
  mutable size : int;
  eq : 'a -> 'a -> bool;
  hash : 'a -> int;
  max_load : float;
} (** A hashtable is an array of (key, value) buckets that have a state,
      plus the size of the table and equality/hash functions *)
and ('a, 'b) bucket =
  | Empty
  | Deleted
  | Used of 'a * 'b * int  (* int: the distance from home of the key *)
  (** a bucket *)

(** Create a table. Size will be >= 2 *)
let create ?(max_load=0.8) ?(eq=fun x y -> x = y)
           ?(hash=fun x -> Hashtbl.hash x) size =
  let size = max 2 size in
  { buckets = Array.make size Empty;
    size = 0;
    max_load;
    eq;
    hash; }

module type Hashable = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** Create a hashtable from the given 'typeclass' *)
let create_tc (type key) (h : (module Hashable with type t = key)) size =
  let module H = (val h) in
  create ~eq:H.equal ~hash:H.hash size

(** Copy of the hashtable *)
let copy t = {
  eq = t.eq;
  hash = t.hash;
  max_load = t.max_load;
  size = t.size;
  buckets = Array.copy t.buckets;
}

(** clear the table, by resetting all states to Empty *)
let clear t =
  Array.fill t.buckets 0 (Array.length t.buckets) Empty;
  t.size <- 0

(** Index of slot, for i-th probing starting from hash [h] in
    a table of length [n] *)
let addr h n i = (h + i) mod n

(** Insert (key -> value) in table, starting with the hash. *)
let insert t key value =
  let n = Array.length t.buckets in
  let h = t.hash key in
  (* lookup an empty slot to insert the key->value in. *)
  let rec lookup h i key value dist =
    let j = addr h n i in
    match t.buckets.(j) with
    | Empty | Deleted ->
      (* insert here *)
      t.size <- t.size + 1;
      t.buckets.(j) <- Used (key, value, dist)
    | Used (key', _, _) when t.eq key key' ->
      (* insert here (erase old value) *)
      t.buckets.(j) <- Used (key, value, dist)
    | Used (key', value', dist') when dist > dist' ->
      (* displace this key/value *)
      t.buckets.(j) <- Used (key, value, dist);
      (* insert the other value again *)
      lookup h (i+1) key' value' (dist+1)
    | Used _ ->
      (* search further for insertion *)
      lookup h (i+1) key value (dist+1)
  in
  lookup h 0 key value 1

(** Resize the array, by inserting its content into twice as large an array *)
let resize t =
  let new_size = min (Array.length t.buckets * 2 + 1) Sys.max_array_length in
  if not (new_size > Array.length t.buckets) then failwith "hashtbl is full";
  let old_buckets = t.buckets in
  t.buckets <- Array.make new_size Empty;
  t.size <- 0;  (* will be updated again *)
  for i = 0 to Array.length old_buckets - 1 do
    match old_buckets.(i) with
    | Used (key, value, _) ->
      (* insert key -> value into new array *)
      insert t key value
    | Empty | Deleted -> ()
  done

(** Lookup [key] in the table *)
let find t key =
  let n = Array.length t.buckets in
  let h = t.hash key in
  let buckets = t.buckets in
  let rec probe h n i =
    if i = n then raise Not_found else
    let j = addr h n i in
    match buckets.(j) with
    | Used (key', value, _) when t.eq key key' ->
      value  (* found value for this key *)
    | Deleted | Used _ ->
      probe h n (i+1) (* try next bucket *)
    | Empty -> raise Not_found
  in
  probe h n 0

(** put [key] -> [value] in the hashtable *)
let replace t key value =
  let load = float_of_int t.size /. float_of_int (Array.length t.buckets) in
  (if load > t.max_load then resize t);
  insert t key value

(** alias for replace *)
let add t key value =
  replace t key value

(** Remove the key from the table *)
let remove t key =
  let n = Array.length t.buckets in
  let h = t.hash key in
  let buckets = t.buckets in
  let rec probe h n i =
    let j = addr h n i in
    match buckets.(j) with
    | Used (key', _, _) when t.eq key key' ->
      buckets.(j) <- Deleted;
      t.size <- t.size - 1  (* remove slot *)
    | Deleted | Used _ ->
      probe h n (i+1) (* search further *)
    | Empty -> ()  (* not present *)
  in
  probe h n 0

(** size of the table *)
let length t = t.size

(** Is the key member of the table? *)
let mem t key =
  try ignore (find t key); true
  with Not_found -> false

(** Iterate on key -> value pairs *)
let iter k t =
  let buckets = t.buckets in
  for i = 0 to Array.length buckets - 1 do
    match buckets.(i) with
    | Used (key, value, _) -> k key value
    | Empty | Deleted -> ()
  done

(** Fold on key -> value pairs *)
let fold f acc t =
  let acc = ref acc in
  let buckets = t.buckets in
  for i = 0 to Array.length buckets - 1 do
    match buckets.(i) with
    | Used (key, value, _) ->
      acc := f !acc key value
    | Empty | Deleted -> ()
  done;
  !acc

(** Map, replaces values by other values *)
let map f t =
  let t' = create ~eq:t.eq ~hash:t.hash (Array.length t.buckets) in
  for i = 0 to Array.length t.buckets - 1 do
    match t.buckets.(i) with
    | Empty -> ()
    | Deleted -> t'.buckets.(i) <- Deleted
    | Used (k, v, dist) ->
      t'.buckets.(i) <- Used (k, f k v, dist)
  done;
  t'.size <- t.size;
  t'

(** Destructive filter (remove bindings that do not satisfiy predicate) *)
let filter pred t =
  for i = 0 to Array.length t.buckets - 1 do
    match t.buckets.(i) with
    | Empty | Deleted -> ()
    | Used (k, v, _) when pred k v -> ()
    | Used (k, v, _) -> (* remove this element *)
      t.buckets.(i) <- Deleted;
      t.size <- t.size - 1
  done

(** Add the given pairs to the hashtable *)
let of_seq t seq =
  seq (fun (k,v) -> add t k v)

(** CCSequence of pairs *)
let to_seq t kont = iter (fun k v -> kont (k,v)) t

(** Statistics on the table *)
let stats t = (Array.length t.buckets, t.size, t.size, 0, 0, 1)

let get_eq t = t.eq

let get_hash t = t.hash
