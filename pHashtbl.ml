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

(** {1 Open addressing hashtable, with linear probing} *)

type ('a, 'b) t = {
  mutable buckets : ('a * 'b * state) array;
  mutable size : int;
  eq : 'a -> 'a -> bool;
  hash : 'a -> int;
  max_load : float;
} (** A hashtable is an array of (key, value) buckets that have a state,
      plus the size of the table and equality/hash functions *)
and state = Used | Empty | Deleted
  (** state of a bucket *)

let my_null () = (Obj.magic None, Obj.magic None, Empty)

let my_deleted key = (key, Obj.magic None, Deleted)

(** Create a table. Size will be >= 2 *)
let create ?(max_load=0.8) ?(eq=fun x y -> x = y)
           ?(hash=fun x -> Hashtbl.hash x) size =
  let size = max 2 size in
  let null = my_null () in
  { buckets = Array.make size null;
    size = 0;
    max_load;
    eq;
    hash; }

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
  let null = my_null () in
  Array.fill t.buckets 0 (Array.length t.buckets) null;
  t.size <- 0

(** Index of slot, for i-th probing starting from hash [h] in
    a table of length [n] *)
let addr h n i = (h + i) mod n
  
(** Insert (key -> value) in buckets, starting with the hash. *)
let insert ~eq buckets h key value =
  let n = Array.length buckets in
  (* lookup an empty slot to insert the key->value in. *)
  let rec lookup h n i =
    let j = addr h n i in
    match buckets.(j) with
    | (_, _, Empty) -> buckets.(j) <- (key, value, Used)
    | (key', _, _) when eq key key' -> ()
    | _ -> lookup h n (i+1)
  in
  lookup h n 0

(** Resize the array, by inserting its content into twice as large an array *)
let resize ~eq ~hash buckets =
  let buckets' = Array.make (Array.length buckets * 2) (my_null ()) in
  for i = 0 to Array.length buckets - 1 do
    match buckets.(i) with
    | (key, value, Used) ->
      insert ~eq buckets' (hash key) key value  (* insert key -> value into new array *)
    | _ -> ()
  done;
  buckets'

(** Lookup [key] in the table *)
let find t key =
  let n = Array.length t.buckets in
  let h = t.hash key in
  let buckets = t.buckets in
  let rec probe h n i num =
    if num = n then raise Not_found
    else
    let j = addr h n i in
    match buckets.(j) with
    | (key', value, Used) when t.eq key key' ->
      value  (* found value for this key *)
    | (_, _, Deleted) | (_, _, Used) ->
      probe h n (i+1) (num + 1) (* try next bucket *)
    | (_, _, Empty) -> raise Not_found
  in
  probe h n 0 0

(** put [key] -> [value] in the hashtable *)
let replace t key value =
  let load = float_of_int t.size /. float_of_int (Array.length t.buckets) in
  (if load > t.max_load then t.buckets <- resize ~eq:t.eq ~hash:t.hash t.buckets);
  let n = Array.length t.buckets in
  let h = t.hash key in
  let buckets = t.buckets in
  let rec probe h n i =
    let j = addr h n i in
    match buckets.(j) with
    | (key', _, Used) when t.eq key key' ->
      buckets.(j) <- (key, value, Used)  (* replace value *)
    | (_, _, Deleted) | (_, _, Empty) ->
      buckets.(j) <- (key, value, Used);
      t.size <- t.size + 1 (* insert and increment size *)
    | (_, _, Used) ->
      probe h n (i+1) (* go further *)
  in
  probe h n 0

(** alias for replace *)
let add t key value = replace t key value

(** Remove the key from the table *)
let remove t key =
  let n = Array.length t.buckets in
  let h = t.hash key in
  let buckets = t.buckets in
  let rec probe h n i =
    let j = addr h n i in
    match buckets.(j) with
    | (key', _, Used) when t.eq key key' ->
      buckets.(j) <- (my_deleted key'); t.size <- t.size - 1  (* remove slot *)
    | (_, _, Deleted) | (_, _, Used) ->
      probe h n (i+1) (* search further *)
    | (_, _, Empty) -> ()  (* not present *)
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
    | (key, value, Used) -> k key value
    | _ -> ()
  done

(** Fold on key -> value pairs *)
let fold f acc t =
  let acc = ref acc in
  let buckets = t.buckets in
  for i = 0 to Array.length buckets - 1 do
    match buckets.(i) with
    | (key, value, Used) -> acc := f !acc key value
    | _ -> ()
  done;
  !acc

(** Destructive filter (remove bindings that do not satisfiy predicate) *)
let filter pred t =
  for i = 0 to Array.length t.buckets - 1 do
    match t.buckets.(i) with
    | (_, _, (Empty | Deleted)) -> ()
    | (k, v, Used) when pred k v -> ()
    | (k, v, Used) -> (* remove this element *)
      t.buckets.(i) <- my_deleted k;
      t.size <- t.size - 1
  done

(** Add the given pairs to the hashtable *)
let of_seq t seq =
  Sequence.iter (fun (k,v) -> add t k v) seq

(** Sequence of pairs *)
let to_seq t =
  Sequence.from_iter
    (fun kont -> iter (fun k v -> kont (k,v)) t)

(** Statistics on the table *)
let stats t = (Array.length t.buckets, t.size, t.size, 0, 0, 1)
