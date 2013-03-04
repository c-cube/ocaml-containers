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

(** Open addressing hashtable, with linear probing. *)

module type S =
  sig
    type key

    type 'a t

    val create : ?max_load:float -> int -> 'a t
      (** Create a hashtable.  [max_load] is (number of items / size of table).
          Must be in ]0, 1[ *)

    val clear : 'a t -> unit
      (** Clear the content of the hashtable *)

    val find : 'a t -> key -> 'a
      (** Find the value for this key, or raise Not_found *)

    val replace : 'a t -> key -> 'a -> unit
      (** Add/replace the binding for this key. O(1) amortized. *)

    val remove : 'a t -> key -> unit
      (** Remove the binding for this key, if any *)

    val length : 'a t -> int
      (** Number of bindings in the table *)

    val mem : 'a t -> key -> bool
      (** Is the key present in the hashtable? *)

    val iter : (key -> 'a -> unit) -> 'a t -> unit
      (** Iterate on bindings *)

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      (** Fold on bindings *)

    val stats : 'a t -> int * int * int * int * int * int
      (** Cf Weak.S *)
  end

module Make(H : Hashtbl.HashedType) =
  struct
    type key = H.t

    (** A hashtable is an array of (key, value) buckets that have a state, plus the
        size of the table *)
    type 'a t = {
      mutable buckets : (key * 'a * state) array;
      mutable size : int;
      max_load : float;
    }
    (* state of a bucket *)
    and state = Used | Empty | Deleted

    let my_null = (Obj.magic None, Obj.magic None, Empty)
    let my_deleted = (Obj.magic None, Obj.magic None, Deleted)

    (** Create a table. Size will be >= 2 *)
    let create ?(max_load=0.8) size =
      let size = max 2 size in
      { buckets = Array.make size my_null;
        size = 0;
        max_load; }

    (** clear the table, by resetting all states to Empty *)
    let clear t =
      Array.fill t.buckets 0 (Array.length t.buckets) my_null;
      t.size <- 0

    (** Index of slot, for i-th probing starting from hash [h] in
        a table of length [n] *)
    let addr h n i = (h + i) mod n
      
    (** Insert (key -> value) in buckets, starting with the hash. *)
    let insert buckets h key value =
      let n = Array.length buckets in
      (* lookup an empty slot to insert the key->value in. *)
      let rec lookup h n i =
        let j = addr h n i in
        match buckets.(j) with
        | (_, _, Empty) -> buckets.(j) <- (key, value, Used)
        | (key', _, _) when H.equal key key' -> ()
        | _ -> lookup h n (i+1)
      in
      lookup h n 0

    (** Resize the array, by inserting its content into twice as large an array *)
    let resize buckets =
      let buckets' = Array.make (Array.length buckets * 2) my_null in
      for i = 0 to Array.length buckets - 1 do
        match buckets.(i) with
        | (key, value, Used) ->
          insert buckets' (H.hash key) key value  (* insert key -> value into new array *)
        | _ -> ()
      done;
      buckets'

    (** Lookup [key] in the table *)
    let find t key =
      let n = Array.length t.buckets in
      let h = H.hash key in
      let buckets = t.buckets in
      let rec probe h n i num =
        if num = n then raise Not_found
        else
        let j = addr h n i in
        match buckets.(j) with
        | (key', value, Used) when H.equal key key' ->
          value  (* found value for this key *)
        | (_, _, Deleted) | (_, _, Used) ->
          probe h n (i+1) (num + 1) (* try next bucket *)
        | (_, _, Empty) -> raise Not_found
      in
      probe h n 0 0

    (** put [key] -> [value] in the hashtable *)
    let replace t key value =
      let load = float_of_int t.size /. float_of_int (Array.length t.buckets) in
      (if load > t.max_load then t.buckets <- resize t.buckets);
      let n = Array.length t.buckets in
      let h = H.hash key in
      let buckets = t.buckets in
      let rec probe h n i =
        let j = addr h n i in
        match buckets.(j) with
        | (key', _, Used) when H.equal key key' ->
          buckets.(j) <- (key, value, Used)  (* replace value *)
        | (_, _, Deleted) |(_, _, Empty) ->
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
      let h = H.hash key in
      let buckets = t.buckets in
      let rec probe h n i =
        let j = addr h n i in
        match buckets.(j) with
        | (key', _, Used) when H.equal key key' ->
          buckets.(i) <- my_deleted; t.size <- t.size - 1  (* remove slot *)
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
    let fold f t acc =
      let acc = ref acc in
      let buckets = t.buckets in
      for i = 0 to Array.length buckets - 1 do
        match buckets.(i) with
        | (key, value, Used) -> acc := f key value !acc
        | _ -> ()
      done;
      !acc

    (** Statistics on the table *)
    let stats t = (Array.length t.buckets, t.size, t.size, 0, 0, 1)
  end

(** Hashconsed type *)
module type HashconsedType =
  sig
    include Hashtbl.HashedType
    val tag : int -> t -> t
  end

(** Create a hashconsing module *)
module Hashcons(H : HashconsedType) =
  struct
    module Table = Make(H)

    type t = H.t

    let table = Table.create 5003

    let count = ref 0

    let hashcons x =
      try Table.find table x
      with Not_found ->
        let x' = H.tag !count x in
        incr count;
        Table.replace table x' x';
        x'

    let iter k =
      Table.iter (fun _ x -> k x) table

    let stats () =
      Table.stats table
  end
