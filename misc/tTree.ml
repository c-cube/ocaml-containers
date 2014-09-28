
(*
copyright (c) 2013, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
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

(** {1 T-Trees} *)

(** {2 Persistent array}

The nodes of the tree are arrays, but to expose a persistent interface we
use persistent arrays. *)

module PArray = struct
  type 'a t = 'a zipper ref
  and 'a zipper =
    | Array of 'a array
    | Diff of int * 'a * 'a zipper ref

  (* XXX maybe having a snapshot of the array from point to point may help? *)

  let make size elt =
    let a = Array.make size elt in
    ref (Array a)

  (** Recover the given version of the shared array. Returns the array
      itself. *)
  let rec reroot t =
    match !t with
    | Array a -> a
    | Diff (i, v, t') ->
      begin
        let a = reroot t' in
        let v' = a.(i) in
        t' := Diff (i, v', t);
        a.(i) <- v;
        t := Array a;
        a
      end

  let get t i =
    match !t with
    | Array a -> a.(i)
    | Diff _ -> 
      let a = reroot t in
      a.(i)

  let set t i v =
    let a =
      match !t with
      | Array a -> a
      | Diff _ -> reroot t in
    let v' = a.(i) in
    if v == v'
      then t (* no change *)
      else begin
        let t' = ref (Array a) in
        a.(i) <- v;
        t := Diff (i, v', t');
        t' (* create new array *)
      end

  let fold_left f acc t =
    let a = reroot t in
    Array.fold_left f acc a

  let rec length t =
    match !t with
    | Array a -> Array.length a
    | Diff (_, _, t') -> length t'
end

(** {2 signature} *)

module type S = sig
  type key

  type 'a t

  val empty : 'a t
    (** Empty tree *)

  val add : 'a t -> key -> 'a -> 'a t
    (** Add a binding key/value. If the key already was bound to some
        value, the old binding is erased. *)

  val remove : 'a t -> key -> 'a t
    (** Remove the key *)

  val find : 'a t -> key -> 'a
    (** Find the element associated with this key.
        @raise Not_found if the key is not present *)

  val length : 'a t -> int
    (** Number of bindings *)

  val fold : 'a t -> 'b -> ('b -> key -> 'a -> 'b) -> 'b
    (** Fold on bindings *)
end

(** {2 Functor} *)

module Make(X : Set.OrderedType) = struct
  type key = X.t

  (* bucket that maps a key to a value *)
  type 'a bucket =
    | B_none
    | B_some of key * 'a

  (* recursive tree type *)
  type 'a node = {
    left : 'a node option;
    right : 'a node option;
    depth : int;
    buckets : 'a bucket PArray.t;
  }

  (* to avoid the value restriction, we need to make a special case for
      the empty tree *)
  type 'a t =
    | E
    | N of 'a node

  let empty = E

  let add tree k v = assert false

  let remove tree k = assert false

  let find tree k =
    let rec find node k = assert false (* TODO *)
    in
    match tree with
    | E -> raise Not_found
    | N node -> find node k

  let length tree = assert false

  let fold tree acc f = assert false
end
