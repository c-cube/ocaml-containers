
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

(** {1 B-Trees} *)

type 'a sequence = ('a -> unit) -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 signature} *)

module type S = sig
  type key
  type 'a t

  val create : unit -> 'a t
  (** Empty map *)

  val size : _ t -> int
  (** Number of bindings *)

  val add : key -> 'a -> 'a t -> unit
  (** Add a binding to the tree. Erases the old binding, if any *)

  val remove : key -> 'a t -> unit
  (** Remove the given key, or does nothing if the key isn't present *)

  val get : key -> 'a t -> 'a option
  (** Key lookup *)

  val get_exn : key -> 'a t -> 'a
  (** Unsafe version of {!get}.
      @raise Not_found if the key is not present *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on bindings *)

  val of_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list
  val to_tree : 'a t -> (key * 'a) list ktree
end

(** {2 Functor} *)

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module Make(X : ORDERED) = struct
  type key = X.t

  let _len_node = 1 lsl 6
  let _min_len = _len_node / 2

  (* B-tree *)
  type 'a tree =
    | E
    | N of 'a node
    | L of 'a node

  (* an internal node, with children separated by keys/value pairs.
    the [i]-th key of [n.keys] separates the subtrees [n.children.(i)] and
    [n.children.(i+1)] *)
  and 'a node = {
    keys : key array;
    values : 'a array;
    children : 'a tree array; (* with one more slot *)
    mutable size : int;  (* number of bindings in the [key] *)
  }

  type 'a t = {
    mutable root : 'a tree;
    mutable cardinal : int;
  }

  let is_empty = function
    | E -> true
    | N _
    | L _ -> false

  let create () = {
    root=E;
    cardinal=0;
  }

  (* build a new leaf with the given binding *)
  let _make_singleton k v = {
    keys = Array.make _len_node k;
    values = Array.make _len_node v;
    children = Array.make (_len_node+1) E;
    size = 1;
  }

  (* slice of [l] starting at indices [i], of length [len]. Only
    copies inner children (between two keys in the range). *)
  let _make_slice l i len =
    assert (len>0);
    assert (i+len<=l.size);
    let k = l.keys.(i) and v = l.values.(i)  in
    let l' = {
      keys = Array.make _len_node k;
      values = Array.make _len_node v;
      children = Array.make (_len_node+1) E;
      size = len;
    } in
    Array.blit l.keys i l'.keys 0 len;
    Array.blit l.values i l'.values 0 len;
    Array.blit l.children (i+1) l'.children 1 (len-1);
    l'

  let _full_node n = n.size = _len_node
  let _empty_node n = n.size = 0

  let size t = t.cardinal

  let rec _fold f acc t = match t with
    | E -> ()
    | L n ->
        for i=0 to n.size-1 do
          assert (n.children.(i) = E);
          acc := f !acc n.keys.(i) n.values.(i)
        done
    | N n ->
        for i=0 to n.size-1 do
          _fold f acc n.children.(i);
          acc := f !acc n.keys.(i) n.values.(i);
        done;
        _fold f acc n.children.(n.size)

  let fold f acc t =
    let acc = ref acc in
    _fold f acc t.root;
    !acc

  type lookup_result =
    | At of int
    | After of int

  (* lookup in a node. *)
  let rec _lookup_rec l k i =
    if i = l.size then After (i-1)
    else match X.compare k l.keys.(i) with
      | 0 -> At i
      | n when n<0 -> After (i-1)
      | _ -> _lookup_rec l k (i+1)

  let _lookup l k =
    if l.size = 0 then After ~-1
    else _lookup_rec l k 0

  (* recursive lookup in a tree *)
  let rec _get_exn k t = match t with
    | E -> raise Not_found
    | L l ->
        begin match _lookup l k with
        | At i -> l.values.(i)
        | After _ -> raise Not_found
        end
    | N n ->
        assert (n.size > 0);
        match _lookup n k with
        | At i -> n.values.(i)
        | After i -> _get_exn k n.children.(i+1)

  let get_exn k t = _get_exn k t.root

  let get k t =
    try Some (_get_exn k t.root)
    with Not_found -> None

  (* sorted insertion into a leaf that has room and doesn't contain the key *)
  let _insert_sorted l k v i =
    assert (not (_full_node l));
    (* make room by shifting to the right *)
    let len = l.size - i in
    assert (i+len<=l.size);
    assert (len>=0);
    Array.blit l.keys i l.keys (i+1) len;
    Array.blit l.values i l.values (i+1) len;
    l.keys.(i) <- k;
    l.values.(i) <- v;
    l.size <- l.size + 1;

  (* what happens when we insert a value *)
  type 'a add_result =
    | NewTree of 'a tree
    | Add
    | Replace
    | Split of 'a tree * key * 'a * 'a tree

  let _add_leaf k v t l =
    match _lookup l k with
    | At i ->
        l.values.(i) <- v;
        Replace
    | After i ->
        if _full_node l
        then (
          (* split. [k'] and [v']: separator for split *)
          let j = _len_node/2 in
          let left = _make_slice l 0 j in
          let right = _make_slice l (j+1) (_len_node-j-1) in
          (* insert in proper sub-leaf *)
          (if i+1<j
            then _insert_sorted left k v (i+1)
            else _insert_sorted right k v (i-j)
          );
          let k' = l.keys.(j) in
          let v' = l.values.(j) in
          Split (L left, k', v', L right)
        ) else (
          (* just insert at sorted position *)
          _insert_sorted l k v (i+1);
          Add
        )

  let _insert_node n i k v sub1 sub2 =
    assert (not(_full_node n));
    let len = n.size - i in
    assert (len>=0);
    Array.blit n.keys i n.keys (i+1) len;
    Array.blit n.values i n.values (i+1) len;
    Array.blit n.children (i+1) n.children (i+2) len;
    n.keys.(i) <- k;
    n.values.(i) <- v;
    (* erase subtree with sub1,sub2 *)
    n.children.(i) <- sub1;
    n.children.(i+1) <- sub2;
    n.size <- n.size + 1;
    ()

  (* return a boolean indicating whether the key was already
    present, and a new tree. *)
  let rec _add k v t = match t with
    | E -> NewTree (L (_make_singleton k v))
    | L l -> _add_leaf k v t l
    | N n ->
        match _lookup n k with
        | At i ->
            n.values.(i) <- v;
            Replace
        | After i ->
          assert (X.compare n.keys.(i) k < 0);
          let sub = n.children.(i+1) in
          match _add k v sub with
          | NewTree t' ->
              n.children.(i+1) <- t';
              Add
          | Add -> Add
          | Replace -> Replace
          | Split (sub1, k', v', sub2) ->
              assert (X.compare n.keys.(i) k' < 0);
              if _full_node n
              then (
                (* split this node too! *)
                let j = _len_node/2 in
                let left = _make_slice n 0 j in
                let right = _make_slice n (j+1) (_len_node-j-1) in
                left.children.(0) <- n.children.(0);
                right.children.(_len_node-j) <- n.children.(_len_node);
                (* insert k' and subtrees in the correct tree *)
                (if i<j
                  then _insert_node left (i+1) k' v' sub1 sub2
                  else _insert_node right (i+1-j) k' v' sub1 sub2
                );
                (* return the split tree *)
                let k'' = n.keys.(j) in
                let v'' = n.values.(j) in
                Split (N left, k'', v'', N right)
              ) else (
                (* insertion of [k] at position [i+1] *)
                _insert_node n (i+1) k' v' sub1 sub2;
                Add
              )

  let add k v t =
    match _add k v t.root with
      | NewTree t' ->
          t.cardinal <- t.cardinal + 1;
          t.root <- t'
      | Replace -> ()
      | Add -> t.cardinal <- t.cardinal + 1
      | Split (sub1, k, v, sub2) ->
          (* make a new root with one child *)
          let n = _make_singleton k v in
          n.children.(0) <- sub1;
          n.children.(1) <- sub2;
          t.cardinal <- t.cardinal + 1;
          t.root <- N n

  let of_list l =
    let t = create() in
    List.iter (fun (k, v) -> add k v t) l;
    t

  let to_list t =
    List.rev (fold (fun acc k v -> (k,v)::acc) [] t)

  let rec _to_tree t () = match t with
    | E -> `Nil
    | L n
    | N n ->
        let l = ref [] and children = ref [] in
        for i=0 to n.size-1 do
          l := (n.keys.(i),n.values.(i)) :: !l;
          children := n.children.(i) :: !children
        done;
        children := n.children.(n.size) :: !children;
        children := List.filter (function E -> false | _ -> true) !children;
        `Node (List.rev !l, List.rev_map _to_tree !children)

  let to_tree t = _to_tree t.root

  (*$T
    let module T = Make(CCInt) in \
      let t = T.of_list (CCList.(1--1000) |> List.map (fun x->x, string_of_int x)) in \
      T.get 1 t = Some "1"
    let module T = Make(CCInt) in \
      let t = T.of_list (CCList.(1--1000) |> List.map (fun x->x, string_of_int x)) in \
      T.get 3 t = Some "3"
    let module T = Make(CCInt) in \
      let t = T.of_list (CCList.(1--100) |> List.map (fun x->x, string_of_int x)) in \
      T.get 400 t = None
  *)

  (* remove the key if present.  TODO
  let rec _remove k t = match t with
    | E -> false, E
    | N n ->
        assert (n.size > 0);
        if X.compare k (_min_key n) < 0
        then (
          let removed, left' = _remove k n.left in
          n.left <- left';
          n.depth <- 1+max (_depth n.left) (_depth n.right);
          removed, _balance t
        ) else if X.compare k (_max_key n) > 0
        then (
          let removed, right' = _remove k n.right in
          n.right <- right';
          n.depth <- 1+max (_depth n.left) (_depth n.right);
          removed, _balance t
        )
        else try
          let i = _lookup n k 0 in
          if n.size = 1  (* TODO: actually minimal threshold should be higher *)
          then true, E
          else (
            let len = n.size - i in
            Array.blit n.keys (i+1) n.keys i len;
            Array.blit n.values (i+1) n.values i len;
            true, t
          )
        with Not_found ->
          false, t  (* not to be removed *)
  *)

  let remove k t = assert false (* TODO *)
end
