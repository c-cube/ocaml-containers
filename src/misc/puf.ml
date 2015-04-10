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

(** {1 Functional (persistent) extensible union-find} *)

(** {2 Persistent array} *)

module PArray = struct
  type 'a t = 'a zipper ref
  and 'a zipper =
    | Array of 'a array
    | Diff of int * 'a * 'a t

  (* XXX maybe having a snapshot of the array from point to point may help? *)

  let make size elt =
    let a = Array.make size elt in
    ref (Array a)

  let init size f =
    let a = Array.init size f in
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

  let iteri f t = Array.iteri f (reroot t)

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

  let rec length t =
    match !t with
    | Array a -> Array.length a
    | Diff (_, _, t') -> length t'

  (** Extend [t] to the given [size], initializing new elements with [elt] *)
  let extend t size elt =
    let a = match !t with
    | Array a -> a
    | _ -> reroot t in
    if size > Array.length a
      then begin  (* resize: create bigger array *)
        let size = min Sys.max_array_length size in
        let a' = Array.make size elt in
        (* copy old part *)
        Array.blit a 0 a' 0 (Array.length a);
        t := Array a'
      end

  (** Extend [t] to the given [size], initializing elements with [f] *)
  let extend_init t size f =
    let a = match !t with
    | Array a -> a
    | _ -> reroot t in
    if size > Array.length a
      then begin  (* resize: create bigger array *)
        let size = min Sys.max_array_length size in
        let a' = Array.init size f in
        (* copy old part *)
        Array.blit a 0 a' 0 (Array.length a);
        t := Array a'
      end

  let fold_left f acc t =
    let a = reroot t in
    Array.fold_left f acc a
end

(** {2 Persistent Bitvector} *)

module PBitVector = struct
  type t = int PArray.t

  let width = Sys.word_size - 1   (* number of usable bits in an integer *)

  let make size = PArray.make size 0

  let ensure bv offset =
    if offset >= PArray.length bv
      then
        let len = offset + offset/2 + 1 in
        PArray.extend bv len 0
      else ()

  (** [get bv i] gets the value of the [i]-th element of [bv] *)
  let get bv i =
    let offset = i / width in
    let bit = i mod width in
    ensure bv offset;
    let bits = PArray.get bv offset in
    (bits land (1 lsl bit)) <> 0

  (** [set bv i v] sets the value of the [i]-th element of [bv] to [v] *)
  let set bv i v =
    let offset = i / width in
    let bit = i mod width in
    ensure bv offset;
    let bits = PArray.get bv offset in
    let bits' =
      if v
        then bits lor (1 lsl bit)
        else bits land (lnot (1 lsl bit))
    in
    PArray.set bv offset bits'

  (** Bitvector with all bits set to 0 *)
  let clear bv = make 5

  let set_true bv i = set bv i true
  let set_false bv i = set bv i false
end

(** {2 Type with unique identifier} *)

module type ID = sig
  type t
  val get_id : t -> int
end

(** {2 Persistent Union-Find with explanations} *)

module type S = sig
  type elt
    (** Elements of the Union-find *)

  type 'e t
    (** An instance of the union-find, ie a set of equivalence classes; It
        is parametrized by the type of explanations. *)

  val create : int -> 'e t
    (** Create a union-find of the given size. *)

  val find : 'e t -> elt -> elt
    (** [find uf a] returns the current representative of [a] in the given
        union-find structure [uf]. By default, [find uf a = a]. *)

  val union : 'e t -> elt -> elt -> 'e -> 'e t
    (** [union uf a b why] returns an update of [uf] where [find a = find b],
        the merge being justified by [why]. *)

  val distinct : 'e t -> elt -> elt -> 'e t
    (** Ensure that the two elements are distinct. *)

  val must_be_distinct : _ t -> elt -> elt -> bool
    (** Should the two elements be distinct? *)

  val fold_equiv_class : _ t -> elt -> ('a -> elt -> 'a) -> 'a -> 'a
    (** [fold_equiv_class uf a f acc] folds on [acc] and every element
        that is congruent to [a] with [f]. *)

  val iter_equiv_class : _ t -> elt -> (elt -> unit) -> unit
    (** [iter_equiv_class uf a f] calls [f] on every element of [uf] that
        is congruent to [a], including [a] itself. *)

  val iter : _ t -> (elt -> unit) -> unit
    (** Iterate on all root values *)

  val inconsistent : _ t -> (elt * elt * elt * elt) option
    (** Check whether the UF is inconsistent. It returns [Some (a, b, a', b')]
        in case of inconsistency, where a = b, a = a' and b = b' by congruence,
        and a' != b' was a call to [distinct]. *)

  val common_ancestor : 'e t -> elt -> elt -> elt
    (** Closest common ancestor of the two elements in the proof forest *)

  val explain_step : 'e t -> elt -> (elt * 'e) option
    (** Edge from the element to its parent in the proof forest; Returns
        None if the element is a root of the forest. *)

  val explain : 'e t -> elt -> elt -> 'e list
    (** [explain uf a b] returns a list of labels that justify why
        [find uf a = find uf b]. Such labels were provided by [union]. *)

  val explain_distinct : 'e t -> elt -> elt -> elt * elt
    (** [explain_distinct uf a b] gives the original pair [a', b'] that
        made [a] and [b] distinct by calling [distinct a' b']. The
        terms must be distinct, otherwise Failure is raised. *)
end

module IH = Hashtbl.Make(struct type t = int let equal i j = i = j let hash i = i end)

module Make(X : ID) : S with type elt = X.t = struct
  type elt = X.t

  type 'e t = {
    mutable parent : int PArray.t;      (* idx of the parent, with path compression *)
    mutable data : elt_data option PArray.t;        (* ID -> data for an element *)
    inconsistent : (elt * elt * elt * elt) option;  (* is the UF inconsistent? *)
    forest : 'e edge PArray.t;          (* explanation forest *)
  } (** An instance of the union-find, ie a set of equivalence classes *)
  and elt_data = {
    elt : elt;
    size : int;                         (* number of elements in the class *)
    next : int;                         (* next element in equiv class *)
    distinct : (int * elt * elt) list;  (* classes distinct from this one, and why *)
  } (** Data associated to the element. Most of it is only meaningful for
        a representative (ie when elt = parent(elt)). *)
  and 'e edge =
    | EdgeNone
    | EdgeTo of int * 'e
    (** Edge of the proof forest, annotated with 'e *)

  let get_data uf id =
    match PArray.get uf.data id with
    | Some data -> data
    | None -> assert false

  (** Create a union-find of the given size. *)
  let create size =
    { parent = PArray.init size (fun i -> i);
      data = PArray.make size None;
      inconsistent = None;
      forest = PArray.make size EdgeNone;
    }

  (* ensure the arrays are big enough for [id], and set [elt.(id) <- elt] *)
  let ensure uf id elt =
    if id >= PArray.length uf.data then begin
      (* resize *)
      let len = id + (id / 2) in
      PArray.extend_init uf.parent len (fun i -> i);
      PArray.extend uf.data len None;
      PArray.extend uf.forest len EdgeNone;
    end;
    match PArray.get uf.data id with
    | None ->
      let data = { elt; size = 1; next=id; distinct=[]; } in
      uf.data <- PArray.set uf.data id (Some data)
    | Some _ -> ()

  (* Find the ID of the root of the given ID *)
  let rec find_root uf id =
    let parent_id = PArray.get uf.parent id in
    if id = parent_id
      then id
      else begin (* recurse *)
        let root = find_root uf parent_id in
        (* path compression *)
        (if root <> parent_id then uf.parent <- PArray.set uf.parent id root);
        root
      end

  (** [find uf a] returns the current representative of [a] in the given
      union-find structure [uf]. By default, [find uf a = a]. *)
  let find uf elt =
    let id = X.get_id elt in
    if id >= PArray.length uf.parent
      then elt  (* not present *)
      else
        let id' = find_root uf id in
        match PArray.get uf.data id' with
        | Some data -> data.elt
        | None -> assert (id = id'); elt  (* not present *)

  (* merge i and j in the forest, with explanation why *)
  let rec merge_forest forest i j why =
    assert (i <> j);
    (* invert path from i to roo, reverting all edges *)
    let rec invert_path forest i =
      match PArray.get forest i with
      | EdgeNone -> forest  (* reached root *)
      | EdgeTo (i', e) ->
        let forest' = invert_path forest i' in
        PArray.set forest' i' (EdgeTo (i, e))
    in
    let forest = invert_path forest i in
    (* root of [j] is the new root of [i] and [j] *)
    let forest = PArray.set forest i (EdgeTo (j, why)) in
    forest

  (** Merge the class of [a] (whose representative is [ia'] into the class
      of [b], whose representative is [ib'] *)
  let merge_into uf a ia' b ib' why =
    let data_a = get_data uf ia' in
    let data_b = get_data uf ib' in
    (* merge roots (a -> b, arbitrarily) *)
    let parent = PArray.set uf.parent ia' ib' in
    (* merge 'distinct' lists: distinct(b) <- distinct(b)+distinct(a) *)
    let distinct' = List.rev_append data_a.distinct data_b.distinct in
    (* size of the new equivalence class *)
    let size' = data_a.size + data_b.size in
    (* concatenation of circular linked lists (equivalence classes),
       concatenation of distinct lists *)
    let data_a' = {data_a with next=data_b.next; } in
    let data_b' = {data_b with next=data_a.next; distinct=distinct'; size=size'; } in
    let data = PArray.set uf.data ia' (Some data_a') in
    let data = PArray.set data ib' (Some data_b') in
    (* inconsistency check *)
    let inconsistent =
      List.fold_left
        (fun acc (id, a', b') -> match acc with
          | Some _ -> acc
          | None when find_root uf id = ib' -> Some (a, b, a', b')  (* found! *)
          | None -> None)
        None data_a.distinct
    in
    (* update forest *)
    let forest = merge_forest uf.forest (X.get_id a) (X.get_id b) why in
    { parent; data; inconsistent; forest; }

  (** [union uf a b why] returns an update of [uf] where [find a = find b],
      the merge being justified by [why]. *)
  let union uf a b why =
    (if uf.inconsistent <> None
      then raise (Invalid_argument "inconsistent uf"));
    let ia = X.get_id a in
    let ib = X.get_id b in
    (* get sure we can access [ia] and [ib] in [uf] *)
    ensure uf ia a;
    ensure uf ib b;
    (* indexes of roots of [a] and [b] *)
    let ia' = find_root uf ia
    and ib' = find_root uf ib in
    if ia' = ib'
      then uf  (* no change *)
      else
        (* data associated to both representatives *)
        let data_a = get_data uf ia' in
        let data_b = get_data uf ib' in
        (* merge the smaller class into the bigger class *)
        if data_a.size > data_b.size
          then merge_into uf b ib' a ia' why
          else merge_into uf a ia' b ib' why

  (** Ensure that the two elements are distinct. May raise Inconsistent *)
  let distinct uf a b =
    (if uf.inconsistent <> None
      then raise (Invalid_argument "inconsistent uf"));
    let ia = X.get_id a in
    let ib = X.get_id b in
    ensure uf ia a;
    ensure uf ib b;
    (* representatives of a and b *)
    let ia' = find_root uf ia in
    let ib' = find_root uf ib in
    (* update 'distinct' lists *)
    let data_a = get_data uf ia' in
    let data_a' = {data_a with distinct= (ib',a,b) :: data_a.distinct; } in
    let data_b = get_data uf ib' in
    let data_b' = {data_b with distinct= (ia',a,b) :: data_b.distinct; } in
    let data = PArray.set uf.data ia' (Some data_a') in
    let data = PArray.set data ib' (Some data_b') in
    (* check inconsistency *)
    let inconsistent = if ia' = ib' then Some (data_a.elt, data_b.elt, a, b) else None in
    { uf with inconsistent; data; }

  let must_be_distinct uf a b =
    let ia = X.get_id a in
    let ib = X.get_id b in
    let len = PArray.length uf.parent in
    if ia >= len || ib >= len
      then false  (* no chance *)
      else
        (* representatives *)
        let ia' = find_root uf ia in
        let ib' = find_root uf ib in
        (* list of equiv classes that must be != a *)
        match PArray.get uf.data ia' with
        | None -> false  (* ia' not present *)
        | Some data_a ->
          List.exists (fun (id,_,_) -> find_root uf id = ib') data_a.distinct

  (** [fold_equiv_class uf a f acc] folds on [acc] and every element
      that is congruent to [a] with [f]. *)
  let fold_equiv_class uf a f acc =
    let ia = X.get_id a in
    if ia >= PArray.length uf.parent
      then f acc a  (* alone. *)
      else
        let rec traverse acc id =
          match PArray.get uf.data id with
          | None -> f acc a  (* alone. *)
          | Some data ->
            let acc' = f acc data.elt in
            let id' = data.next in
            if id' = ia
              then acc'  (* traversed the whole list *)
              else traverse acc' id'
        in
        traverse acc ia

  (** [iter_equiv_class uf a f] calls [f] on every element of [uf] that
      is congruent to [a], including [a] itself. *)
  let iter_equiv_class uf a f =
    let ia = X.get_id a in
    if ia >= PArray.length uf.parent
      then f a  (* alone. *)
      else
        let rec traverse id =
          match PArray.get uf.data id with
          | None -> f a  (* alone. *)
          | Some data ->
            f data.elt;  (* yield element *)
            let id' = data.next in
            if id' = ia
              then ()  (* traversed the whole list *)
              else traverse id'
        in
        traverse ia

  let iter uf f =
    PArray.iteri
      (fun i i' ->
         if i = i' then match PArray.get uf.data i with
         | None -> ()
         | Some d -> f d.elt
      ) uf.parent

  let inconsistent uf = uf.inconsistent

  (** Closest common ancestor of the two elements in the proof forest *)
  let common_ancestor uf a b =
    let forest = uf.forest in
    let explored = IH.create 3 in
    let rec recurse i j =
      if i = j
        then return i  (* found *)
      else if IH.mem explored i
        then return i
      else if IH.mem explored j
        then return j
      else
        let i' = match PArray.get forest i with
          | EdgeNone -> i
          | EdgeTo (i', e) ->
            IH.add explored i ();
            i'
        and j' = match PArray.get forest j with
          | EdgeNone -> j
          | EdgeTo (j', e) ->
            IH.add explored j ();
            j'
        in
        recurse i' j'
    and return i =
      (get_data uf i).elt  (* return the element *)
    in
    recurse (X.get_id a) (X.get_id b)

  (** Edge from the element to its parent in the proof forest; Returns
      None if the element is a root of the forest. *)
  let explain_step uf a =
    match PArray.get uf.forest (X.get_id a) with
    | EdgeNone -> None
    | EdgeTo (i, e) ->
      let b = (get_data uf i).elt in
      Some (b, e)

  (** [explain uf a b] returns a list of labels that justify why
      [find uf a = find uf b]. Such labels were provided by [union]. *)
  let explain uf a b =
    (if find_root uf (X.get_id a) <> find_root uf (X.get_id b)
      then failwith "Puf.explain: can only explain equal terms");
    let c = common_ancestor uf a b in
    (* path from [x] to [c] *)
    let rec build_path path x =
      if (X.get_id x) = (X.get_id c)
        then path
        else match explain_step uf x with
          | None -> assert false
          | Some (x', e) ->
            build_path (e::path) x'
    in
    build_path (build_path [] a) b

  (** [explain_distinct uf a b] gives the original pair [a', b'] that
      made [a] and [b] distinct by calling [distinct a' b']. The
      terms must be distinct, otherwise Failure is raised. *)
  let explain_distinct uf a b =
    let ia' = find_root uf (X.get_id a) in
    let ib' = find_root uf (X.get_id b) in
    let node_a = get_data uf ia' in
    let rec search l = match l with
      | [] -> failwith "Puf.explain_distinct: classes are not distinct"
      | (ib'', a', b')::_ when ib' = ib'' -> (a', b')  (* explanation found *)
      | _ :: l' -> search l'
    in
    search node_a.distinct
end
