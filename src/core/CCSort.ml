
(** Reusable sort algorithm *)

(** Array-like structure.

    We only require constant-time indexing inside the array. *)
module type ARRAY = sig
  type elt
  type t

  val compare : elt -> elt -> int

  val length : t -> int

  val get : t -> int -> elt

  val set : t -> int -> elt -> unit

  val swap : t -> int -> int -> unit
end

type 'a compare = ('a -> 'a -> int)
(** Type for a comparison function *)

module type S = sig
  module A : ARRAY

  val sort_slice : A.t -> i:int -> len:int -> unit

  val sort : A.t -> unit

  val sort_uniq_slice : A.t -> i:int -> len:int -> int

  val sort_uniq : A.t -> int
end

module Make(A : ARRAY)
  : S with module A = A
= struct
  module A = A
  open A

  let rec insert_ a i k =
    if k<i then ()
    else if compare (A.get a k) (A.get a (k+1)) > 0 then (
      swap a k (k+1);
      insert_ a i (k-1)
    )

  let insertion_sort_ a ~i:first ~j:last =
    if first+1<last then (
      for i=first +1 to last-1 do
        let j= ref i in
        (* insert a[i] into slice [start… i-1] *)
        while !j > first && A.compare (get a (!j - 1)) (get a !j) > 0 do
          swap a (!j - 1) !j;
          decr j;
        done;
      done
    )

  (* sort between [i] and [j], both included *)
  let rec sort_slice_ a ~i ~j : unit =
    if j - i < 2 then ()
    else if j - i < 16 then (
      (* base case *)
      insertion_sort_ a ~i ~j
    ) else (
      (* quicksort *)

    )

  let sort_slice a ~i ~len =
    if len > 1 then sort_slice_ a ~i ~j:(i+len-1)

  let[@inline] sort a = sort_slice a ~i:0 ~len:(A.length a)

  (* assume [a[i:i+len]] is sorted, return new length *)
  let uniq_ a ~i:i0 ~len : int =
    let cur = ref i0 in
    let i = ref i0 in
    while !i < i0 + len do
      let x = get a !i in
      if A.compare x (get a !cur) = 0 then (
        incr i; (* dup, skip it *)
      ) else (
        (* move it to [!cur] *)
        if !i <> !cur then set a !cur x;
        incr i;
        incr cur;
      );
    done;
    !cur - i0

  let sort_uniq_slice a ~i ~len : int =
    sort_slice a ~i ~len;
    uniq_ a ~i ~len

  let[@inline] sort_uniq a = sort_uniq_slice a ~i:0 ~len:(A.length a)
end[@inline]

(*
(* Dual Pivot Quicksort (Yaroslavskiy)
   from "average case analysis of Java 7's Dual Pivot Quicksort" *)
module SortGeneric(A : MONO_ARRAY) = struct
  module Rand = Random.State

  let seed_ = [|123456|]

  type state = {
    mutable l: int; (* left pointer *)
    mutable g: int; (* right pointer *)
    mutable k: int;
  }

  let rand_idx_ rand i j = i + Rand.int rand (j-i)

  let swap_ a i j =
    if i=j then ()
    else (
      let tmp = A.get a i in
      A.set a i (A.get a j);
      A.set a j tmp
    )

  let sort ~cmp a =
    let rec insert_ a i k =
      if k<i then ()
      else if cmp (A.get a k) (A.get a (k+1)) > 0 then (
        swap_ a k (k+1);
        insert_ a i (k-1)
      )
    in
    (* recursive part of insertion sort *)
    let rec sort_insertion_rec a i j k =
      if k<j then (
        insert_ a i (k-1);
        sort_insertion_rec a i j (k+1)
      )
    in
    (* insertion sort, for small slices *)
    let sort_insertion a i j =
      if j-i > 1 then sort_insertion_rec a i j (i+1)
    in
    let rand = Rand.make seed_ in
    (* sort slice.
       There is a chance that the two pivots are equal, but it's unlikely. *)
    let rec sort_slice_ ~st a i j =
      if j-i>10 then (
        st.l <- i;
        st.g <- j-1;
        st.k <- i;
        (* choose pivots *)
        let p = A.get a (rand_idx_ rand i j) in
        let q = A.get a (rand_idx_ rand i j) in
        (* invariant: st.p <= st.q, swap them otherwise *)
        let p, q = if cmp p q > 0 then q, p else p, q in
        while st.k <= st.g do
          let cur = A.get a st.k in
          if cmp cur p < 0 then (
            (* insert in leftmost band *)
            if st.k <> st.l then swap_ a st.k st.l;
            st.l <- st.l + 1
          ) else if cmp cur q > 0 then (
            (* insert in rightmost band *)
            while st.k < st.g && cmp (A.get a st.g) q > 0 do
              st.g <- st.g - 1
            done;
            swap_ a st.k st.g;
            st.g <- st.g - 1;
            (* the element swapped from the right might be in the first situation.
               that is, < p  (we know it's <= q already) *)
            if cmp (A.get a st.k) p < 0 then (
              if st.k <> st.l then swap_ a st.k st.l;
              st.l <- st.l + 1
            )
          );
          st.k <- st.k + 1
        done;
        (* save values before recursing *)
        let l = st.l and g = st.g and sort_middle = cmp p q < 0 in
        sort_slice_ ~st a i l;
        if sort_middle then sort_slice_ ~st a l (g+1);
        sort_slice_ ~st a (g+1) j;
      ) else sort_insertion a i j
    in
    if A.length a > 0 then (
      let st = { l=0; g=A.length a; k=0; } in
      sort_slice_ ~st a 0 (A.length a)
    )
end
   *)
