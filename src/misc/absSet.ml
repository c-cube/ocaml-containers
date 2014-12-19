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

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
  damages (including, but not limited to, procurement of substitute goods or
  services; loss of use, data, or profits; or business interruption) however
  caused and on any theory of liability, whether in contract, strict liability,
  or tort (including negligence or otherwise) arising in any way out of the use
  of this software, even if advised of the possibility of such damage.
*)

(** {1 Abstract set/relation} *)

type 'a sequence = ('a -> unit) -> unit

type 'a t = {
  mem : 'a -> bool;
  iter : ('a -> unit) -> unit;
  cardinal : unit -> int;
} (** The abstract set *)

let empty = {
  mem = (fun _ -> false);
  iter = (fun _ -> ());
  cardinal = (fun () -> 0);
}

let mem set x = set.mem x

let iter set k = set.iter k

let fold set acc f =
  let acc = ref acc in
  set.iter (fun x -> acc := f !acc x);
  !acc

let cardinal set = set.cardinal ()

let singleton ?(eq=(=)) x =
  let mem y = eq x y in
  let iter k = k x in
  let cardinal () = 1 in
  { mem; iter; cardinal; }

(* basic cardinal computation, by counting elements *)
let __default_cardinal iter =
  fun () ->
    let r = ref 0 in
    iter (fun _ -> incr r);
    !r

let mk_generic ?cardinal ~mem ~iter =
  let cardinal = match cardinal with
  | Some c -> c
  | None ->  __default_cardinal iter   (* default implementation *)
  in
  { mem; iter; cardinal; }

let of_hashtbl h =
  let mem x = Hashtbl.mem h x in
  let iter k = Hashtbl.iter (fun x _ -> k x) h in
  let cardinal () = Hashtbl.length h in
  { mem; iter; cardinal; }

let filter set pred =
  let mem x = set.mem x && pred x in
  let iter k = set.iter (fun x -> if pred x then k x) in
  let cardinal = __default_cardinal iter in
  { mem; iter; cardinal; }

let union s1 s2 =
  let mem x = s1.mem x || s2.mem x in
  let iter k =
    s1.iter k;
    s2.iter (fun x -> if not (s1.mem x) then k x);
  in
  let cardinal = __default_cardinal iter in
  { mem; iter; cardinal; }

let intersection s1 s2 =
  let mem x = s1.mem x && s2.mem x in
  let iter k = s1.iter (fun x -> if s2.mem x then k x) in
  let cardinal = __default_cardinal iter in
  { mem; iter; cardinal; }

let product s1 s2 =
  let mem (x,y) = s1.mem x && s2.mem y in
  let iter k =
    s1.iter (fun x -> s2.iter (fun y -> k (x,y))) in
  let cardinal () = s1.cardinal () * s2.cardinal () in
  { mem; iter; cardinal; }

let to_seq set k = set.iter k

let to_list set =
  let l = ref [] in
  set.iter (fun x -> l := x :: !l);
  !l

(** {2 Set builders} *)

(** A set builder is a value that serves to build a set, element by element.
    Several implementations can be provided, but the two operations that
    must be present are:

    - add an element to the builder
    - extract the set composed of all elements added so far
*)

type 'a builder = {
  add : 'a -> unit;
  get : unit -> 'a t;
}

let mk_builder ~add ~get =
  { add; get; }

let builder_hash (type k) ?(size=15) ?(eq=(=)) ?(hash=Hashtbl.hash) () =
  let module H = Hashtbl.Make(struct type t = k let equal = eq let hash = hash end) in
  let h = H.create size in
  let add x = H.replace h x () in
  let get () =
    let mem x = H.mem h x in
    let iter k = H.iter (fun x _ -> k x) h in
    let cardinal () = H.length h in
    mk_generic ~cardinal ~mem ~iter
  in
  mk_builder ~add ~get

let builder_cmp (type k) ?(cmp=Pervasives.compare) () =
  let module S = Set.Make(struct type t = k let compare = cmp end) in
  let s = ref S.empty in
  let add x = s := S.add x !s in
  let get () =
    let s' = !s in
    let mem x = S.mem x s' in
    let iter k = S.iter k s' in
    let cardinal () = S.cardinal s' in
    mk_generic ~cardinal ~mem ~iter
  in
  mk_builder ~add ~get

let of_seq_builder ~builder seq =
  seq builder.add;
  builder.get ()

let of_seq_hash ?eq ?hash seq =
  let b = builder_hash ?eq ?hash () in
  of_seq_builder b seq

let of_seq_cmp ?cmp seq =
  let b = builder_cmp ?cmp () in
  of_seq_builder b seq

let of_list l = of_seq_hash (fun k -> List.iter k l)

let map ?(builder=builder_hash ()) set ~f =
  set.iter
    (fun x ->
      let y = f x in
      builder.add y);
  builder.get ()

(* relational join *)
let hash_join
  (type k) ?(eq=(=)) ?(size=20) ?(hash=Hashtbl.hash) ?(builder=builder_hash ())
  ~project1 ~project2 ~merge s1 s2
  =
  let module H = Hashtbl.Make(struct type t = k let equal = eq let hash = hash end) in
  let h = H.create size in
  s1.iter
    (fun x ->
      let key = project1 x in
      H.add h key x);
  s2.iter
    (fun y ->
      let key = project2 y in
      let xs = H.find_all h key in
      List.iter (fun x -> builder.add (merge x y)) xs);
  builder.get ()

(** {2 Functorial interfaces} *)

module MakeHash(X : Hashtbl.HashedType) = struct
  type elt = X.t
    (** Elements of the set are hashable *)

  module H = Hashtbl.Make(X)

  let of_seq ?(size=5) seq =
    let h = Hashtbl.create size in
    seq (fun x -> Hashtbl.add h x ());
    let mem x = Hashtbl.mem h x in
    let iter k = Hashtbl.iter (fun x () -> k x) h in
    let cardinal () = Hashtbl.length h in
    mk_generic ~cardinal ~mem ~iter
end


module MakeSet(S : Set.S) = struct
  type elt = S.elt

  let of_set set =
    let mem x = S.mem x set in
    let iter k = S.iter k set in
    let cardinal () = S.cardinal set in
    mk_generic ~cardinal ~mem ~iter

  let of_seq ?(init=S.empty) seq =
    let set = ref init in
    seq (fun x -> set := S.add x !set);
    of_set !set

  let to_set set =
    fold set S.empty (fun set x -> S.add x set)
end
