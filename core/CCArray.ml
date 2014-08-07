(*
copyright (c) 2013-2014, simon cruanes
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

(** {1 Array utils} *)

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a random_gen = Random.State.t -> 'a

module type S = sig
  type 'a t
  (** Array, or sub-array, containing elements of type ['a] *)

  val empty : 'a t

  val equal : 'a equal -> 'a t equal

  val compare : 'a ord -> 'a t ord

  val get : 'a t -> int -> 'a

  val set : 'a t -> int -> 'a -> unit

  val length : _ t -> int

  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val foldi : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** fold left on array, with index *)

  val iter : ('a -> unit) -> 'a t -> unit

  val iteri : (int -> 'a -> unit) -> 'a t -> unit

  val blit : 'a t -> int -> 'a t -> int -> int -> unit
  (** [blit from i into j len] copies [len] elements from the first array
      to the second. See {!Array.blit}. *)

  val reverse_in_place : 'a t -> unit
  (** Reverse the array in place *)

  val find : ('a -> 'b option) -> 'a t -> 'b option
  (** [find f a] returns [Some y] if there is an element [x] such
      that [f x = Some y], else it returns [None] *)

  val lookup : ?cmp:'a ord -> 'a -> 'a t -> int option
  (** Lookup the index of some value in a sorted array.
      @return [None] if the key is not present, or
        [Some i] ([i] the index of the key) otherwise *)

  val lookup_exn : ?cmp:'a ord -> 'a -> 'a t -> int
  (** Same as {!lookup_exn}, but
      @raise Not_found if the key is not present *)

  val for_all : ('a -> bool) -> 'a t -> bool

  val for_all2 : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** Forall on pairs of arrays.
      @raise Invalid_argument if they have distinct lengths *)

  val exists : ('a -> bool) -> 'a t -> bool

  val exists2 : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** Exists on pairs of arrays.
      @raise Invalid_argument if they have distinct lengths *)

  val shuffle : 'a t -> unit
  (** shuffle randomly the array, in place *)

  val shuffle_with : Random.State.t -> 'a t -> unit
  (** Like shuffle but using a specialized random state *)

  val random_choose : 'a t -> 'a random_gen
  (** Choose an element randomly.
      @raise Not_found if the array/slice is empty *)

  val to_seq : 'a t -> 'a sequence
  val to_gen : 'a t -> 'a gen
  val to_klist : 'a t -> 'a klist

  (** {2 IO} *)

  val pp: ?sep:string -> (Buffer.t -> 'a -> unit) ->
          Buffer.t -> 'a t -> unit
  (** print an array of items with printing function *)

  val pp_i: ?sep:string -> (Buffer.t -> int -> 'a -> unit) ->
            Buffer.t -> 'a t -> unit
  (** print an array, giving the printing function both index and item *)

  val print : ?sep:string -> (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a t -> unit
  (** print an array of items with printing function *)
end

(** {2 General Implementation}
Most of those functions that a range [(i,j)] with
[i] included and [j] excluded *)

let rec _foldi f acc a i j =
  if i = j then acc else _foldi f (f acc i a.(i)) a (i+1) j

let _reverse_in_place a i ~len =
  if len=0 then ()
  else
    for k = 0 to (len-1)/2 do
      let t = a.(i+k) in
      a.(i+k) <- a.(i+len-1-k);
      a.(i+len-1-k) <- t;
    done

let rec _equal eq a1 i1 j1 a2 i2 j2 =
  if i1 = j1 || i2 = j2
  then (assert (i1=j1 && i2=j2); true)
  else
    eq a1.(i1) a2.(i2) && _equal eq a1 (i1+1) j1 a2 (i2+2) j2

let rec _compare cmp a1 i1 j1 a2 i2 j2 =
  if i1 = j1
  then if i2=j2 then 0 else -1
  else if i2=j2
    then 1
    else
      let c = cmp a1.(i1) a2.(i2) in
      if c = 0
        then _compare cmp a1 (i1+1) j1 a2 (i2+1) j2
        else c

(*$T
  compare CCOrd.compare [| 1; 2; 3 |] [| 1; 2; 3 |] = 0
  compare CCOrd.compare [| 1; 2; 3 |] [| 2; 2; 3 |] < 0
  compare CCOrd.compare [| 1; 2; |] [| 1; 2; 3 |] < 0
  compare CCOrd.compare [| 1; 2; 3 |] [| 1; 2; |] > 0
*)

let rec _find f a i j =
  if i = j then None
  else match f a.(i) with
    | Some _ as res -> res
    | None -> _find f a (i+1) j

let rec _lookup_rec ~cmp k a i j =
  if i>j then raise Not_found
  else if i=j
    then if cmp k a.(i) = 0
      then i
      else raise Not_found
  else
    let middle = (j+i)/2 in
    match cmp k a.(middle) with
    | 0 -> middle
    | n when n<0 -> _lookup_rec ~cmp k a i (middle-1)
    | _ -> _lookup_rec ~cmp k a (middle+1) j

let _lookup_exn ~cmp k a i j =
  if i>j then raise Not_found;
  match cmp k a.(i) with
  | 0 -> i
  | n when n<0 -> raise Not_found (* too low *)
  | _ when i=j -> raise Not_found (* too high *)
  | _ ->
      match cmp k a.(j) with
      | 0 -> j
      | n when n<0 -> _lookup_rec ~cmp k a (i+1) (j-1)
      | _ -> raise Not_found  (* too high *)

let rec _for_all p a i j =
  i = j || (p a.(i) && _for_all p a (i+1) j)

let rec _exists p a i j =
  i <> j && (p a.(i) || _exists p a (i+1) j)

let rec _for_all2 p a1 a2 i1 i2 ~len =
  len=0 || (p a1.(i1) a2.(i2) && _for_all2 p a1 a2 (i1+1) (i2+1) ~len:(len-1))

let rec _exists2 p a1 a2 i1 i2 ~len =
  len>0 && (p a1.(i1) a2.(i2) || _exists2 p a1 a2 (i1+1) (i2+1) ~len:(len-1))

(* shuffle a[i...j[ using the given int random generator
   See http://en.wikipedia.org/wiki/Fisher-Yates_shuffle *)
let _shuffle _rand_int a i j = 
  for k = j-1 downto i+1 do
    let l = _rand_int (k+1) in
    let tmp = a.(l) in
    a.(l) <- a.(k);
    a.(k) <- tmp;
  done

(*$T
  let st = Random.State.make [||] in let a = 0--10000 in \
  let b = Array.copy a in shuffle_with st a; a <> b
*)

let _choose a i j st =
  if i>=j then raise Not_found;
  a.(i+Random.int (j-i))

let _pp ~sep pp_item buf a i j =
  for k = i to j - 1 do
    if k > i then Buffer.add_string buf sep;
    pp_item buf a.(k)
  done

let _pp_i ~sep pp_item buf a i j =
  for k = i to j - 1 do
    if k > i then Buffer.add_string buf sep;
    pp_item buf k a.(k)
  done

let _print ~sep pp_item fmt a i j =
  for k = i to j - 1 do
    if k > i then (Format.pp_print_string fmt sep; Format.pp_print_cut fmt ());
    pp_item fmt a.(k)
  done

let _to_gen a i j =
  let k = ref i in
  fun () ->
    if !k < j
    then (
      let x = a.(!k) in
      incr k;
      Some x
    ) else None

let rec _to_klist a i j () =
  if i=j then `Nil else `Cons (a.(i), _to_klist a (i+1) j)

(** {2 Arrays} *)

type 'a t = 'a array

let empty = [| |]

let map = Array.map

let length = Array.length

let get = Array.get

let set = Array.set

let fold = Array.fold_left

let foldi f acc a = _foldi f acc a 0 (Array.length a)

let iter = Array.iter

let iteri = Array.iteri

let blit = Array.blit

let reverse_in_place a =
  _reverse_in_place a 0 (Array.length a)

(*$T
  reverse_in_place [| |]; true
  reverse_in_place [| 1 |]; true
  let a = [| 1; 2; 3; 4; 5 |] in \
    reverse_in_place a; \
    a = [| 5;4;3;2;1 |]
  let a = [| 1; 2; 3; 4; 5; 6 |] in \
    reverse_in_place a; \
    a = [| 6;5;4;3;2;1 |]
*)

let find f a =
  _find f a 0 (Array.length a)

let filter_map f a =
  let rec aux acc i =
    if i = Array.length a
    then (
      let a' = Array.of_list acc in
      reverse_in_place a';
      a'
    ) else match f a.(i) with
      | None -> aux acc (i+1)
      | Some x -> aux (x::acc) (i+1)
  in aux [] 0

(*$T
  filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
    [| 1; 2; 3; 4 |] = [| "2"; "4" |]
  filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
    [| 1; 2; 3; 4; 5; 6 |] \
    = [| "2"; "4"; "6" |]
*)

let filter p a =
  filter_map (fun x -> if p x then Some x else None) a

(* append [rev a] in front of [acc] *)
let rec __rev_append_list a acc i =
  if i = Array.length a
  then acc
  else
    __rev_append_list a (a.(i) :: acc) (i+1)

let flat_map f a =
  let rec aux acc i =
    if i = Array.length a
    then (
      let a' = Array.of_list acc in
      reverse_in_place a';
      a'
    )
    else
      let a' = f a.(i) in
      aux (__rev_append_list a' acc 0) (i+1)
  in aux [] 0

(*$T
  let a = [| 1; 3; 5 |] in \
  let a' = flat_map (fun x -> [| x; x+1 |]) a in \
  a' = [| 1; 2; 3; 4; 5; 6 |]
*)

let lookup_exn ?(cmp=Pervasives.compare) k a =
  _lookup_exn ~cmp k a 0 (Array.length a-1)

let lookup ?(cmp=Pervasives.compare) k a =
  try Some (_lookup_exn ~cmp k a 0 (Array.length a-1))
  with Not_found -> None

(*$T
  lookup 2 [|0;1;2;3;4;5|] = Some 2
  lookup 4 [|0;1;2;3;4;5|] = Some 4
  lookup 0 [|1;2;3;4;5|] = None
  lookup 6 [|1;2;3;4;5|] = None
  lookup 3 [| |] = None
  lookup 1 [| 1 |] = Some 0
  lookup 2 [| 1 |] = None
*)

let (>>=) a f = flat_map f a

let for_all p a = _for_all p a 0 (Array.length a)

let exists p a = _exists p a 0 (Array.length a)

let for_all2 p a b =
  Array.length a = Array.length b
  &&
  _for_all2 p a b 0 0 ~len:(Array.length a)

let exists2 p a b =
  _exists2 p a b 0 0 ~len:(min (Array.length a) (Array.length b))

let (--) i j =
  if i<=j
  then
    Array.init (j-i+1) (fun k -> i+k)
  else
    Array.init (i-j+1) (fun k -> i-k)

(** all the elements of a, but the i-th, into a list *)
let except_idx a i =
  foldi
    (fun acc j elt -> if i = j then acc else elt::acc)
    [] a

let equal eq a b =
  Array.length a = Array.length b
  &&
  _equal eq a 0 (Array.length a) b 0 (Array.length b)

let compare cmp a b =
  _compare cmp a 0 (Array.length a) b 0 (Array.length b)

let shuffle a = _shuffle Random.int a 0 (Array.length a)

let shuffle_with st a = _shuffle (Random.State.int st) a 0 (Array.length a)

let random_choose a st = _choose a 0 (Array.length a) st

let random_len n g st =
  Array.init n (fun _ -> g st)

let random g st =
  let n = Random.State.int st 1_000 in
  random_len n g st

let random_non_empty g st =
  let n = 1 + Random.State.int st 1_000 in
  random_len n g st

let pp ?(sep=", ") pp_item buf a = _pp ~sep pp_item buf a 0 (Array.length a)

let pp_i ?(sep=", ") pp_item buf a = _pp_i ~sep pp_item buf a 0 (Array.length a)

let print ?(sep=", ") pp_item fmt a = _print ~sep pp_item fmt a 0 (Array.length a)

let to_seq a k = iter k a

let to_gen a = _to_gen a 0 (Array.length a)

let to_klist a = _to_klist a 0 (Array.length a)

module Sub = struct
  type 'a t = {
    arr : 'a array;
    i : int; (** Start index (included) *)
    j : int;  (** Stop index (excluded) *)
  }

  let empty = {
    arr = [||];
    i = 0;
    j = 0;
  }

  let make arr i ~len =
    if i<0||i+len > Array.length arr then invalid_arg "Array.Sub.make";
    { arr; i; j=i+len; }

  let of_slice (arr,i,len) = make arr i ~len

  let to_slice a = a.arr, a.i, a.j-a.i

  let full arr = { arr; i=0; j=Array.length arr; }

  let underlying a = a.arr

  let length a = a.j - a.i

  let copy a = Array.sub a.arr a.i (length a)

  let sub a i len = make a.arr (a.i + i) len

  let equal eq a b =
    length a = length b && _equal eq a.arr a.i a.j b.arr b.i b.j

  let compare cmp a b =
    _compare cmp a.arr a.i a.j b.arr b.i b.j

  let fold f acc a =
    let rec _fold acc i j =
      if i=j then acc
      else _fold (f acc a.arr.(i)) (i+1) j
    in _fold acc a.i a.j

  let foldi f acc a = _foldi f acc a.arr a.i a.j

  let get a i =
    let j = a.i + i in
    if i<0 || j>=a.j then invalid_arg "Array.Sub.get";
    a.arr.(j)

  let set a i x =
    let j = a.i + i in
    if i<0 || j>=a.j then invalid_arg "Array.Sub.get";
    a.arr.(j) <- x

  let iter f a =
    for k=a.i to a.j-1 do f a.arr.(k) done

  let iteri f a =
    for k=0 to length a-1 do f k a.arr.(a.i + k) done

  let blit a i b j len =
    if i+len>length a || j+len>length b then invalid_arg "Array.Sub.blit";
    Array.blit a.arr (a.i+i) b.arr (b.i+j) len

  let reverse_in_place a = _reverse_in_place a.arr a.i ~len:(length a)

  (*$T
  let a = 1--6 in let s = Sub.make a 2 ~len:3 in \
    Sub.reverse_in_place s; a = [| 1; 2; 5; 4; 3; 6 |]
  *)

  let find f a = _find f a.arr a.i a.j

  let lookup_exn ?(cmp=Pervasives.compare) k a =
    _lookup_exn ~cmp k a.arr a.i (a.j-1)

  let lookup ?(cmp=Pervasives.compare) k a =
    try Some (_lookup_exn ~cmp k a.arr a.i (a.j-1))
    with Not_found -> None

  let for_all p a = _for_all p a.arr a.i a.j

  let exists p a = _exists p a.arr a.i a.j

  let for_all2 p a b =
    length a = length b && _for_all2 p a.arr b.arr a.i b.i ~len:(length a)

  let exists2 p a b =
    _exists2 p a.arr b.arr a.i b.i ~len:(min (length a) (length b))

  (*$T
  Sub.exists2 (=) (Sub.make [| 1;2;3;4 |] 1 ~len:2) (Sub.make [| 0;1;3;4 |] 1 ~len:3)
  *)

  let shuffle a =
    _shuffle Random.int a.arr a.i a.j

  let shuffle_with st a =
    _shuffle (Random.State.int st) a.arr a.i a.j

  let random_choose a st = _choose a.arr a.i a.j st

  let pp ?(sep=", ") pp_item buf a = _pp ~sep pp_item buf a.arr a.i a.j

  let pp_i ?(sep=", ") pp_item buf a = _pp_i ~sep pp_item buf a.arr a.i a.j

  let print ?(sep=", ") pp_item fmt a = _print ~sep pp_item fmt a.arr a.i a.j

  let to_seq a k = iter k a

  let to_gen a = _to_gen a.arr a.i a.j

  let to_klist a = _to_klist a.arr a.i a.j
end
