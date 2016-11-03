
(* This file is free software, part of containers. See file "license" for more details. *)

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

  val get_safe : 'a t -> int -> 'a option
  (** [get_safe a i] returns [Some a.(i)] if [i] is a valid index
      @since 0.18 *)

  val set : 'a t -> int -> 'a -> unit

  val length : _ t -> int

  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Fold left on array, with index *)

  val fold_while : ('a -> 'b -> 'a * [`Stop | `Continue]) -> 'a -> 'b t -> 'a
  (** Fold left on array until a stop condition via [('a, `Stop)] is
      indicated by the accumulator
      @since 0.8 *)

  val iter : ('a -> unit) -> 'a t -> unit

  val iteri : (int -> 'a -> unit) -> 'a t -> unit

  val blit : 'a t -> int -> 'a t -> int -> int -> unit
  (** [blit from i into j len] copies [len] elements from the first array
      to the second. See {!Array.blit}. *)

  val reverse_in_place : 'a t -> unit
  (** Reverse the array in place *)

  val sorted : ('a -> 'a -> int) -> 'a t -> 'a array
  (** [sorted cmp a] makes a copy of [a] and sorts it with [cmp].
      @since 0.21 *)

  val sort_indices : ('a -> 'a -> int) -> 'a t -> int array
  (** [sort_indices cmp a] returns a new array [b], with the same length as [a],
      such that [b.(i)] is the index of the [i]-th element of [a] in [sort cmp a].
      In other words, [map (fun i -> a.(i)) (sort_indices a) = sorted cmp a].
      [a] is not modified.
      @since 0.21 *)

  val sort_ranking : ('a -> 'a -> int) -> 'a t -> int array
  (** [sort_ranking cmp a] returns a new array [b], with the same length as [a],
      such that [b.(i)] is the position in [sorted cmp a] of the [i]-th
      element of [a].
      [a] is not modified.

      In other words, [map (fun i -> (sorted cmp a).(i)) (sort_ranking cmp a) = a].

      Without duplicates, we also have
      [lookup_exn a.(i) (sorted a) = (sorted_ranking a).(i)]
      @since 0.21 *)

  val find : ('a -> 'b option) -> 'a t -> 'b option
  (** [find f a] returns [Some y] if there is an element [x] such
      that [f x = Some y], else it returns [None] *)

  val findi : (int -> 'a -> 'b option) -> 'a t -> 'b option
  (** Like {!find}, but also pass the index to the predicate function.
      @since 0.3.4 *)

  val find_idx : ('a -> bool) -> 'a t -> (int * 'a) option
  (** [find_idx p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
      and [p x] holds. Otherwise returns [None]
      @since 0.3.4 *)

  val lookup : ?cmp:'a ord -> 'a -> 'a t -> int option
  (** Lookup the index of some value in a sorted array.
      @return [None] if the key is not present, or
        [Some i] ([i] the index of the key) otherwise *)

  val lookup_exn : ?cmp:'a ord -> 'a -> 'a t -> int
  (** Same as {!lookup_exn}, but
      @raise Not_found if the key is not present *)

  val bsearch : ?cmp:('a -> 'a -> int) -> 'a -> 'a t ->
    [ `All_lower | `All_bigger | `Just_after of int | `Empty | `At of int ]
  (** [bsearch ?cmp x arr] finds the index of the object [x] in the array [arr],
      provided [arr] is {b sorted} using [cmp]. If the array is not sorted,
      the result is not specified (may raise Invalid_argument).

      Complexity: O(log n) where n is the length of the array
      (dichotomic search).

      @return
      - [`At i] if [cmp arr.(i) x = 0] (for some i)
      - [`All_lower] if all elements of [arr] are lower than [x]
      - [`All_bigger] if all elements of [arr] are bigger than [x]
      - [`Just_after i] if [arr.(i) < x < arr.(i+1)]
      - [`Empty] if the array is empty

      @raise Invalid_argument if the array is found to be unsorted w.r.t [cmp]
      @since 0.13 *)

  val for_all : ('a -> bool) -> 'a t -> bool

  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** Forall on pairs of arrays.
      @raise Invalid_argument if they have distinct lengths
      allow different types @since 0.20 *)

  val exists : ('a -> bool) -> 'a t -> bool

  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** Exists on pairs of arrays.
      @raise Invalid_argument if they have distinct lengths
      allow different types @since 0.20 *)

  val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
  (** Fold on two arrays stepwise.
      @raise Invalid_argument if they have distinct lengths
      @since 0.20 *)

  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  (** Iterate on two arrays stepwise.
      @raise Invalid_argument if they have distinct lengths
      @since 0.20 *)

  val shuffle : 'a t -> unit
  (** Shuffle randomly the array, in place *)

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
  (** Print an array of items with printing function *)

  val pp_i: ?sep:string -> (Buffer.t -> int -> 'a -> unit) ->
            Buffer.t -> 'a t -> unit
  (** Print an array, giving the printing function both index and item *)

  val print : ?sep:string -> (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a t -> unit
  (** Print an array of items with printing function *)
end

(** {2 General Implementation}
Most of those functions use a range [(i,j)] with
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
  if i1 = j1
  then (assert (i1=j1 && i2=j2); true)
  else
    eq a1.(i1) a2.(i2) && _equal eq a1 (i1+1) j1 a2 (i2+1) j2

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
  else match f i a.(i) with
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

let bsearch_ ~cmp x arr i j =
  let rec aux i j =
    if i > j
      then `Just_after j
      else
        let middle = i + (j - i) / 2 in (* avoid overflow *)
        match cmp x arr.(middle) with
        | 0 -> `At middle
        | n when n<0 -> aux i (middle - 1)
        | _ -> aux (middle + 1) j
  in
  if i>=j then `Empty
  else match cmp arr.(i) x, cmp arr.(j) x with
  | n, _ when n>0 -> `All_bigger
  | _, n when n<0 -> `All_lower
  | _ -> aux i j

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

let _sort_indices cmp a i j =
  let len = j-i in
  let b = Array.init len (fun k->k) in
  Array.sort (fun k1 k2 -> cmp a.(k1+i) a.(k2+i)) b;
  b

let _sorted cmp a i j =
  let len = j-i in
  let b = Array.sub a i len in
  Array.sort cmp b;
  b

let _choose a i j st =
  if i>=j then raise Not_found;
  a.(i+Random.State.int st (j-i))

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

let map2 f a b =
  if Array.length a <> Array.length b then invalid_arg "map2";
  Array.init (Array.length a) (fun i -> f (Array.unsafe_get a i) (Array.unsafe_get b i))

let length = Array.length

let get = Array.get

let get_safe a i =
  if i>=0 && i<Array.length a
  then Some (Array.unsafe_get a i)
  else None

(*$=
  (Some 1) (get_safe [|1;2;3|] 0)
  (Some 2) (get_safe [|1;2;3|] 1)
  (Some 3) (get_safe [|1;2;3|] 2)
  None (get_safe [|1;2;3|] 4)
  None (get_safe [|1;2;3|] max_int)
  None (get_safe [|1;2;3|] ~-1)
  None (get_safe [|1;2;3|] ~-42)
*)

let set = Array.set

let fold = Array.fold_left

let foldi f acc a = _foldi f acc a 0 (Array.length a)

let fold_while f acc a =
  let rec fold_while_i f acc i =
    if i < Array.length a then
      let acc, cont = f acc a.(i) in
      match cont with
      | `Stop -> acc
      | `Continue -> fold_while_i f acc (i+1)
    else acc
  in fold_while_i f acc 0

(*$T
  fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0 (Array.of_list [true;true;false;true]) = 2
*)

let iter = Array.iter

let iteri = Array.iteri

let blit = Array.blit

let reverse_in_place a =
  _reverse_in_place a 0 ~len:(Array.length a)

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

let sorted cmp a = _sorted cmp a 0 (Array.length a)

(*$= & ~cmp:(=) ~printer:Q.Print.(array int)
  [||] (sorted Pervasives.compare [||])
  [|0;1;2;3;4|] (sorted Pervasives.compare [|3;2;1;4;0|])
  *)

(*$Q
  Q.(array int) (fun a -> \
    let b = Array.copy a in \
    Array.sort Pervasives.compare b; b = sorted Pervasives.compare a)
*)

let sort_indices cmp a = _sort_indices cmp a 0 (Array.length a)

(*$= & ~cmp:(=) ~printer:Q.Print.(array int)
  [||] (sort_indices Pervasives.compare [||])
  [|4;2;1;0;3|] (sort_indices Pervasives.compare [|"d";"c";"b";"e";"a"|])
*)

(*$Q
  Q.(array printable_string) (fun a -> \
    let b = sort_indices String.compare a in \
    sorted String.compare a = Array.map (Array.get a) b)
*)

let sort_ranking cmp a =
  let cmp_int : int -> int -> int = Pervasives.compare in
  sort_indices cmp_int (sort_indices cmp a)

(*$= & ~cmp:(=) ~printer:Q.Print.(array int)
  [||] (sort_ranking Pervasives.compare [||])
  [|3;2;1;4;0|] (sort_ranking Pervasives.compare [|"d";"c";"b";"e";"a"|])
*)

(*$Q
  Q.(array printable_string) (fun a -> \
    let b = sort_ranking String.compare a in \
    let a_sorted = sorted String.compare a in \
    a = Array.map (Array.get a_sorted) b)
*)

let rev a =
  let b = Array.copy a in
  reverse_in_place b;
  b

(*$Q
  Q.(array small_int) (fun a -> rev (rev a) = a)
*)

(*$T
  rev [| 1; 2; 3 |] = [| 3; 2; 1 |]
  rev [| 1; 2; |] = [| 2; 1 |]
  rev [| |] = [| |]
  *)

let find f a =
  _find (fun _ -> f ) a 0 (Array.length a)

let findi f a =
  _find f a 0 (Array.length a)

let find_idx p a =
  _find (fun i x -> if p x then Some (i,x) else None) a 0 (Array.length a)

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

let bsearch ?(cmp=Pervasives.compare) k a = bsearch_ ~cmp k a 0 (Array.length a-1)

(*$T bsearch
  bsearch 3 [|1; 2; 2; 3; 4; 10|] = `At 3
  bsearch 5 [|1; 2; 2; 3; 4; 10|] = `Just_after 4
  bsearch 1 [|1; 2; 5; 5; 11; 12|] = `At 0
  bsearch 12 [|1; 2; 5; 5; 11; 12|] = `At 5
  bsearch 10 [|1; 2; 2; 3; 4; 9|] = `All_lower
  bsearch 0 [|1; 2; 2; 3; 4; 9|] = `All_bigger
  bsearch 3 [| |] = `Empty
*)

let (>>=) a f = flat_map f a

let (>>|) a f = map f a

let (>|=) a f = map f a

let for_all p a = _for_all p a 0 (Array.length a)

let exists p a = _exists p a 0 (Array.length a)

let for_all2 p a b =
  Array.length a = Array.length b
  &&
  _for_all2 p a b 0 0 ~len:(Array.length a)

let exists2 p a b =
  _exists2 p a b 0 0 ~len:(min (Array.length a) (Array.length b))

let _iter2 f a b i j ~len =
  for o = 0 to len-1 do
    f (Array.get a (i+o)) (Array.get b (j+o))
  done

let _fold2 f acc a b i j ~len =
  let rec aux acc o =
    if o=len then acc
    else
      let acc = f acc (Array.get a (i+o)) (Array.get b (j+o)) in
      aux acc (o+1)
  in
  aux acc 0

let iter2 f a b =
  if length a <> length b then invalid_arg "iter2";
  _iter2 f a b 0 0 ~len:(Array.length a)

let fold2 f acc a b =
  if length a <> length b then invalid_arg "fold2";
  _fold2 f acc a b 0 0 ~len:(Array.length a)

let (--) i j =
  if i<=j
  then
    Array.init (j-i+1) (fun k -> i+k)
  else
    Array.init (i-j+1) (fun k -> i-k)

(*$T
  (1 -- 4) |> Array.to_list = [1;2;3;4]
  (4 -- 1) |> Array.to_list = [4;3;2;1]
  (0 -- 0) |> Array.to_list = [0]
*)

(*$Q
  Q.(pair small_int small_int) (fun (a,b) -> \
    (a -- b) |> Array.to_list = CCList.(a -- b))
*)

let (--^) i j =
  if i=j then [| |]
  else if i>j
    then Array.init (i-j) (fun k -> i-k)
    else Array.init (j-i) (fun k -> i+k)

(*$Q
  Q.(pair small_int small_int) (fun (a,b) -> \
    (a --^ b) |> Array.to_list = CCList.(a --^ b))
*)

(** all the elements of a, but the i-th, into a list *)
let except_idx a i =
  foldi
    (fun acc j elt -> if i = j then acc else elt::acc)
    [] a

let equal eq a b =
  Array.length a = Array.length b
  &&
  _equal eq a 0 (Array.length a) b 0 (Array.length b)

(*$Q
  Q.(pair (array small_int)(array small_int)) (fun (a,b) -> \
    equal (=) a b = equal (=) b a)
*)

(*$T
  equal (=) [|1|] [|1|]
*)

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

  let sub a i len = make a.arr (a.i + i) ~len
  (*$=
    [ 3;4 ] \
      (let a = Sub.make (0--10) 2 5 in Sub.sub a 1 2 |> Sub.to_list)
    [ ] \
      (let a = Sub.make (0--10) 2 5 in Sub.sub a 1 0 |> Sub.to_list)
    [ 5 ] \
      (let a = Sub.make (0--10) 1 9 in Sub.sub a 4 1 |> Sub.to_list)
  *)

  let equal eq a b =
    length a = length b && _equal eq a.arr a.i a.j b.arr b.i b.j

  let compare cmp a b =
    _compare cmp a.arr a.i a.j b.arr b.i b.j

  let fold f acc a =
    let rec _fold acc i j =
      if i=j then acc
      else _fold (f acc a.arr.(i)) (i+1) j
    in _fold acc a.i a.j

  let to_list a =
    let l = fold (fun l x -> x::l) [] a in
    List.rev l

  let foldi f acc a = _foldi f acc a.arr a.i a.j

  let fold_while f acc a =
    let rec fold_while_i f acc i =
      if i < Array.length a.arr && i < a.j then
        let acc, cont = f acc a.arr.(i) in
        match cont with
        | `Stop -> acc
        | `Continue -> fold_while_i f acc (i+1)
      else acc
    in fold_while_i f acc a.i

  let get a i =
    let j = a.i + i in
    if i<0 || j>=a.j then invalid_arg "Array.Sub.get";
    a.arr.(j)

  let get_safe a i =
    try Some (get a i)
    with Invalid_argument _ -> None

  (*$inject
    let sub_a = Sub.make [|1;2;3;4;5|] 1 ~len:3
  *)

  (*$=
    (Some 2) (Sub.get_safe sub_a 0)
    (Some 3) (Sub.get_safe sub_a 1)
    (Some 4) (Sub.get_safe sub_a 2)
    None (Sub.get_safe sub_a 4)
    None (Sub.get_safe sub_a max_int)
    None (Sub.get_safe sub_a ~-1)
    None (Sub.get_safe sub_a ~-42)
  *)

  let set a i x =
    let j = a.i + i in
    if i<0 || j>=a.j then invalid_arg "Array.Sub.set";
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

  let sorted cmp a = _sorted cmp a.arr a.i a.j

  (*$= & ~cmp:(=) ~printer:Q.Print.(array int)
    [||] \
      (let a = 1--6 in let s = Sub.make a 2 ~len:0 in \
       Sub.sorted Pervasives.compare s)
    [|2;3;4|] \
      (let a = [|6;5;4;3;2;1|] in let s = Sub.make a 2 ~len:3 in \
      Sub.sorted Pervasives.compare s)
  *)

  (*$Q
    Q.(array int) (fun a -> \
    Array.length a > 10 ==> ( Array.length a > 10 && \
    let s = Sub.make a 5 ~len:5 in \
    let b = Array.sub a 5 5 in \
    Array.sort Pervasives.compare b; b = Sub.sorted Pervasives.compare s))
  *)

  let sort_ranking cmp a =
    let idx = _sort_indices cmp a.arr a.i a.j in
    let cmp_int : int -> int -> int = Pervasives.compare in
    sort_indices cmp_int idx

  (*$= & ~cmp:(=) ~printer:Q.Print.(array int)
    [||] \
       (let a = 1--6 in let s = Sub.make a 2 ~len:0 in \
       Sub.sort_ranking Pervasives.compare s)
    [|2;1;3;0|] \
      (let a = [|"d";"c";"b";"e";"a"|] in let s = Sub.make a 1 ~len:4 in \
      Sub.sort_ranking Pervasives.compare s)
  *)

  (*$Q
    Q.(array printable_string) (fun a -> \
    Array.length a > 10 ==> ( Array.length a > 10 && \
    let s = Sub.make a 5 ~len:5 in \
    let b = Sub.sort_indices String.compare s in \
    Sub.sorted String.compare s = Array.map (Sub.get s) b))
  *)

  let sort_indices cmp a = _sort_indices cmp a.arr a.i a.j

  (*$= & ~cmp:(=) ~printer:Q.Print.(array int)
    [||] \
       (let a = 1--6 in let s = Sub.make a 2 ~len:0 in \
       Sub.sort_indices Pervasives.compare s)
    [|3;1;0;2|] \
      (let a = [|"d";"c";"b";"e";"a"|] in let s = Sub.make a 1 ~len:4 in \
      Sub.sort_indices Pervasives.compare s)
  *)

  (*$Q
    Q.(array printable_string) (fun a -> \
    Array.length a > 10 ==> ( Array.length a > 10 && \
    let s = Sub.make a 5 ~len:5 in \
    let b = Sub.sort_ranking String.compare s in \
    let a_sorted = Sub.sorted String.compare s in \
    Sub.copy s = Array.map (Array.get a_sorted) b))
  *)


  let find f a = _find (fun _ -> f) a.arr a.i a.j

  let findi f a = _find (fun i -> f (i-a.i)) a.arr a.i a.j

  let find_idx p a =
    _find (fun i x -> if p x then Some (i-a.i,x) else None) a.arr a.i a.j

  (*$=
    (Some (1,"c")) (Sub.find_idx ((=) "c") (Sub.make [| "a"; "b"; "c" |] 1 2))
    *)

  let lookup_exn ?(cmp=Pervasives.compare) k a =
    _lookup_exn ~cmp k a.arr a.i (a.j-1) - a.i

  let lookup ?(cmp=Pervasives.compare) k a =
    try Some (_lookup_exn ~cmp k a.arr a.i (a.j-1) - a.i)
    with Not_found -> None

  (*$=
    (Some 1) (Sub.lookup "c" (Sub.make [| "a"; "b"; "c" |] 1 2))
    *)

  let bsearch ?(cmp=Pervasives.compare) k a =
    match bsearch_ ~cmp k a.arr a.i (a.j - 1) with
      | `At m -> `At (m - a.i)
      | `Just_after m -> `Just_after (m - a.i)
      | res -> res

  let for_all p a = _for_all p a.arr a.i a.j

  let exists p a = _exists p a.arr a.i a.j

  let for_all2 p a b =
    length a = length b && _for_all2 p a.arr b.arr a.i b.i ~len:(length a)

  let exists2 p a b =
    _exists2 p a.arr b.arr a.i b.i ~len:(min (length a) (length b))

  (*$T
  Sub.exists2 (=) (Sub.make [| 1;2;3;4 |] 1 ~len:2) (Sub.make [| 0;1;3;4 |] 1 ~len:3)
  *)

  let iter2 f a b =
    if length a <> length b then invalid_arg "iter2";
    _iter2 f a.arr b.arr a.i b.i ~len:(length a)

  let fold2 f acc a b =
    if length a <> length b then invalid_arg "fold2";
    _fold2 f acc a.arr b.arr a.i b.i ~len:(length a)

  let shuffle a =
    _shuffle Random.int a.arr a.i a.j

  let shuffle_with st a =
    _shuffle (Random.State.int st) a.arr a.i a.j

  let random_choose a st = _choose a.arr a.i a.j st

  let pp ?(sep=", ") pp_item buf a = _pp ~sep pp_item buf a.arr a.i a.j

  let pp_i ?(sep=", ") pp_item buf a =
    _pp_i ~sep (fun out k x -> pp_item out (k-a.i) x) buf a.arr a.i a.j

  let print ?(sep=", ") pp_item fmt a = _print ~sep pp_item fmt a.arr a.i a.j

  let to_seq a k = iter k a

  let to_gen a = _to_gen a.arr a.i a.j

  let to_klist a = _to_klist a.arr a.i a.j
end

(** {2 Generic Functions} *)

module type MONO_ARRAY = sig
  type elt
  type t

  val length : t -> int

  val get : t -> int -> elt

  val set : t -> int -> elt -> unit
end

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


let sort_generic (type arr)(type elt)
(module A : MONO_ARRAY with type t = arr and type elt = elt)
?(cmp=Pervasives.compare) a
=
  let module S = SortGeneric(A) in
  S.sort ~cmp a

(*$inject
  module IA = struct
    type elt = int
    type t = int array
    include Array
  end

  let gen_arr = Q.Gen.(array_size (1--100) small_int)
  let arr_arbitrary = Q.make
    ~print:Q.Print.(array int)
    ~small:Array.length
    ~shrink:Q.Shrink.(array ?shrink:None)
    gen_arr
*)

(*$Q & ~count:300
  arr_arbitrary (fun a -> \
    let a1 = Array.copy a and a2 = Array.copy a in \
    Array.sort CCInt.compare a1; sort_generic ~cmp:CCInt.compare (module IA) a2; \
    a1 = a2 )
*)
