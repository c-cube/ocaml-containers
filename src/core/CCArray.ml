(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Array utils} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a random_gen = Random.State.t -> 'a
type 'a printer = Format.formatter -> 'a -> unit

(*$T
  let st = Random.State.make [||] in let a = 0--10000 in \
  let b = Array.copy a in shuffle_with st a; a <> b
*)

(** {2 Arrays} *)

include CCShims_
include CCShimsArray_

let empty = [| |]

let map = Array.map

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

let foldi f acc a =
  let rec aux acc i =
    if i = Array.length a then acc else aux (f acc i a.(i)) (i+1)
  in
  aux acc 0

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

let fold_map f acc a =
  let n = length a in
  (* need special case for initializing the result *)
  if n = 0 then acc, [||]
  else (
    let acc, b0 = f acc a.(0) in
    let res = Array.make n b0 in
    let acc = ref acc in
    for i = 1 to n-1 do
      let new_acc, b = f !acc a.(i) in
      acc := new_acc;
      res.(i) <- b;
    done;
    !acc, res
  )

(*$=
  (6, [|"1"; "2"; "3"|]) \
    (fold_map (fun acc x->acc+x, string_of_int x) 0 [|1;2;3|])
*)

(*$Q
  Q.(array int) (fun a -> \
    fold_map (fun acc x -> x::acc, x) [] a = (List.rev @@ Array.to_list a, a))
*)

let scan_left f acc a =
  let n = length a in
  let res = Array.make (n+1) acc in
  Array.iteri
    (fun i x ->
       let new_acc = f res.(i) x in
       res.(i+1) <- new_acc)
    a;
  res

(*$= & ~printer:Q.Print.(array int)
  [|0;1;3;6|] (scan_left (+) 0 [|1;2;3|])
  [|0|] (scan_left (+) 0 [||])
*)


let iter = Array.iter

let iteri = Array.iteri

let blit = Array.blit

let reverse_in_place a =
  let len = Array.length a in
  if len>0 then (
    for k = 0 to (len-1)/2 do
      let t = a.(k) in
      a.(k) <- a.(len-1-k);
      a.(len-1-k) <- t;
    done
  )

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

let sorted cmp a =
  let b = Array.copy a in
  Array.sort cmp b;
  b

(*$= & ~cmp:(=) ~printer:Q.Print.(array int)
  [||] (sorted Stdlib.compare [||])
  [|0;1;2;3;4|] (sorted Stdlib.compare [|3;2;1;4;0|])
*)

(*$Q
  Q.(array int) (fun a -> \
    let b = Array.copy a in \
    Array.sort Stdlib.compare b; b = sorted Stdlib.compare a)
*)

let sort_indices cmp a =
  let len = Array.length a in
  let b = Array.init len (fun k->k) in
  Array.sort (fun k1 k2 -> cmp a.(k1) a.(k2)) b;
  b

(*$= & ~cmp:(=) ~printer:Q.Print.(array int)
  [||] (sort_indices Stdlib.compare [||])
  [|4;2;1;0;3|] (sort_indices Stdlib.compare [|"d";"c";"b";"e";"a"|])
*)

(*$Q
  Q.(array_of_size Gen.(0 -- 30) printable_string) (fun a -> \
    let b = sort_indices String.compare a in \
    sorted String.compare a = Array.map (Array.get a) b)
*)

let sort_ranking cmp a =
  sort_indices compare (sort_indices cmp a)

(*$= & ~cmp:(=) ~printer:Q.Print.(array int)
  [||] (sort_ranking Stdlib.compare [||])
  [|3;2;1;4;0|] (sort_ranking Stdlib.compare [|"d";"c";"b";"e";"a"|])
*)

(*$Q
  Q.(array_of_size Gen.(0--50) printable_string) (fun a -> \
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

exception Found

let mem ?(eq = Stdlib.(=)) elt a =
  try
    Array.iter (fun e -> if eq e elt then raise_notrace Found) a;
    false
  with Found -> true

(*$Q mem
  Q.(array small_int) (fun a -> \
  mem 1 a = (Array.mem 1 a))
*)

let rec find_aux f a i =
  if i >= Array.length a then None
  else match f i a.(i) with
    | Some _ as res -> res
    | None -> find_aux f a (i+1)

let find_map f a = find_aux (fun _ -> f ) a 0

let find = find_map

let find_map_i f a = find_aux f a 0

let findi = find_map_i

let find_idx p a =
  find_aux (fun i x -> if p x then Some (i,x) else None) a 0

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

let monoid_product f a1 a2 =
  let na1 = length a1 in
  init (na1 * length a2)
    (fun i_prod ->
       let i = i_prod mod na1 in
       let j = i_prod / na1 in
       f a1.(i) a2.(j))

(*$= & ~cmp:(=) ~printer:Q.Print.(array int)
  [| 11; 12; 21; 22 |] (sorted CCInt.compare @@ monoid_product (+) [| 10; 20 |] [| 1; 2 |])
  [| 11; 12; 13; 14 |] (sorted CCInt.compare @@ monoid_product (+) [| 10 |] [| 1; 2; 3; 4 |])
*)

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

let lookup_exn ~cmp k a =
  _lookup_exn ~cmp k a 0 (Array.length a-1)

let lookup ~cmp k a =
  try Some (_lookup_exn ~cmp k a 0 (Array.length a-1))
  with Not_found -> None

(*$T
  lookup ~cmp:CCInt.compare 2 [|0;1;2;3;4;5|] = Some 2
  lookup ~cmp:CCInt.compare 4 [|0;1;2;3;4;5|] = Some 4
  lookup ~cmp:CCInt.compare 0 [|1;2;3;4;5|] = None
  lookup ~cmp:CCInt.compare 6 [|1;2;3;4;5|] = None
  lookup ~cmp:CCInt.compare 3 [| |] = None
  lookup ~cmp:CCInt.compare 1 [| 1 |] = Some 0
  lookup ~cmp:CCInt.compare 2 [| 1 |] = None
*)

let bsearch ~cmp k a =
  let rec aux i j =
    if i > j
    then `Just_after j
    else
      let middle = i + (j - i) / 2 in (* avoid overflow *)
      match cmp k a.(middle) with
        | 0 -> `At middle
        | n when n<0 -> aux i (middle - 1)
        | _ -> aux (middle + 1) j
  in
  let n = Array.length a in
  if n=0 then `Empty
  else match cmp a.(0) k, cmp a.(n-1) k with
    | c, _ when c>0 -> `All_bigger
    | _, c when c<0 -> `All_lower
    | _ -> aux 0 (n-1)

(*$T bsearch
  bsearch ~cmp:CCInt.compare 3 [|1; 2; 2; 3; 4; 10|] = `At 3
  bsearch ~cmp:CCInt.compare 5 [|1; 2; 2; 3; 4; 10|] = `Just_after 4
  bsearch ~cmp:CCInt.compare 1 [|1; 2; 5; 5; 11; 12|] = `At 0
  bsearch ~cmp:CCInt.compare 12 [|1; 2; 5; 5; 11; 12|] = `At 5
  bsearch ~cmp:CCInt.compare 10 [|1; 2; 2; 3; 4; 9|] = `All_lower
  bsearch ~cmp:CCInt.compare 0 [|1; 2; 2; 3; 4; 9|] = `All_bigger
  bsearch ~cmp:CCInt.compare 3 [| |] = `Empty
*)

let rec _for_all2 p a1 a2 i1 i2 ~len =
  len=0 || (p a1.(i1) a2.(i2) && _for_all2 p a1 a2 (i1+1) (i2+1) ~len:(len-1))

let for_all2 p a b =
  Array.length a = Array.length b
  &&
  _for_all2 p a b 0 0 ~len:(Array.length a)

let rec _exists2 p a1 a2 i1 i2 ~len =
  len>0 && (p a1.(i1) a2.(i2) || _exists2 p a1 a2 (i1+1) (i2+1) ~len:(len-1))

let exists2 p a b =
  _exists2 p a b 0 0 ~len:(min (Array.length a) (Array.length b))

let _fold2 f acc a b i j ~len =
  let rec aux acc o =
    if o=len then acc
    else
      let acc = f acc (Array.get a (i+o)) (Array.get b (j+o)) in
      aux acc (o+1)
  in
  aux acc 0

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
  let rec aux i =
    if i = Array.length a then true
    else eq a.(i) b.(i) && aux (i+1)
  in
  Array.length a = Array.length b
  &&
  aux 0

(*$Q
  Q.(pair (array small_int)(array small_int)) (fun (a,b) -> \
    equal (=) a b = equal (=) b a)
*)

(*$T
  equal (=) [|1|] [|1|]
*)

let compare cmp a b =
  let rec aux i =
    if i = Array.length a
    then if i = Array.length b then 0 else -1
    else if i = Array.length b
    then 1
    else
      let c = cmp a.(i) b.(i) in
      if c = 0 then aux (i+1) else c
  in
  aux 0

(*$T
  compare CCOrd.compare [| 1; 2; 3 |] [| 1; 2; 3 |] = 0
  compare CCOrd.compare [| 1; 2; 3 |] [| 2; 2; 3 |] < 0
  compare CCOrd.compare [| 1; 2; |] [| 1; 2; 3 |] < 0
  compare CCOrd.compare [| 1; 2; 3 |] [| 1; 2; |] > 0
*)

(* swap elements of array *)
let swap a i j =
  if i<>j then (
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp;
  )

(*$T
  let a = [| 1;2;3 |] in \
    swap a 0 1; \
    a = [| 2;1;3 |]
  let a = [| 1;2;3 |] in \
    swap a 0 2; \
    a = [| 3;2;1 |]
*)

(*$QR
  Q.(array_of_size Gen.(0 -- 100) small_int) (fun a ->
    let b = Array.copy a in
    for i = 0 to Array.length a-1 do
      for j = i+1 to Array.length a-1 do
        swap a i j; done; done;
    for i = 0 to Array.length a-1 do
      for j = i+1 to Array.length a-1 do
        swap a i j; done; done;
    a=b)
*)

(* shuffle a[i...j[ using the given int random generator
   See http://en.wikipedia.org/wiki/Fisher-Yates_shuffle *)
let _shuffle _rand_int a i j =
  for k = j-1 downto i+1 do
    let l = _rand_int (k+1) in
    let tmp = a.(l) in
    a.(l) <- a.(k);
    a.(k) <- tmp;
  done

let shuffle a =
  _shuffle Random.int a 0 (Array.length a)

let shuffle_with st a =
  _shuffle (Random.State.int st) a 0 (Array.length a)

let rec _to_klist a i j () =
  if i=j then `Nil else `Cons (a.(i), _to_klist a (i+1) j)

let random_choose a =
  let n = Array.length a in
  if n = 0 then invalid_arg "Array.random_choose";
  fun st -> a.(Random.State.int st n)

let random_len n g st =
  Array.init n (fun _ -> g st)

let random g st =
  let n = Random.State.int st 1_000 in
  random_len n g st

let random_non_empty g st =
  let n = 1 + Random.State.int st 1_000 in
  random_len n g st

let pp ?(sep=", ") pp_item out a =
  for k = 0 to Array.length a-1 do
    if k > 0 then (Format.pp_print_string out sep; Format.pp_print_cut out ());
    pp_item out a.(k)
  done

let pp_i ?(sep=", ") pp_item out a =
  for k = 0 to Array.length a - 1 do
    if k > 0 then (Format.pp_print_string out sep; Format.pp_print_cut out ());
    pp_item k out a.(k)
  done

let to_string ?(sep=", ") item_to_string a =
  Array.to_list a
  |> List.map item_to_string
  |> String.concat sep

(*$= to_string & ~printer:(fun s -> s)
  (to_string string_of_int [|1;2;3;4;5;6|]) "1, 2, 3, 4, 5, 6"
  (to_string string_of_int [||]) ""
  (to_string ~sep:" " string_of_int [|1;2;3;4;5;6|]) "1 2 3 4 5 6"
  (to_string string_of_int [|1|]) "1"
*)

let to_std_seq a =
  let rec aux i () =
    if i>= length a then Seq.Nil
    else Seq.Cons (a.(i), aux (i+1))
  in
  aux 0

(*$=
  [] (to_std_seq [||] |> CCList.of_std_seq)
  [1;2;3] (to_std_seq [|1;2;3|] |> CCList.of_std_seq)
  CCList.(1 -- 1000) (to_std_seq (1--1000) |> CCList.of_std_seq)
*)

let to_iter a k = iter k a

let to_gen a =
  let k = ref 0 in
  fun () ->
    if !k < Array.length a
    then (
      let x = a.(!k) in
      incr k;
      Some x
    ) else None

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
    ~cmp a
  =
  let module S = SortGeneric(A) in
  S.sort ~cmp a

(*$inject
  module IA = struct
    let get = Array.get
    let set = Array.set
    let length = Array.length
    type elt = int
    type t = int array
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
    Array.sort CCInt.compare a1; sort_generic (module IA) ~cmp:CCInt.compare a2; \
    a1 = a2 )
*)


module Infix = struct
  let (>>=) a f = flat_map f a
  let (>>|) a f = map f a
  let (>|=) a f = map f a
  let (--) = (--)
  let (--^) = (--^)

  include CCShimsMkLet_.Make(struct
      type 'a t = 'a array
      let (>>=) = (>>=)
      let (>|=) = (>|=)
      let monoid_product a1 a2 = monoid_product (fun x y->x,y) a1 a2
    end)
end

include Infix


(* test consistency of interfaces *)
(*$inject
  module FA = CCShimsArray_.Floatarray
  module type L = module type of CCArray with module Floatarray := FA
  module type LL = module type of CCArrayLabels with module Floatarray := FA
*)

(*$R
  ignore (module CCArrayLabels : L)
*)

(*$R
  ignore (module CCArray : LL)
*)
