
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Array Slice} *)

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a random_gen = Random.State.t -> 'a
type 'a printer = Format.formatter -> 'a -> unit

(*$inject
  let (--) = CCArray.(--)
*)

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
  if i<0||i+len > Array.length arr then invalid_arg "Array_slice.make";
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
    (let a = make (0--10) 2 5 in sub a 1 2 |> to_list)
  [ ] \
    (let a = make (0--10) 2 5 in sub a 1 0 |> to_list)
  [ 5 ] \
    (let a = make (0--10) 1 9 in sub a 4 1 |> to_list)
*)

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
  if i<0 || j>=a.j then invalid_arg "Array_slice.get";
  a.arr.(j)

let get_safe a i =
  try Some (get a i)
  with Invalid_argument _ -> None

(*$inject
  let sub_a = make [|1;2;3;4;5|] 1 ~len:3
*)

(*$=
  (Some 2) (get_safe sub_a 0)
  (Some 3) (get_safe sub_a 1)
  (Some 4) (get_safe sub_a 2)
  None (get_safe sub_a 4)
  None (get_safe sub_a max_int)
  None (get_safe sub_a ~-1)
  None (get_safe sub_a ~-42)
*)

let set a i x =
  let j = a.i + i in
  if i<0 || j>=a.j then invalid_arg "Array_slice.set";
  a.arr.(j) <- x

let iter f a =
  for k=a.i to a.j-1 do f a.arr.(k) done

let iteri f a =
  for k=0 to length a-1 do f k a.arr.(a.i + k) done

let blit a i b j len =
  if i+len>length a || j+len>length b then invalid_arg "Array_slice.blit";
  Array.blit a.arr (a.i+i) b.arr (b.i+j) len

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
  let b = Array.copy a in CCArray.shuffle_with st a; a <> b
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

let _pp ~sep pp_item out a i j =
  for k = i to j - 1 do
    if k > i then (Format.pp_print_string out sep; Format.pp_print_cut out ());
    pp_item out a.(k)
  done

let _pp_i ~sep pp_item out a i j =
  for k = i to j - 1 do
    if k > i then (Format.pp_print_string out sep; Format.pp_print_cut out ());
    pp_item k out a.(k)
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

let reverse_in_place a = _reverse_in_place a.arr a.i ~len:(length a)

(*$T
  let a = 1--6 in let s = make a 2 ~len:3 in \
  reverse_in_place s; a = [| 1; 2; 5; 4; 3; 6 |]
*)

let sorted cmp a = _sorted cmp a.arr a.i a.j

(*$= & ~cmp:(=) ~printer:Q.Print.(array int)
  [||] \
    (let a = 1--6 in let s = make a 2 ~len:0 in \
     sorted Pervasives.compare s)
  [|2;3;4|] \
    (let a = [|6;5;4;3;2;1|] in let s = make a 2 ~len:3 in \
    sorted Pervasives.compare s)
*)

(*$Q
  Q.(array int) (fun a -> \
  Array.length a > 10 ==> ( Array.length a > 10 && \
  let s = make a 5 ~len:5 in \
  let b = Array.sub a 5 5 in \
  Array.sort Pervasives.compare b; b = sorted Pervasives.compare s))
*)

let sort_ranking cmp a =
  let idx = _sort_indices cmp a.arr a.i a.j in
  let cmp_int : int -> int -> int = Pervasives.compare in
  let sort_indices cmp a = _sort_indices cmp a 0 (Array.length a) in
  sort_indices cmp_int idx

(*$= & ~cmp:(=) ~printer:Q.Print.(array int)
  [||] \
     (let a = 1--6 in let s = make a 2 ~len:0 in \
     sort_ranking Pervasives.compare s)
  [|2;1;3;0|] \
    (let a = [|"d";"c";"b";"e";"a"|] in let s = make a 1 ~len:4 in \
    sort_ranking Pervasives.compare s)
*)

(*$Q
  Q.(array printable_string) (fun a -> \
  Array.length a > 10 ==> ( Array.length a > 10 && \
  let s = make a 5 ~len:5 in \
  let b = sort_indices String.compare s in \
  sorted String.compare s = Array.map (get s) b))
*)

let sort_indices cmp a = _sort_indices cmp a.arr a.i a.j

(*$= & ~cmp:(=) ~printer:Q.Print.(array int)
  [||] \
     (let a = 1--6 in let s = make a 2 ~len:0 in \
     sort_indices Pervasives.compare s)
  [|3;1;0;2|] \
    (let a = [|"d";"c";"b";"e";"a"|] in let s = make a 1 ~len:4 in \
    sort_indices Pervasives.compare s)
*)

(*$Q
  Q.(array printable_string) (fun a -> \
  Array.length a > 10 ==> ( Array.length a > 10 && \
  let s = make a 5 ~len:5 in \
  let b = sort_ranking String.compare s in \
  let a_sorted = sorted String.compare s in \
  copy s = Array.map (Array.get a_sorted) b))
*)


let find f a = _find (fun _ -> f) a.arr a.i a.j

let findi f a = _find (fun i -> f (i-a.i)) a.arr a.i a.j

let find_idx p a =
  _find (fun i x -> if p x then Some (i-a.i,x) else None) a.arr a.i a.j

(*$=
  (Some (1,"c")) (find_idx ((=) "c") (make [| "a"; "b"; "c" |] 1 2))
*)

let lookup_exn ?(cmp=Pervasives.compare) k a =
  _lookup_exn ~cmp k a.arr a.i (a.j-1) - a.i

let lookup ?(cmp=Pervasives.compare) k a =
  try Some (_lookup_exn ~cmp k a.arr a.i (a.j-1) - a.i)
  with Not_found -> None

(*$=
  (Some 1) (lookup "c" (make [| "a"; "b"; "c" |] 1 2))
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
  exists2 (=) (make [| 1;2;3;4 |] 1 ~len:2) (make [| 0;1;3;4 |] 1 ~len:3)
*)

let _iter2 f a b i j ~len =
  for o = 0 to len-1 do
    f (Array.get a (i+o)) (Array.get b (j+o))
  done

let iter2 f a b =
  if length a <> length b then invalid_arg "iter2";
  _iter2 f a.arr b.arr a.i b.i ~len:(length a)

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
  _fold2 f acc a.arr b.arr a.i b.i ~len:(length a)

let shuffle a =
  _shuffle Random.int a.arr a.i a.j

let shuffle_with st a =
  _shuffle (Random.State.int st) a.arr a.i a.j

let random_choose a st = _choose a.arr a.i a.j st

let pp ?(sep=", ") pp_item buf a = _pp ~sep pp_item buf a.arr a.i a.j

let pp_i ?(sep=", ") pp_item out a =
  _pp_i ~sep (fun k out x -> pp_item (k-a.i) out x) out a.arr a.i a.j

let to_seq a k = iter k a

let to_gen a = _to_gen a.arr a.i a.j

let to_klist a = _to_klist a.arr a.i a.j
