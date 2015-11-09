(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bloom Filter} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

type 'a hash_funs = ('a -> int) array

let primes_ = [|
  2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71;
  73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127; 131; 137; 139;
  149; 151; 157; 163; 167; 173
|]

let default_hash_funs k =
  Array.init k
    (fun i ->
      let seed = if i<Array.length primes_ then primes_.(i) else i in
      fun x -> Hashtbl.seeded_hash seed x
    )

(** {2 Bloom Filter} *)

type 'a t = {
  hash_funs : 'a hash_funs;
  arr : Bytes.t;
}

let mk_default_ size =
  default_hash_funs (max 2 (size / 20))

let create ?hash size =
  if size < 2 then invalid_arg "CCBloom.create";
  let hash_funs = match hash with
    | None -> mk_default_ size
    | Some h -> h
  in
  let arr = Bytes.make size '\000' in
  { hash_funs; arr }

let create_default ?hash_len size =
  let hash = match hash_len with
    | None -> mk_default_ size
    | Some n -> default_hash_funs n
  in
  create ~hash size

let copy f =
  {f with arr= Bytes.copy f.arr }

let size f = 8 * Bytes.length f.arr

(* number of 1 bits in [c] *)
let rec popcount_byte_ c =
  if c=0 then 0
  else
    (c land 1) + popcount_byte_ (c lsr 1)

let () = assert (
  popcount_byte_ 0 = 0 &&
  popcount_byte_ 3 = 2 &&
  popcount_byte_ 255 = 8
)

(* count the number of 1 bits *)
let rec count_ones_ arr i acc =
  if i=Bytes.length arr then acc
  else
    let c = Char.code (Bytes.get arr i) in
    count_ones_ arr (i+1) (acc + popcount_byte_ c)

let load f =
  let ones = count_ones_ f.arr 0 0 in
  float_of_int ones /. (float_of_int (Bytes.length f.arr * 8))

exception LocalExit

(* get i-th bit *)
let get_ arr i =
  let j = i / 8 in
  let c = Char.code (Bytes.get arr j) in
  c land (1 lsl (i mod 8)) <> 0

(* set i-th bit *)
let set_ arr i =
  let j = i / 8 in
  let c = Char.code (Bytes.get arr j) in
  let c = c lor (1 lsl (i mod 8)) in
  Bytes.set arr j (Char.chr c)

let mem f x =
  let n = size f in
  try
    Array.iter
      (fun hash -> if not (get_ f.arr (hash x mod n)) then raise LocalExit)
      f.hash_funs;
    true
  with LocalExit -> false

let add f x =
  let n = size f in
  Array.iter
    (fun hash -> set_ f.arr (hash x mod n))
    f.hash_funs

(*$Q
  Q.(list int) (fun l -> \
    let f = create 30 in add_list f l ; \
    List.for_all (mem f) l)
*)

let union_mut ~into f =
  if size into <> size f then invalid_arg "CCBloom.union_mut";
  Bytes.iteri
    (fun i c ->
      Bytes.set into.arr i
        (Char.chr (Char.code (Bytes.get into.arr i) lor (Char.code c)))
    ) f.arr

let union a b =
  if size a <> size b then invalid_arg "CCBloom.union";
  let into = copy a in
  union_mut ~into b;
  into

(*$Q
  Q.(pair (list int)(list int)) (fun (l1,l2) -> \
    let f1=create 100 and f2 = create 100 in \
    add_list f1 l1; add_list f2 l2; \
    let f = union f1 f2 in \
    List.for_all (fun i -> not (mem f1 i) || mem f i) l1 && \
    List.for_all (fun i -> not (mem f2 i) || mem f i) l2)
*)

let inter_mut ~into f =
  if size into <> size f then invalid_arg "CCBloom.inter_mut";
  Bytes.iteri
    (fun i c ->
      Bytes.set into.arr i
        (Char.chr (Char.code (Bytes.get into.arr i) land (Char.code c)))
    ) f.arr

let inter a b =
  if size a <> size b then invalid_arg "CCBloom.inter";
  let into = copy a in
  inter_mut ~into b;
  into

(*$Q
  Q.(pair (list int)(list int)) (fun (l1,l2) -> \
    let f1=create 100 and f2 = create 100 in \
    add_list f1 l1; add_list f2 l2; \
    let f = inter f1 f2 in \
    List.for_all (fun i -> not (mem f1 i) || not (mem f2 i) || mem f i) (l1@l2))
*)

let add_list f l = List.iter (add f) l

let add_seq f seq = seq (add f)

let rec add_gen f g = match g() with
  | None -> ()
  | Some x -> add f x; add_gen f g
