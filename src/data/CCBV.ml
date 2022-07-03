(** {2 Imperative Bitvectors} *)

(* TODO: move to [bytes] and replace all [mod] and [/] with bitshifts
   because width_=8 *)

let width_ = Sys.word_size - 1

(** We use OCamls ints to store the bits. We index them from the
    least significant bit. We create masks to zero out the most significant
    bits that aren't used to store values. This is necessary when we are
    constructing or negating a bit vector. *)
let lsb_masks_ =
  let a = Array.make (width_ + 1) 0 in
  for i = 1 to width_ do
    a.(i) <- a.(i - 1) lor (1 lsl (i - 1))
  done;
  a

let all_ones_ = lsb_masks_.(width_)

(* count the 1 bits in [n]. See https://en.wikipedia.org/wiki/Hamming_weight *)
let count_bits_ n =
  let rec recurse count n =
    if n = 0 then
      count
    else
      recurse (count + 1) (n land (n - 1))
  in
  recurse 0 n

(* Can I access the "private" members in testing? $Q
   (Q.int_bound (Sys.word_size - 1)) (fun i -> count_bits_ lsb_masks_.(i) = i)
*)

type t = { mutable a: int array; mutable size: int }

let length t = t.size
let empty () = { a = [||]; size = 0 }

let array_length_of_size size =
  if size mod width_ = 0 then
    size / width_
  else
    (size / width_) + 1

let create ~size default =
  if size = 0 then
    { a = [||]; size }
  else (
    let n = array_length_of_size size in
    let a =
      if default then
        Array.make n all_ones_
      else
        Array.make n 0
    in
    (* adjust last bits *)
    let r = size mod width_ in
    if default && r <> 0 then Array.unsafe_set a (n - 1) lsb_masks_.(r);
    { a; size }
  )

let copy bv = { bv with a = Array.copy bv.a }
let capacity bv = width_ * Array.length bv.a

let cardinal bv =
  if bv.size = 0 then
    0
  else (
    let n = ref 0 in
    for i = 0 to Array.length bv.a - 1 do
      n := !n + count_bits_ bv.a.(i) (* MSB of last element are all 0 *)
    done;
    !n
  )

let really_resize_ bv ~desired ~current size =
  let a' = Array.make desired 0 in
  Array.blit bv.a 0 a' 0 current;
  bv.a <- a';
  bv.size <- size

let grow_ bv size =
  if size <= capacity bv (* within capacity *) then
    bv.size <- size
  else (
    (* beyond capacity *)
    let desired = array_length_of_size size in
    let current = Array.length bv.a in
    assert (desired > current);
    really_resize_ bv ~desired ~current size
  )

let shrink_ bv size =
  let desired = array_length_of_size size in
  let current = Array.length bv.a in
  really_resize_ bv ~desired ~current size

let resize bv size =
  if size < 0 then invalid_arg "resize: negative size";
  if size < bv.size (* shrink *) then
    shrink_ bv size
  else if size = bv.size then
    ()
  else
    grow_ bv size

let is_empty bv =
  try
    for i = 0 to Array.length bv.a - 1 do
      if bv.a.(i) <> 0 then raise Exit (* MSB of last element are all 0 *)
    done;
    true
  with Exit -> false

let get bv i =
  if i < 0 then invalid_arg "get: negative index";
  let n = i / width_ in
  let i = i mod width_ in
  if n < Array.length bv.a then
    Array.unsafe_get bv.a n land (1 lsl i) <> 0
  else
    false

let set bv i =
  if i < 0 then
    invalid_arg "set: negative index"
  else (
    let n = i / width_ in
    let j = i mod width_ in
    if i >= bv.size then grow_ bv (i + 1);
    Array.unsafe_set bv.a n (Array.unsafe_get bv.a n lor (1 lsl j))
  )

let reset bv i =
  if i < 0 then
    invalid_arg "reset: negative index"
  else (
    let n = i / width_ in
    let j = i mod width_ in
    if i >= bv.size then grow_ bv (i + 1);
    Array.unsafe_set bv.a n (Array.unsafe_get bv.a n land lnot (1 lsl j))
  )

let flip bv i =
  if i < 0 then
    invalid_arg "reset: negative index"
  else (
    let n = i / width_ in
    let j = i mod width_ in
    if i >= bv.size then grow_ bv (i + 1);
    Array.unsafe_set bv.a n (Array.unsafe_get bv.a n lxor (1 lsl j))
  )

let clear bv = Array.fill bv.a 0 (Array.length bv.a) 0
let equal x y : bool = x.size = y.size && x.a = y.a

let iter bv f =
  let len = array_length_of_size bv.size in
  assert (len <= Array.length bv.a);
  for n = 0 to len - 2 do
    let j = width_ * n in
    let word_n = Array.unsafe_get bv.a n in
    for i = 0 to width_ - 1 do
      f (j + i) (word_n land (1 lsl i) <> 0)
    done
  done;
  if bv.size > 0 then (
    let j = width_ * (len - 1) in
    let r = bv.size mod width_ in
    let final_length =
      if r = 0 then
        width_
      else
        r
    in
    let final_word = Array.unsafe_get bv.a (len - 1) in
    for i = 0 to final_length - 1 do
      f (j + i) (final_word land (1 lsl i) <> 0)
    done
  )

let[@inline] iter_true bv f =
  iter bv (fun i b ->
      if b then
        f i
      else
        ())

let to_list bv =
  let l = ref [] in
  iter_true bv (fun i -> l := i :: !l);
  !l

let to_sorted_list bv = List.rev (to_list bv)

(* Interpret these as indices. *)
let of_list l =
  let size = List.fold_left max 0 l + 1 in
  let bv = create ~size false in
  List.iter (fun i -> set bv i) l;
  bv

exception FoundFirst of int

let first_exn bv =
  try
    iter_true bv (fun i -> raise (FoundFirst i));
    raise Not_found
  with FoundFirst i -> i

let first bv = try Some (first_exn bv) with Not_found -> None
let filter bv p = iter_true bv (fun i -> if not (p i) then reset bv i)

let negate_self b =
  let len = Array.length b.a in
  for n = 0 to len - 1 do
    Array.unsafe_set b.a n (lnot (Array.unsafe_get b.a n))
  done;
  let r = b.size mod width_ in
  if r <> 0 then (
    let l = Array.length b.a - 1 in
    Array.unsafe_set b.a l (lsb_masks_.(r) land Array.unsafe_get b.a l)
  )

let negate b =
  let a = Array.map lnot b.a in
  let r = b.size mod width_ in
  if r <> 0 then (
    let l = Array.length b.a - 1 in
    Array.unsafe_set a l (lsb_masks_.(r) land Array.unsafe_get a l)
  );
  { a; size = b.size }

let union_into_no_resize_ ~into bv =
  assert (Array.length into.a >= Array.length bv.a);
  for i = 0 to Array.length bv.a - 1 do
    Array.unsafe_set into.a i
      (Array.unsafe_get into.a i lor Array.unsafe_get bv.a i)
  done

(* Underlying size grows for union. *)
let union_into ~into bv =
  if into.size < bv.size then grow_ into bv.size;
  union_into_no_resize_ ~into bv

(* To avoid potentially 2 passes, figure out what we need to copy. *)
let union b1 b2 =
  if b1.size <= b2.size then (
    let into = copy b2 in
    union_into_no_resize_ ~into b1;
    into
  ) else (
    let into = copy b1 in
    union_into_no_resize_ ~into b2;
    into
  )

let inter_into_no_resize_ ~into bv =
  assert (Array.length into.a <= Array.length bv.a);
  for i = 0 to Array.length into.a - 1 do
    Array.unsafe_set into.a i
      (Array.unsafe_get into.a i land Array.unsafe_get bv.a i)
  done

(* Underlying size shrinks for inter. *)
let inter_into ~into bv =
  if into.size > bv.size then shrink_ into bv.size;
  inter_into_no_resize_ ~into bv

let inter b1 b2 =
  if b1.size <= b2.size then (
    let into = copy b1 in
    inter_into_no_resize_ ~into b2;
    into
  ) else (
    let into = copy b2 in
    inter_into_no_resize_ ~into b1;
    into
  )

(* Underlying size depends on the 'in_' set for diff, so we don't change
   it's size! *)
let diff_into ~into bv =
  let n = min (Array.length into.a) (Array.length bv.a) in
  for i = 0 to n - 1 do
    Array.unsafe_set into.a i
      (Array.unsafe_get into.a i land lnot (Array.unsafe_get bv.a i))
  done

let diff in_ not_in =
  let into = copy in_ in
  diff_into ~into not_in;
  into

let select bv arr =
  let l = ref [] in
  (try
     iter_true bv (fun i ->
         if i >= Array.length arr then
           raise Exit
         else
           l := arr.(i) :: !l)
   with Exit -> ());
  !l

let selecti bv arr =
  let l = ref [] in
  (try
     iter_true bv (fun i ->
         if i >= Array.length arr then
           raise Exit
         else
           l := (arr.(i), i) :: !l)
   with Exit -> ());
  !l

type 'a iter = ('a -> unit) -> unit

let to_iter bv k = iter_true bv k

let of_iter seq =
  let l = ref [] and maxi = ref 0 in
  seq (fun x ->
      l := x :: !l;
      maxi := max !maxi x);
  let bv = create ~size:(!maxi + 1) false in
  List.iter (fun i -> set bv i) !l;
  bv

let pp out bv =
  Format.pp_print_string out "bv {";
  iter bv (fun _i b ->
      Format.pp_print_char out
        (if b then
          '1'
        else
          '0'));
  Format.pp_print_string out "}"

let __to_word_l bv = Array.to_list bv.a
