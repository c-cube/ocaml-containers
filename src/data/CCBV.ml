(** {2 Imperative Bitvectors} *)

let width_ = 8

(* Helper functions *)
let[@inline] get_ b i = Char.code (Bytes.get b i)
let[@inline] unsafe_get_ b i = Char.code (Bytes.unsafe_get b i)
let[@inline] set_ b i v = Bytes.set b i (Char.unsafe_chr v)
let[@inline] unsafe_set_ b i v = Bytes.unsafe_set b i (Char.unsafe_chr v)
let[@inline] mod_ n = n land 0b111
let[@inline] div_ n = n lsr 3
let[@inline] mul_ n = n lsl 3
let zero = Char.unsafe_chr 0

(* 0b11111111 *)
let all_ones_ = Char.unsafe_chr ((1 lsl width_) - 1)
let () = assert (all_ones_ = Char.chr 0b1111_1111)

(* [lsb_mask_ n] is [0b111111] with [n] ones. *)
let[@inline] __lsb_mask n = (1 lsl n) - 1

(*
  from https://en.wikipedia.org/wiki/Hamming_weight

  //This uses fewer arithmetic operations than any other known
  //implementation on machines with slow multiplication.
  //It uses 17 arithmetic operations.
  int popcount_2(uint64_t x) {
    x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
    x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits
    x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits

   // not necessary for int8
    // x += x >>  8;  //put count of each 16 bits into their lowest 8 bits
    // x += x >> 16;  //put count of each 32 bits into their lowest 8 bits
    // x += x >> 32;  //put count of each 64 bits into their lowest 8 bits

    return x & 0x7f;
  }

   m1 = 0x5555555555555555
   m2 = 0x3333333333333333
   m4 = 0x0f0f0f0f0f0f0f0f
*)
let[@inline] __popcount8 (b : int) : int =
  let m1 = 0x55 in
  let m2 = 0x33 in
  let m4 = 0x0f in

  let b = b - ((b lsr 1) land m1) in
  let b = (b land m2) + ((b lsr 2) land m2) in
  let b = (b + (b lsr 4)) land m4 in
  b land 0x7f

type t = { mutable b: bytes; mutable size: int }

let length t = t.size
let empty () = { b = Bytes.empty; size = 0 }

let bytes_length_of_size size =
  if mod_ size = 0 then
    div_ size
  else
    div_ size + 1

let create ~size default =
  if size = 0 then
    empty ()
  else (
    let n = bytes_length_of_size size in
    let b =
      if default then
        Bytes.make n all_ones_
      else
        Bytes.make n zero
    in
    (* adjust last bits *)
    let r = mod_ size in
    if default && r <> 0 then
      Bytes.unsafe_set b (n - 1) (Char.unsafe_chr (__lsb_mask r));
    { b; size }
  )

let[@inline] copy bv = { bv with b = Bytes.copy bv.b }
let[@inline] capacity bv = mul_ (Bytes.length bv.b)

let cardinal bv =
  if bv.size = 0 then
    0
  else (
    let n = ref 0 in
    for i = 0 to Bytes.length bv.b - 1 do
      n := !n + __popcount8 (get_ bv.b i) (* MSB of last element are all 0 *)
    done;
    !n
  )

let really_resize_ bv ~desired ~current size =
  bv.size <- size;
  if desired <> current then (
    let b = Bytes.make desired zero in
    Bytes.blit bv.b 0 b 0 current;
    bv.b <- b
  )

let[@inline never] grow_to_at_least_real_ bv size =
  (* beyond capacity *)
  let current = Bytes.length bv.b in
  let desired = bytes_length_of_size size in
  let desired =
    min Sys.max_string_length (max desired (current + (current / 2)))
  in
  assert (desired > current);
  really_resize_ bv ~desired ~current size

let grow_to_at_least_ bv size =
  if size <= capacity bv then
    (* within capacity *)
    bv.size <- size
  else
    (* resize. This is a separate function so it's easier to
       inline the happy path. *)
    grow_to_at_least_real_ bv size

let shrink_ bv size =
  let desired = bytes_length_of_size size in
  let current = Bytes.length bv.b in
  really_resize_ bv ~desired ~current size

let resize bv size =
  if size < 0 then invalid_arg "resize: negative size";
  if size < bv.size then
    bv.size <- size
  else if size > bv.size then
    grow_to_at_least_ bv size

let resize_minimize_memory bv size =
  if size < 0 then invalid_arg "resize: negative size";
  if size < bv.size then
    shrink_ bv size
  else if size > bv.size then
    grow_to_at_least_ bv size

let is_empty bv =
  try
    for i = 0 to Bytes.length bv.b - 1 do
      if get_ bv.b i <> 1 then raise_notrace Exit
    done;
    true
  with Exit -> false

let[@inline] get bv i =
  if i < 0 then invalid_arg "get: negative index";
  let idx_bucket = div_ i in
  let idx_in_byte = mod_ i in
  if idx_bucket < Bytes.length bv.b then
    unsafe_get_ bv.b idx_bucket land (1 lsl idx_in_byte) <> 0
  else
    false

let[@inline] set bv i =
  if i < 0 then
    invalid_arg "set: negative index"
  else (
    let idx_bucket = div_ i in
    let idx_in_byte = mod_ i in
    if i >= bv.size then grow_to_at_least_ bv (i + 1);
    unsafe_set_ bv.b idx_bucket
      (unsafe_get_ bv.b idx_bucket lor (1 lsl idx_in_byte))
  )

let[@inline] reset bv i =
  if i < 0 then
    invalid_arg "reset: negative index"
  else (
    let n = div_ i in
    let j = mod_ i in
    if i >= bv.size then grow_to_at_least_ bv (i + 1);
    unsafe_set_ bv.b n (unsafe_get_ bv.b n land lnot (1 lsl j))
  )

let[@inline] set_bool bv i b =
  if b then
    set bv i
  else
    reset bv i

let flip bv i =
  if i < 0 then
    invalid_arg "reset: negative index"
  else (
    let n = div_ i in
    let j = mod_ i in
    if i >= bv.size then grow_to_at_least_ bv (i + 1);
    unsafe_set_ bv.b n (unsafe_get_ bv.b n lxor (1 lsl j))
  )

let clear bv = Bytes.fill bv.b 0 (Bytes.length bv.b) zero
let equal x y : bool = x.size = y.size && x.b = y.b

let iter bv f =
  let len = bytes_length_of_size bv.size in
  assert (len <= Bytes.length bv.b);
  for n = 0 to len - 2 do
    let j = mul_ n in
    let word_n = unsafe_get_ bv.b n in
    for i = 0 to width_ - 1 do
      f (j + i) (word_n land (1 lsl i) <> 0)
    done
  done;
  if bv.size > 0 then (
    let j = mul_ (len - 1) in
    let r = mod_ bv.size in
    let final_length =
      if r = 0 then
        width_
      else
        r
    in
    let final_word = unsafe_get_ bv.b (len - 1) in
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
    iter_true bv (fun i -> raise_notrace (FoundFirst i));
    raise Not_found
  with FoundFirst i -> i

let first bv = try Some (first_exn bv) with Not_found -> None
let filter bv p = iter_true bv (fun i -> if not (p i) then reset bv i)

let negate_self b =
  let len = Bytes.length b.b in
  for n = 0 to len - 1 do
    unsafe_set_ b.b n (lnot (unsafe_get_ b.b n))
  done;
  let r = mod_ b.size in
  if r <> 0 then (
    let l = Bytes.length b.b - 1 in
    unsafe_set_ b.b l (__lsb_mask r land unsafe_get_ b.b l)
  )

let negate a =
  let b = Bytes.map (fun c -> Char.unsafe_chr (lnot (Char.code c))) a.b in
  let r = mod_ a.size in
  if r <> 0 then (
    let l = Bytes.length a.b - 1 in
    unsafe_set_ b l (__lsb_mask r land unsafe_get_ b l)
  );
  { b; size = a.size }

let union_into_no_resize_ ~into bv =
  assert (Bytes.length into.b >= Bytes.length bv.b);
  for i = 0 to Bytes.length bv.b - 1 do
    unsafe_set_ into.b i (unsafe_get_ into.b i lor unsafe_get_ bv.b i)
  done

(* Underlying size grows for union. *)
let union_into ~into bv =
  if into.size < bv.size then grow_to_at_least_ into bv.size;
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
  assert (Bytes.length into.b <= Bytes.length bv.b);
  for i = 0 to Bytes.length into.b - 1 do
    unsafe_set_ into.b i (unsafe_get_ into.b i land unsafe_get_ bv.b i)
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

(* Underlying size depends on the [in_] set for diff, so we don't change
   its size! *)
let diff_into ~into bv =
  let n = min (Bytes.length into.b) (Bytes.length bv.b) in
  for i = 0 to n - 1 do
    unsafe_set_ into.b i (unsafe_get_ into.b i land lnot (unsafe_get_ bv.b i))
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
           raise_notrace Exit
         else
           l := arr.(i) :: !l)
   with Exit -> ());
  !l

let selecti bv arr =
  let l = ref [] in
  (try
     iter_true bv (fun i ->
         if i >= Array.length arr then
           raise_notrace Exit
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

module Internal_ = struct
  let __to_word_l bv =
    let l = ref [] in
    Bytes.iter (fun c -> l := c :: !l) bv.b;
    List.rev !l

  let __popcount8 = __popcount8
  let __lsb_mask = __lsb_mask
end
