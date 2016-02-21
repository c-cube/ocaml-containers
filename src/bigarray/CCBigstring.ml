
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Interface to 1-dimension Bigarrays of bytes (char)} *)

module B = Bigarray.Array1

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let create size = B.create Bigarray.char Bigarray.c_layout size

let empty = create 0

let init size f =
  let a = create size in
  for i = 0 to size-1 do
    B.unsafe_set a i (f i)
  done;
  a

let fill = B.fill

let get = B.get

let set = B.set

let size = B.dim
let length = B.dim

let sub = B.sub

let blit a i b j len =
  let a' = sub a i len in
  let b' = sub b j len in
  B.blit a' b'

let copy a =
  let b = create (size a) in
  B.blit a b;
  b

(*$T
  copy (of_string "abcd") |> to_string = "abcd"
  *)

let fold f acc a =
  let rec fold' f acc a i len =
    if i = len then acc
    else
      let acc = f acc (get a i) in
      fold' f acc a (i+1) len
  in
  fold' f acc a 0 (size a)

let iter f a =
  let n = size a in
  for i = 0 to n-1 do
    f (get a i)
  done

let rec equal_rec a b i len =
  i = len
  ||
  ( get a i = get b i && equal_rec a b (i+1) len)

let equal a b =
  size a = size b
  &&
  equal_rec a b 0 (size a)

(*$Q
  Q.(pair printable_string printable_string) (fun (s1, s2) -> \
    let a1 = of_string s1 and a2 = of_string s2 in \
    equal a1 a2 = (s1 = s2))
*)

let rec compare_rec a b i len_a len_b =
  if i=len_a && i=len_b then 0
  else if i=len_a then -1
  else if i=len_b then 1
  else
    match Char.compare (get a i) (get b i) with
    | 0 -> compare_rec a b (i+1) len_a len_b
    | n -> n

let compare a b =
  compare_rec a b 0 (size a) (size b)

(*$T
  compare (of_string "abc") (of_string "abd") < 0
  compare (of_string "abc") (of_string "abcd") < 0
  compare (of_string "abcd") (of_string "abc") > 0
  compare (of_string "abc") (of_string "b") < 0
*)

(*$Q
  Q.(pair string string) (fun (s1, s2) -> \
    let a1 = of_string s1 and a2 = of_string s2 in \
    CCInt.sign (compare a1 a2) = CCInt.sign (String.compare s1 s2))
*)

(** {2 Conversions} *)

let to_bytes a =
  Bytes.init (size a) (fun i -> B.unsafe_get a i)

let of_bytes b =
  init (Bytes.length b) (fun i -> Bytes.get b i)

let of_bytes_slice b i len =
  if i < 0 || i+len > Bytes.length b then invalid_arg "CCBigstring";
  init len (fun j -> Bytes.get b (i+j))

let sub_bytes a i len =
  if i < 0 || i+len > size a then invalid_arg "CCBigstring";
  Bytes.init len (fun j -> B.get a (i+j))

let blit_to_bytes a i b j len =
  if i < 0 || j < 0 || i+len > size a || j+len > Bytes.length b
    then invalid_arg "CCBigstring";
  for x=0 to len-1 do
    Bytes.set b (j+x) (B.get a (i+x))
  done

let blit_of_bytes a i b j len =
  if i < 0 || j < 0 || i+len > Bytes.length a || j+len > size b
    then invalid_arg "CCBigstring";
  for x=0 to len-1 do
    B.set b (j+x) (Bytes.get a (i+x))
  done

let to_string a =
  CCString.init (size a) (fun i -> B.unsafe_get a i)

let of_string s =
  init (String.length s) (fun i -> String.get s i)

let of_string_slice s i len =
  if i < 0 || i+len > String.length s then invalid_arg "CCBigstring";
  init len (fun j -> String.get s (i+j))

let sub_string a i len =
  if i < 0 || i+len > size a then invalid_arg "CCBigstring";
  CCString.init len (fun j -> B.get a (i+j))

(*$T
  of_string_slice "abcde" 1 3 |> to_string = "bcd"
*)

let blit_of_string a i b j len =
  if i < 0 || j < 0 || i+len > String.length a || j+len > size b
    then invalid_arg "CCBigstring";
  for x=0 to len-1 do
    B.set b (j+x) (String.get a (i+x))
  done

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

let to_seq a k = iter k a

let to_gen a =
  let i = ref 0 in
  let n = size a in
  fun () ->
    if !i = n then None
    else (
      let x = get a !i in
      incr i;
      Some x
    )

(*$T
  of_string "abcd" |> to_gen |> Gen.to_string = "abcd"
*)

let to_seq_slice a i len =
  to_seq (sub a i len)

let to_gen_slice a i len =
  to_gen (sub a i len)

let print out s =
  Format.pp_print_string out "bigstring \"";
  iter
    (function
      | '\n' -> Format.pp_print_string out "\\n"
      | '\t' -> Format.pp_print_string out "\\t"
      | '\\' -> Format.pp_print_string out "\\\\"
      | c -> Format.pp_print_char out c
    ) s;
  Format.pp_print_char out '"'

(** {2 Memory-map} *)

let map_file_descr ?pos ?(shared=false) fd len =
  B.map_file fd ?pos Bigarray.char Bigarray.c_layout shared len

let with_map_file ?pos ?len ?(mode=0o644) ?(flags=[Open_rdonly]) ?shared name f =
  let ic = open_in_gen flags mode name in
  let len = match len with
    | None -> in_channel_length ic
    | Some n -> n
  in
  let a = map_file_descr ?pos ?shared (Unix.descr_of_in_channel ic) len in
  try
    let x = f a in
    close_in ic;
    x
  with e ->
    close_in ic;
    raise e
