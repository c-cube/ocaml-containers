(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash combinators with 64-bit state} *)

type state = int64

let seed : state = Hash_impl_.seed

let[@inline] finalize64 (s : state) : int64 = Hash_impl_.fmix64 s
let[@inline] finalize (s : state) : int = Hash_impl_.finalize s

type 'a t = state -> 'a -> state

let[@inline] int s x = Hash_impl_.combine_int s x
let[@inline] bool s b = Hash_impl_.combine_int s (if b then 1 else 2)
let[@inline] char s c = Hash_impl_.combine_char s (Char.code c)
let[@inline] int64 s (x : int64) = Hash_impl_.combine_i64 s x
let[@inline] int32 s (x : int32) = Hash_impl_.combine_i32 s x
let[@inline] nativeint s (x : nativeint) = Hash_impl_.combine_i64 s (Int64.of_nativeint x)
let[@inline] string s x = Hash_impl_.combine_string s x
let[@inline] bytes s x = Hash_impl_.combine_string s (Bytes.unsafe_to_string x)

let slice str ofs s len =
  let j = ofs + len in
  let rec loop k st =
    if k = j then
      st
    else
      loop (k + 1) (Hash_impl_.combine_char st (Char.code (String.unsafe_get str k)))
  in
  loop ofs s

let opt f s = function
  | None -> Hash_impl_.combine_int s 0
  | Some x -> f (Hash_impl_.combine_int s 1) x

let list f s l = List.fold_left f s l
let array f s a = Array.fold_left f s a
let pair f g s (x, y) = g (f s x) y
let triple f g h s (x, y, z) = h (g (f s x) y) z
let quad f g h k s (x, y, z, w) = k (h (g (f s x) y) z) w

let map proj f s x = f s (proj x)
let if_ b then_ else_ s x = if b then then_ s x else else_ s x
let poly s x = Hash_impl_.combine_int s (Hashtbl.hash x)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let iter f s seq =
  let st = ref s in
  seq (fun x -> st := f !st x);
  !st

let seq f s sq = Seq.fold_left f s sq

let gen f s g =
  let rec aux st =
    match g () with
    | None -> st
    | Some x -> aux (f st x)
  in
  aux s

let array_comm f s a =
  let hashes = Array.map (fun x -> finalize64 (f seed x)) a in
  Array.sort Int64.compare hashes;
  Array.fold_left Hash_impl_.combine_i64 s hashes

let list_comm f s l =
  let arr = Array.of_list l in
  array_comm f s arr
