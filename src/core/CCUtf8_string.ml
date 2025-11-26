(** {1 UTF8 strings} *)

(** Ref {{: https://en.wikipedia.org/wiki/UTF-8} Wikipedia}

    We only deal with UTF8 strings as they naturally map to OCaml bytestrings *)

type uchar = Uchar.t
type 'a gen = unit -> 'a option
type 'a iter = ('a -> unit) -> unit

(* compat shim *)

[@@@ocaml.warning "-32"]

let equal (a : string) b = Stdlib.( = ) a b
let hash : string -> int = Hashtbl.hash

[@@@ocaml.warning "+32"]

(* end compat shim *)

let pp = Format.pp_print_string

include String

let empty = ""
let to_string x = x

(** State for decoding *)
module Dec = struct
  type t = {
    s: string;
    len: int;
    (* max offset *) mutable i: int; (* offset *)
  }

  let make ?(idx = 0) (s : string) : t = { s; i = idx; len = String.length s }
end

let n_bytes = length

exception Malformed of string * int
(** Malformed string at given offset *)

(* decode next char. Mutate state, calls [yield c] if a char [c] is
   read, [stop ()] otherwise.
   @raise Malformed if an invalid substring is met *)
let next_ (type a) (st : Dec.t) ~(yield : uchar -> a) ~(stop : unit -> a) () : a
    =
  let open Dec in
  let malformed st = raise (Malformed (st.s, st.i)) in
  (* read a multi-byte character.
     @param acc the accumulator (containing the first byte of the char)
     @param n_bytes number of bytes to read (i.e. [width char - 1])
     @param overlong minimal bound on second byte (to detect overlong encoding)
  *)
  let read_multi ?(overlong = 0) n_bytes acc =
    (* inner loop j = 1..jmax *)
    let rec aux j acc =
      let c = Char.code st.s.[st.i + j] in
      (* check that c is in 0b10xxxxxx *)
      if c lsr 6 <> 0b10 then malformed st;
      (* overlong encoding? *)
      if j = 1 && overlong <> 0 && c land 0b111111 < overlong then malformed st;
      (* except for first, each char gives 6 bits *)
      let next = (acc lsl 6) lor (c land 0b111111) in
      if j = n_bytes then
        if
          (* done reading the codepoint *)
          Uchar.is_valid next
        then (
          st.i <- st.i + j + 1;
          (* +1 for first char *)
          yield (Uchar.unsafe_of_int next)
        ) else
          malformed st
      else
        aux (j + 1) next
    in
    assert (n_bytes >= 1);
    (* is the string long enough to contain the whole codepoint? *)
    if st.i + n_bytes < st.len then
      aux 1 acc
    (* start with j=1, first char is already processed! *)
    else
      (* char is truncated *)
      malformed st
  in
  if st.i >= st.len then
    stop ()
  else (
    let c = st.s.[st.i] in
    (* find leading byte, and detect some impossible cases
       according to https://en.wikipedia.org/wiki/Utf8#Codepage_layout *)
    match c with
    | '\000' .. '\127' ->
      st.i <- 1 + st.i;
      yield (Uchar.of_int @@ Char.code c) (* 0xxxxxxx *)
    | '\194' .. '\223' -> read_multi 1 (Char.code c land 0b11111) (* 110yyyyy *)
    | '\225' .. '\239' -> read_multi 2 (Char.code c land 0b1111) (* 1110zzzz *)
    | '\241' .. '\244' -> read_multi 3 (Char.code c land 0b111) (* 11110uuu *)
    | '\224' ->
      (* overlong: if next byte is < than [0b001000000] then the char
         would fit in 1 byte *)
      read_multi ~overlong:0b00100000 2 (Char.code c land 0b1111)
      (* 1110zzzz *)
    | '\240' ->
      (* overlong: if next byte is < than [0b000100000] then the char
         would fit in 2 bytes *)
      read_multi ~overlong:0b00010000 3 (Char.code c land 0b111)
      (* 11110uuu *)
    | '\128' .. '\193' (* 192,193 are forbidden *) | '\245' .. '\255' ->
      malformed st
  )

let to_gen ?(idx = 0) str : uchar gen =
  let st = Dec.make ~idx str in
  fun () -> next_ st ~yield:(fun c -> Some c) ~stop:(fun () -> None) ()

exception Stop

let to_iter ?(idx = 0) s : uchar iter =
 fun yield ->
  let st = Dec.make ~idx s in
  try
    while true do
      next_ st ~yield ~stop:(fun () -> raise Stop) ()
    done
  with Stop -> ()

let to_seq ?(idx = 0) s : uchar Seq.t =
  let rec loop st =
    let r = ref None in
    fun () ->
      match !r with
      | Some c -> c
      | None ->
        let c =
          next_ st
            ~yield:(fun x -> Seq.Cons (x, loop st))
            ~stop:(fun () -> Seq.Nil)
            ()
        in
        r := Some c;
        c
  in
  let st = Dec.make ~idx s in
  loop st

let iter ?idx f s = to_iter ?idx s f

let fold ?idx f acc s =
  let st = Dec.make ?idx s in
  let rec aux acc =
    next_ st
      ~yield:(fun x ->
        let acc = f acc x in
        aux acc)
      ~stop:(fun () -> acc)
      ()
  in
  aux acc

let n_chars = fold (fun x _ -> x + 1) 0

let to_list ?(idx = 0) s : uchar list =
  fold ~idx (fun acc x -> x :: acc) [] s |> List.rev

(* Convert a code point (int) into a string;
   There are various equally trivial versions of this around.
*)

let[@inline] uchar_to_bytes (c : uchar) (f : char -> unit) : unit =
  let c = Uchar.to_int c in
  let mask = 0b111111 in
  assert (Uchar.is_valid c);
  if c <= 0x7f then
    f (Char.unsafe_chr c)
  else if c <= 0x7ff then (
    f (Char.unsafe_chr (0xc0 lor (c lsr 6)));
    f (Char.unsafe_chr (0x80 lor (c land mask)))
  ) else if c <= 0xffff then (
    f (Char.unsafe_chr (0xe0 lor (c lsr 12)));
    f (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
    f (Char.unsafe_chr (0x80 lor (c land mask)))
  ) else if c <= 0x1fffff then (
    f (Char.unsafe_chr (0xf0 lor (c lsr 18)));
    f (Char.unsafe_chr (0x80 lor ((c lsr 12) land mask)));
    f (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
    f (Char.unsafe_chr (0x80 lor (c land mask)))
  ) else (
    f (Char.unsafe_chr (0xf8 lor (c lsr 24)));
    f (Char.unsafe_chr (0x80 lor ((c lsr 18) land mask)));
    f (Char.unsafe_chr (0x80 lor ((c lsr 12) land mask)));
    f (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
    f (Char.unsafe_chr (0x80 lor (c land mask)))
  )

(* number of bytes required to encode this codepoint. A skeleton version
   of {!uchar_to_bytes}. *)
let[@inline] uchar_num_bytes (c : uchar) : int =
  let c = Uchar.to_int c in
  if c <= 0x7f then
    1
  else if c <= 0x7ff then
    2
  else if c <= 0xffff then
    3
  else if c <= 0x1fffff then
    4
  else
    5

let of_gen g : t =
  let buf = Buffer.create 32 in
  let rec aux () =
    match g () with
    | None -> Buffer.contents buf
    | Some c ->
      uchar_to_bytes c (Buffer.add_char buf);
      aux ()
  in
  aux ()

let of_seq seq : t =
  let buf = Buffer.create 32 in
  Seq.iter (fun c -> uchar_to_bytes c (Buffer.add_char buf)) seq;
  Buffer.contents buf

let of_iter i : t =
  let buf = Buffer.create 32 in
  i (fun c -> uchar_to_bytes c (Buffer.add_char buf));
  Buffer.contents buf

let make n c =
  if n = 0 then
    empty
  else (
    let n_bytes = uchar_num_bytes c in
    let buf = Bytes.create (n * n_bytes) in
    (* copy [c] at the beginning of the buffer *)
    let i = ref 0 in
    uchar_to_bytes c (fun b ->
        Bytes.set buf !i b;
        incr i);
    (* now repeat the prefix n-1 times *)
    for j = 1 to n - 1 do
      Bytes.blit buf 0 buf (n_bytes * j) n_bytes
    done;
    Bytes.unsafe_to_string buf
  )

let[@inline] of_uchar c : t = make 1 c

let of_list l : t =
  let len = List.fold_left (fun n c -> n + uchar_num_bytes c) 0 l in
  if len > Sys.max_string_length then
    invalid_arg "CCUtf8_string.of_list: string size limit exceeded";
  let buf = Bytes.make len '\000' in
  let i = ref 0 in
  List.iter
    (fun c ->
      uchar_to_bytes c (fun byte ->
          Bytes.unsafe_set buf !i byte;
          incr i))
    l;
  assert (!i = len);
  Bytes.unsafe_to_string buf

let map f s : t =
  let buf = Buffer.create (n_bytes s) in
  iter (fun c -> uchar_to_bytes (f c) (Buffer.add_char buf)) s;
  Buffer.contents buf

let filter_map f s : t =
  let buf = Buffer.create (n_bytes s) in
  iter
    (fun c ->
      match f c with
      | None -> ()
      | Some c -> uchar_to_bytes c (Buffer.add_char buf))
    s;
  Buffer.contents buf

let flat_map f s : t =
  let buf = Buffer.create (n_bytes s) in
  iter (fun c -> iter (fun c -> uchar_to_bytes c (Buffer.add_char buf)) (f c)) s;
  Buffer.contents buf

let append = Stdlib.( ^ )
let unsafe_of_string s = s

let is_valid (s : string) : bool =
  try
    let st = Dec.make s in
    while true do
      next_ st ~yield:(fun _ -> ()) ~stop:(fun () -> raise Stop) ()
    done;
    assert false
  with
  | Malformed _ -> false
  | Stop -> true

let of_string_exn s =
  if is_valid s then
    s
  else
    invalid_arg "CCUtf8_string.of_string_exn"

let of_string s =
  if is_valid s then
    Some s
  else
    None
