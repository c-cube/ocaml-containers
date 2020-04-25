
(** {1 UTF8 strings} *)

(** Ref {{: https://en.wikipedia.org/wiki/UTF-8} Wikipedia}

    We only deal with UTF8 strings as they naturally map to OCaml bytestrings *)

open CCShims_

type uchar = Uchar.t
type 'a gen = unit -> 'a option
type 'a iter = ('a -> unit) -> unit

let equal (a:string) b = Stdlib.(=) a b
let hash : string -> int = Hashtbl.hash
let pp = Format.pp_print_string

include String

let to_string x = x

(** State for decoding *)
module Dec = struct
  type t = {
    s: string;
    len: int; (* max offset *)
    mutable i: int; (* offset *)
  }

  let make ?(idx=0) (s:string) : t =
    { s=s; i=idx; len=String.length s; }
end

let n_bytes = length

exception Malformed of string * int
(** Malformed string at given offset *)

(* decode next char. Mutate state, calls [yield c] if a char [c] is
   read, [stop ()] otherwise.
   @raise Malformed if an invalid substring is met *)
let next_ (type a) (st : Dec.t) ~(yield:uchar -> a) ~(stop:unit -> a) () : a =
  let open Dec in
  let malformed st = raise (Malformed (st.s,st.i)) in
  (* read a multi-byte character.
     @param acc the accumulator (containing the first byte of the char)
     @param n_bytes number of bytes to read (i.e. [width char - 1])
     @param overlong minimal bound on second byte (to detect overlong encoding)
  *)
  let read_multi ?(overlong=0) n_bytes acc =
    (* inner loop j = 1..jmax *)
    let rec aux j acc =
      let c = Char.code st.s.[ st.i + j] in
      (* check that c is in 0b10xxxxxx *)
      if c lsr 6 <> 0b10 then malformed st;
      (* overlong encoding? *)
      if j=1 && overlong<>0 && (c land 0b111111) < overlong then malformed st;
      (* except for first, each char gives 6 bits *)
      let next = (acc lsl 6) lor (c land 0b111111) in
      if j = n_bytes then (
        (* done reading the codepoint *)
        if Uchar.is_valid next then (
          st.i <- st.i + j + 1; (* +1 for first char *)
          yield (Uchar.unsafe_of_int next)
        ) else (
          malformed st;
        )
      ) else (
        aux (j+1) next
      )
    in
    assert (n_bytes >= 1);
    (* is the string long enough to contain the whole codepoint? *)
    if st.i + n_bytes < st.len then (
      aux 1 acc (* start with j=1, first char is already processed! *)
    ) else (
      (* char is truncated *)
      malformed st;
    )
  in
  if st.i >= st.len then (
    stop ()
  ) else (
    let c = st.s.[ st.i ] in
    (* find leading byte, and detect some impossible cases
       according to https://en.wikipedia.org/wiki/Utf8#Codepage_layout *)
    match c with
      | '\000' .. '\127' ->
        st.i <- 1 + st.i;
        yield (Uchar.of_int @@ Char.code c) (* 0xxxxxxx *)
      | '\194' .. '\223' -> read_multi 1 ((Char.code c) land 0b11111)  (* 110yyyyy *)
      | '\225' .. '\239' -> read_multi 2 ((Char.code c) land 0b1111)   (* 1110zzzz *)
      | '\241' .. '\244' -> read_multi 3 ((Char.code c) land 0b111)    (* 11110uuu *)
      | '\224'           ->
        (* overlong: if next byte is < than [0b001000000] then the char
           would fit in 1 byte *)
        read_multi ~overlong:0b00100000 2 ((Char.code c) land 0b1111)   (* 1110zzzz *)
      | '\240'           ->
        (* overlong: if next byte is < than [0b000100000] then the char
           would fit in 2 bytes *)
        read_multi ~overlong:0b00010000 3 ((Char.code c) land 0b111)    (* 11110uuu *)
      | '\128' .. '\193' (* 192,193 are forbidden *)
      | '\245' .. '\255' -> malformed st;
  )

let to_gen ?(idx=0) str : uchar gen =
  let st = Dec.make ~idx str in
  fun () ->
    next_ st
      ~yield:(fun c -> Some c)
      ~stop:(fun () -> None)
      ()

exception Stop

let to_iter ?(idx=0) s : uchar iter =
  fun yield ->
    let st = Dec.make ~idx s in
    try
      while true do
        next_ st ~yield
          ~stop:(fun () -> raise Stop)
          ()
      done
    with Stop -> ()

let to_std_seq ?(idx=0) s : uchar Seq.t =
  let rec loop st =
    let r = ref None in
    fun () ->
      match !r with
      | Some c -> c
      | None ->
        let c = next_ st ~yield:(fun x -> Seq.Cons (x, loop st))
            ~stop:(fun () -> Seq.Nil) ()
        in
        r := Some c;
        c
  in
  let st = Dec.make ~idx s in
  loop st

(*$= & ~cmp:(=) ~printer:Q.Print.(list (fun c -> string_of_int@@ Uchar.to_int c))
  (to_list (of_string_exn "aébõ😀")) (to_std_seq (of_string_exn "aébõ😀") |> CCList.of_std_seq)
  *)

(* make sure it's persisted correctly *)
(*$R
  let s = (of_string_exn "aébõ😀") in
  let seq = to_std_seq s in
  let l = to_list s in
  let testeq seq = assert_equal ~cmp:(=) l (CCList.of_std_seq seq) in
  testeq seq;
  testeq seq;
  testeq seq;
  *)


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

let n_chars = fold (fun x _ -> x+1) 0

let to_list ?(idx=0) s : uchar list =
  fold ~idx (fun acc x -> x :: acc) [] s |> List.rev

(* Convert a code point (int) into a string;
   There are various equally trivial versions of this around.
*)

let code_to_string buf (c:uchar) : unit =
  let c = Uchar.to_int c in
  let mask = 0b111111 in
  assert (Uchar.is_valid c);
  if c <= 0x7f then (
    Buffer.add_char buf (Char.unsafe_chr c)
  ) else if c <= 0x7ff then (
    Buffer.add_char buf (Char.unsafe_chr (0xc0 lor (c lsr 6)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (c land mask)));
  ) else if c <= 0xffff then (
    Buffer.add_char buf (Char.unsafe_chr (0xe0 lor (c lsr 12)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (c land mask)));
  ) else if c <= 0x1fffff then (
    Buffer.add_char buf (Char.unsafe_chr (0xf0 lor (c lsr 18)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((c lsr 12) land mask)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (c land mask)));
  ) else (
    Buffer.add_char buf (Char.unsafe_chr (0xf8 lor (c lsr 24)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((c lsr 18) land mask)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((c lsr 12) land mask)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (c land mask)));
  )

let of_gen g : t =
  let buf = Buffer.create 32 in
  let rec aux () = match g() with
    | None -> Buffer.contents buf
    | Some c -> code_to_string buf c; aux ()
  in
  aux ()

let of_std_seq seq : t =
  let buf = Buffer.create 32 in
  Seq.iter (code_to_string buf) seq;
  Buffer.contents buf

let of_iter i : t =
  let buf = Buffer.create 32 in
  i (code_to_string buf);
  Buffer.contents buf

let of_list l : t =
  let buf = Buffer.create 32 in
  List.iter (code_to_string buf) l;
  Buffer.contents buf

let map f s : t =
  let buf = Buffer.create (n_bytes s) in
  iter (fun c -> code_to_string buf (f c)) s;
  Buffer.contents buf

let filter_map f s : t =
  let buf = Buffer.create (n_bytes s) in
  iter
    (fun c -> match f c with
       | None -> ()
       | Some c -> code_to_string buf c)
    s;
  Buffer.contents buf

let flat_map f s : t =
  let buf = Buffer.create (n_bytes s) in
  iter (fun c -> iter (code_to_string buf) (f c)) s;
  Buffer.contents buf

let append = Stdlib.(^)

let unsafe_of_string s = s

let is_valid (s:string) : bool =
  try
    let st = Dec.make s in
    while true do
      next_ st
        ~yield:(fun _ -> ())
        ~stop:(fun () -> raise Stop)
        ()
    done;
    assert false
  with
    | Malformed _ -> false
    | Stop -> true

let of_string_exn s =
  if is_valid s then s
  else invalid_arg "CCUtf8_string.of_string_exn"

let of_string s = if is_valid s then Some s else None

(*$inject

  let printer s = String.escaped (to_string s)
  let pp_uchar (c:Uchar.t) = Printf.sprintf "0x%x" (Uchar.to_int c)

  let uutf_is_valid s =
  try
    Uutf.String.fold_utf_8
      (fun () _ -> function
         | `Malformed _ -> raise Exit
         | `Uchar _ -> ())
      () s;
    true
  with Exit ->
    false

  let uutf_to_iter s f =
  Uutf.String.fold_utf_8
    (fun () _ -> function
       | `Malformed _ -> f (Uchar.of_int 0xfffd)
       | `Uchar c -> f c)
      () s
*)

(*$R
  let s = of_string_exn "このため、" in
  let s' = to_iter s |> of_iter in
  assert_equal ~cmp:equal ~printer s s'
*)

(*$QR
  Q.small_string (fun s ->
    Q.assume (CCString.for_all (fun c -> Char.code c < 128) s);
    is_valid s)
*)

(*$QR & ~long_factor:10
  Q.small_string (fun s ->
    Q.assume (CCString.for_all (fun c -> Char.code c < 128) s);
    s = (of_string_exn s |> to_iter|> of_iter|> to_string)
  )
*)

(*$QR & ~long_factor:10
  Q.string (fun s ->
    Q.assume (CCString.for_all (fun c -> Char.code c < 128) s);
    String.length s = List.length (of_string_exn s |> to_list)
  )
*)

(*$T
  not (is_valid "\192\181")
  not (is_valid "\193\143")
  not (is_valid "\224\151\167")
  not (is_valid "\224\137\165")
  is_valid "\240\151\189\163"
*)

(*$QR & ~long_factor:40
  Q.string (fun s ->
    Q.assume (is_valid s);
    let s = of_string_exn s in
    let s2 = s |> to_iter|> of_iter in
    if s=s2 then true
    else Q.Test.fail_reportf "s=%S, s2=%S" (to_string s)(to_string s2)
  )
*)

(*$QR & ~long_factor:40
  Q.string (fun s ->
    Q.assume (is_valid s);
    let s = of_string_exn s in
    let s2 = s |> to_gen |> of_gen in
    if s=s2 then true
    else Q.Test.fail_reportf "s=%S, s2=%S" (to_string s)(to_string s2)
  )
*)

(* compare with uutf *)

(*$QR & ~long_factor:40 ~count:50_000
  Q.small_string (fun s ->
    let v1 = is_valid s in
    let v2 = uutf_is_valid s in
    if v1=v2 then true
    else Q.Test.fail_reportf "s:%S, valid: %B, uutf_valid: %B" s v1 v2
  )
*)

(*$QR & ~long_factor:40 ~count:50_000
  Q.small_string (fun s ->
    Q.assume (is_valid s && uutf_is_valid s);
    let pp s = Q.Print.(list pp_uchar) s in
    let l_uutf = uutf_to_iter s |> Iter.to_list in
    let l_co = of_string_exn s |> to_iter |> Iter.to_list in
    if l_uutf = l_co then true
    else Q.Test.fail_reportf "uutf: '%s', containers: '%s', is_valid %B, uutf_is_valid %B"
      (pp l_uutf) (pp l_co) (is_valid s) (uutf_is_valid s)
  )
*)
