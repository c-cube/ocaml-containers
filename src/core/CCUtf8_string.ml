
(** {1 UTF8 strings} *)

(** Ref {{: https://en.wikipedia.org/wiki/UTF-8} Wikipedia}

    We only deal with UTF8 strings as they naturally map to OCaml bytestrings *)

type uchar = Uchar.t
type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit

type t = string

let to_string x = x

let pp = Format.pp_print_string
let equal = String.equal
let compare = String.compare
let hash : t -> int = Hashtbl.hash

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

exception Malformed of string * int
(** Malformed string at given offset *)

(* decode next char. Mutate state, calls [yield c] if a char [c] is
   read, [stop ()] otherwise.
   @raise Malformed if an invalid substring is met *)
let next_ (type a) (st : Dec.t) ~(yield:uchar -> a) ~(stop:unit -> a) () : a =
  let open Dec in
  (* read a multi-byte character.
       @param acc the accumulator (containing the first byte of the char)
       @param n_bytes number of bytes to read (i.e. [width char - 1]) *)
  let read_multi n_bytes acc =
    (* inner loop j = 1..jmax *)
    let rec aux j acc =
      let c = Char.code st.s.[ st.i + j] in
      (* check that c is in 0b10xxxxxx *)
      if c lsr 6 <> 0b10 then raise (Malformed (st.s,st.i));
      (* except for first, each char gives 6 bits *)
      let next = (acc lsl 6) lor (c land 0b111111) in
      if j = n_bytes then (
        (* done reading the codepoint *)
        if Uchar.is_valid next then (
          st.i <- st.i + j + 1; (* +1 for first char *)
          yield (Uchar.unsafe_of_int next)
        ) else (
          raise (Malformed (st.s,st.i))
        )
      ) else (
        aux (j+1) next
      )
    in
    assert (n_bytes >= 1);
    (* is the string long enough to contain the whole codepoint? *)
    if st.i + n_bytes < st.len then (
      aux 1 acc (* start with j=1, first char is already proccessed! *)
    ) else (
      (* char is truncated *)
      raise (Malformed (st.s,st.i))
    )
  in
  if st.i >= st.len then (
    stop ()
  ) else (
    let c = st.s.[ st.i ] in
    match c with
      | '\000' .. '\127' ->
        st.i <- 1 + st.i;
        yield (Uchar.of_int @@ Char.code c) (* 0xxxxxxx *)
      | '\192' .. '\223' -> read_multi 1 ((Char.code c) land 0b11111)  (* 110yyyyy *)
      | '\224' .. '\239' -> read_multi 2 ((Char.code c) land 0b1111)   (* 1110zzzz *)
      | '\240' .. '\247' -> read_multi 3 ((Char.code c) land 0b111)    (* 11110uuu *)
      | '\128' .. '\191'
      | '\248' .. '\255' ->
        raise (Malformed (st.s,st.i))
  )

let to_gen ?(idx=0) str : uchar gen =
  let st = Dec.make ~idx str in
  fun () ->
    next_ st
      ~yield:(fun c -> Some c)
      ~stop:(fun () -> None)
      ()

exception Stop

let to_seq ?(idx=0) s : uchar sequence =
  fun yield ->
    let st = Dec.make ~idx s in
    try
      while true do
        next_ st ~yield
          ~stop:(fun () -> raise Stop)
          ()
      done
    with Stop -> ()

let iter ?idx f s = to_seq ?idx s f

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

let of_seq seq : t =
  let buf = Buffer.create 32 in
  seq (code_to_string buf);
  Buffer.contents buf

let of_list l : t =
  let buf = Buffer.create 32 in
  List.iter (code_to_string buf) l;
  Buffer.contents buf

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

let uutf_to_seq s f =
  Uutf.String.fold_utf_8
    (fun () _ -> function
       | `Malformed _ -> f (Uchar.of_int 0xfffd)
       | `Uchar c -> f c)
      () s
*)

(*$R
  let s = of_string_exn "このため、" in
  let s' = to_seq s |> of_seq in
  assert_equal ~cmp:equal ~printer s s'
*)

(*$QR
  Q.string (fun s ->
    Q.assume (CCString.for_all (fun c -> Char.code c < 128) s);
    is_valid s)
*)

(*$QR
  Q.string (fun s ->
    Q.assume (CCString.for_all (fun c -> Char.code c < 128) s);
    s = (of_string_exn s |> to_seq |> of_seq |> to_string)
  )
*)

(*$QR
  Q.string (fun s ->
    Q.assume (CCString.for_all (fun c -> Char.code c < 128) s);
    String.length s = List.length (of_string_exn s |> to_list)
  )
*)

(*$QR
  Q.string (fun s ->
    Q.assume (is_valid s);
    let s = of_string_exn s in
    let s2 = s |> to_seq |> of_seq in
    if s=s2 then true
    else Q.Test.fail_reportf "s=%S, s2=%S" (to_string s)(to_string s2)
  )
*)

(*$QR
  Q.string (fun s ->
    Q.assume (is_valid s);
    let s = of_string_exn s in
    let s2 = s |> to_gen |> of_gen in
    if s=s2 then true
    else Q.Test.fail_reportf "s=%S, s2=%S" (to_string s)(to_string s2)
  )
*)

(* compare with uutf *)

(*$QR
  Q.string (fun s ->
    let v1 = is_valid s in
    let v2 = uutf_is_valid s in
    if v1=v2 then true
    else Q.Test.fail_reportf "s:%S, valid: %B, uutf_valid: %B" s v1 v2
  )
*)

(*$QR
  Q.string (fun s ->
    Q.assume (is_valid s && uutf_is_valid s);
    let pp s = Q.Print.(list pp_uchar) s in
    let l_uutf = uutf_to_seq s |> Sequence.to_list in
    let l_co = of_string_exn s |> to_seq |> Sequence.to_list in
    if l_uutf = l_co then true
    else Q.Test.fail_reportf "uutf: '%s', containers: '%s', is_valid %B, uutf_is_valid %B"
      (pp l_uutf) (pp l_co) (is_valid s) (uutf_is_valid s)
  )
  *)
