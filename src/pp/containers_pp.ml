type t = {
  view: view;  (** Document view *)
  wfl: int;  (** Width if flattened *)
}

and view =
  | Nil
  | Newline
  | Nest of int * t
  | Append of t * t
  | Char of char
  | Text of string
  | Text_sub of string * int * int
  | Group of t
  | Fill of { sep: t; l: t list }

let nil : t = { view = Nil; wfl = 0 }
let newline : t = { view = Newline; wfl = 1 }
let nl = newline

let char c =
  if c = '\n' then
    nl
  else
    { view = Char c; wfl = 1 }

let nest i x : t =
  match x.view with
  | _ when i <= 0 -> x
  | Nil -> nil
  | _ -> { view = Nest (i, x); wfl = x.wfl }

let append a b : t =
  match a.view, b.view with
  | Nil, _ -> b
  | _, Nil -> a
  | _ -> { view = Append (a, b); wfl = a.wfl + b.wfl }

let group d : t =
  match d.view with
  | Nil -> nil
  | Group _ -> d
  | _ -> { view = Group d; wfl = d.wfl }

let ( ^ ) = append
let text_sub_ s i len : t = { view = Text_sub (s, i, len); wfl = len }

(* Turn [str], which contains some newlines, into a document.
   We make a concatenation of
   each line's content followed by a newline.
   Then we group the result so that it remains in a unified block. *)
let split_text_ (str : string) : t =
  let cur = ref nil in
  let i = ref 0 in
  let len = String.length str in
  while !i < len do
    match String.index_from str !i '\n' with
    | exception Not_found ->
      (* last chunk *)
      if !i + 1 < len then cur := !cur ^ text_sub_ str !i (len - 1 - !i);
      i := len
    | j ->
      cur := !cur ^ text_sub_ str !i (j - 1 - !i) ^ nl;
      i := j + 1
  done;
  group !cur

let text (str : string) : t =
  if str = "" then
    nil
  else if String.contains str '\n' then
    split_text_ str
  else
    { view = Text str; wfl = String.length str }

let textpf fmt = Printf.ksprintf text fmt
let textf fmt = Format.kasprintf text fmt

module B = Buffer

module Flatten = struct
  let to_buffer buf (self : t) : unit =
    let rec loop (d : t) =
      match d.view with
      | Nil -> ()
      | Char c -> B.add_char buf c
      | Newline -> B.add_char buf ' '
      | Nest (_, x) -> loop x
      | Append (x, y) ->
        loop x;
        loop y
      | Text s -> B.add_string buf s
      | Text_sub (s, i, len) -> B.add_substring buf s i len
      | Group x -> loop x
      | Fill { sep; l } ->
        List.iteri
          (fun i x ->
            if i > 0 then loop sep;
            loop x)
          l
    in
    loop self

  let to_string self : string =
    let buf = Buffer.create 32 in
    to_buffer buf self;
    Buffer.contents buf
end

module Pretty = struct
  type st = { buf: Buffer.t; width: int }

  (** Add [i] spaces of indentation. *)
  let add_indent (st : st) (i : int) =
    for _i = 1 to i do
      B.add_char st.buf ' '
    done

  let rec pp_flatten (st : st) (self : t) : int =
    match self.view with
    | Nil -> 0
    | Char c ->
      B.add_char st.buf c;
      1
    | Newline ->
      B.add_char st.buf ' ';
      1
    | Nest (_i, x) -> pp_flatten st x
    | Append (x, y) ->
      let n = pp_flatten st x in
      n + pp_flatten st y
    | Text s ->
      B.add_string st.buf s;
      String.length s
    | Text_sub (s, i, len) ->
      B.add_substring st.buf s i len;
      len
    | Group x -> pp_flatten st x
    | Fill { sep; l } ->
      (* print separated by spaces *)
      let n = ref 0 in
      List.iteri
        (fun i x ->
          if i > 0 then n := !n + pp_flatten st sep;
          n := !n + pp_flatten st x)
        l;
      !n

  (** Does [x] fit in the current line when flattened, given that [k] chars
          are already on the line? *)
  let[@inline] fits_flattened st k x = x.wfl <= st.width - k

  let pp_newline st i =
    B.add_char st.buf '\n';
    add_indent st i

  (** Print [self] into the buffer.
      @param k how many chars are already printed on the current line
  *)
  let rec pp_rec (st : st) (k : int) (stack : (int * t) list) : unit =
    match stack with
    | [] -> ()
    | (i, d) :: stack_tl ->
      pp_rec_top st ~k ~i d (fun k -> pp_rec st k stack_tl)

  (** Print [d] at indentation [i], with [k] chars already printed
        on the current line, then calls [kont] with the
        new [k]. *)
  and pp_rec_top st ~k ~i d (kont : int -> unit) : unit =
    match d.view with
    | Nil -> kont k
    | Char c ->
      B.add_char st.buf c;
      kont (k + 1)
    | Newline ->
      pp_newline st i;
      kont i
    | Nest (j, x) -> pp_rec_top st ~k ~i:(i + j) x kont
    | Append (x, y) ->
      (* print [x], then print [y] *)
      pp_rec_top st ~k ~i x (fun k -> pp_rec_top st ~k ~i y kont)
    | Text s ->
      B.add_string st.buf s;
      kont (k + String.length s)
    | Text_sub (s, i, len) ->
      B.add_substring st.buf s i len;
      kont (k + len)
    | Group x ->
      if fits_flattened st k x then (
        (* print flattened *)
        let w_x = pp_flatten st x in
        assert (w_x = x.wfl);
        kont (k + w_x)
      ) else
        pp_rec_top st ~k ~i x kont
    | Fill { sep; l } -> pp_fill st ~k ~i sep l kont

  and pp_fill st ~k ~i sep l (kont : int -> unit) : unit =
    (* [k] is the current offset in the line *)
    let rec loop idx k l =
      match l with
      | x :: tl ->
        if fits_flattened st k x then (
          (* all flattened *)
          let w_sep =
            if idx = 0 then
              0
            else
              pp_flatten st sep
          in
          let w_x = pp_flatten st x in
          assert (w_x = x.wfl);
          loop (idx + 1) (k + w_x + w_sep) tl
        ) else (
          (* print, followed by a newline and resume filling with [k=i] *)
          let pp_and_continue k =
            pp_rec_top st ~k ~i x (fun k -> loop (idx + 1) k tl)
          in
          if idx > 0 then
            (* separator, then item *)
            pp_rec_top st ~k ~i sep pp_and_continue
          else
            pp_and_continue k
        )
      | [] -> kont k
    in
    loop 0 k l

  let to_buffer ~width (buf : Buffer.t) (self : t) : unit =
    let st = { buf; width } in
    pp_rec st 0 [ 0, self ]

  let to_string ~width (self : t) : string =
    let buf = Buffer.create 32 in
    to_buffer ~width buf self;
    Buffer.contents buf

  let to_format ~width out self : unit =
    (* TODO: more efficient implementation *)
    CCFormat.string_lines out (to_string ~width self)
end

let pp = Pretty.to_format ~width:80

(* helpers *)

let sp = char ' '

module Infix = struct
  let ( ^ ) = append
  let[@inline] ( ^+ ) x y = x ^ sp ^ y
  let[@inline] ( ^/ ) x y = x ^ nl ^ y
end

include Infix

let true_ = text "true"
let false_ = text "false"

let bool b =
  if b then
    true_
  else
    false_

let int x : t = text (string_of_int x)
let float x : t = text (string_of_float x)
let float_hex x : t = textpf "%h" x

let append_l ?(sep = nil) l =
  let rec loop = function
    | [] -> nil
    | [ x ] -> x
    | x :: tl -> x ^ sep ^ loop tl
  in
  loop l

let append_sp l = append_l ~sep:sp l
let append_nl l = append_l ~sep:nl l

let fill sep = function
  | [] -> nil
  | [ x ] -> x
  | l ->
    (* flattened: just like concat *)
    let wfl =
      List.fold_left (fun wfl x -> wfl + x.wfl) 0 l
      + ((List.length l - 1) * sep.wfl)
    in
    { view = Fill { sep; l }; wfl }

let fill_map sep f l = fill sep (List.map f l)

let of_list ?(sep = nil) f l =
  let rec loop = function
    | [] -> nil
    | [ x ] -> f x
    | x :: tl -> f x ^ sep ^ loop tl
  in
  loop l

let bracket l d r : t = group (text l ^ nest 2 (nl ^ d) ^ nl ^ text r)
let sexp_l l : t = char '(' ^ nest 1 (group (append_nl l ^ char ')'))
let sexp_apply a l : t = sexp_l (text a :: l)

module Dump = struct
  let list l : t =
    let sep = char ';' ^ nl in
    group (char '[' ^ nest 1 (fill sep l) ^ char ']')
end
