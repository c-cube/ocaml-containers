module B = Buffer

module Out = struct
  type t = {
    char: char -> unit;
        (** Output a single char. The char is assumed not to be ['\n']. *)
    sub_string: string -> int -> int -> unit;
        (** Output a string slice (optim for [string]) *)
    string: string -> unit;  (** Output a string *)
    raw_string: string -> unit;
        (** Output a string that should not be modified in any way *)
    newline: unit -> unit;  (** Output a newline *)
  }

  let of_buffer (buf : Buffer.t) : t =
    let char = B.add_char buf in
    let sub_string = B.add_substring buf in
    let string = B.add_string buf in
    let newline () = B.add_char buf '\n' in
    let raw_string = string in
    { char; sub_string; string; newline; raw_string }
end

module Ext = struct
  type 'a t = {
    pre: Out.t -> 'a -> unit;  (** Printed before the wrapped value. *)
    post: Out.t -> 'a -> unit;  (** Printed after the wrapped value. *)
  }
end

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
  | Wrap : 'a Ext.t * 'a * t -> view

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

let ext ext v d : t = { view = Wrap (ext, v, d); wfl = d.wfl }
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
      if !i + 1 < len then cur := !cur ^ text_sub_ str !i (len - !i);
      i := len
    | j ->
      cur := !cur ^ text_sub_ str !i (j - !i) ^ nl;
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

module Flatten = struct
  let to_out (out : Out.t) (self : t) : unit =
    let rec loop (d : t) =
      match d.view with
      | Nil -> ()
      | Char c -> out.char c
      | Newline -> out.char ' '
      | Nest (_, x) -> loop x
      | Append (x, y) ->
        loop x;
        loop y
      | Text s -> out.string s
      | Text_sub (s, i, len) -> out.sub_string s i len
      | Group x -> loop x
      | Fill { sep; l } ->
        List.iteri
          (fun i x ->
            if i > 0 then loop sep;
            loop x)
          l
      | Wrap (ext, v, d) ->
        ext.pre out v;
        loop d;
        ext.post out v
    in
    loop self

  let to_buffer buf (self : t) : unit =
    let out = Out.of_buffer buf in
    to_out out self

  let to_string self : string =
    let buf = Buffer.create 32 in
    to_buffer buf self;
    Buffer.contents buf
end

module Pretty = struct
  type st = { out: Out.t; width: int }

  (** Add [i] spaces of indentation. *)
  let add_indent st (i : int) =
    for _i = 1 to i do
      st.out.char ' '
    done

  let rec pp_flatten (st : st) (self : t) : int =
    match self.view with
    | Nil -> 0
    | Char c ->
      st.out.char c;
      1
    | Newline ->
      st.out.char ' ';
      1
    | Nest (_i, x) -> pp_flatten st x
    | Append (x, y) ->
      let n = pp_flatten st x in
      n + pp_flatten st y
    | Text s ->
      st.out.string s;
      String.length s
    | Text_sub (s, i, len) ->
      st.out.sub_string s i len;
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
    | Wrap (ext, v, d) ->
      ext.pre st.out v;
      let n = pp_flatten st d in
      ext.post st.out v;
      n

  (** Does [x] fit in the current line when flattened, given that [k] chars
          are already on the line? *)
  let[@inline] fits_flattened st k x = x.wfl <= st.width - k

  let pp_newline (st : st) i =
    st.out.char '\n';
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
      st.out.char c;
      kont (k + 1)
    | Newline ->
      pp_newline st i;
      kont i
    | Nest (j, x) -> pp_rec_top st ~k ~i:(i + j) x kont
    | Append (x, y) ->
      (* print [x], then print [y] *)
      pp_rec_top st ~k ~i x (fun k -> pp_rec_top st ~k ~i y kont)
    | Text s ->
      st.out.string s;
      kont (k + String.length s)
    | Text_sub (s, i, len) ->
      st.out.sub_string s i len;
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
    | Wrap (ext, v, d) ->
      ext.pre st.out v;
      pp_rec_top st ~k ~i d (fun k ->
          ext.post st.out v;
          kont k)

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

  let to_out ~width out (self : t) : unit =
    let st = { out; width } in
    pp_rec st 0 [ 0, self ]

  let to_buffer ~width (buf : Buffer.t) (self : t) : unit =
    to_out ~width (Out.of_buffer buf) self

  let to_string ~width (self : t) : string =
    let buf = Buffer.create 32 in
    to_buffer ~width buf self;
    Buffer.contents buf

  let to_format ~width out self : unit =
    (* TODO: more efficient implementation based on:
       open a vbox; make custom out that directly emit Format.pp_foo calls;
       render to this out. *)
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
let text_quoted s : t = text (Printf.sprintf "%S" s)

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

let of_seq ?(sep = nil) f seq : t =
  let rec loop first seq =
    match seq () with
    | Seq.Nil -> nil
    | Seq.Cons (x, tl) ->
      let x = f x in
      (if first then
        x
      else
        sep ^ x)
      ^ loop false tl
  in
  loop true seq

let bracket l d r : t = group (text l ^ nest 2 (nl ^ d) ^ nl ^ text r)
let sexp_l l : t = char '(' ^ nest 1 (group (append_nl l ^ char ')'))
let sexp_apply a l : t = sexp_l (text a :: l)

module Dump = struct
  let list l : t =
    let sep = char ';' ^ nl in
    group (char '[' ^ nest 1 (fill sep l) ^ char ']')
end
