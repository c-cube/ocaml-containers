module B = Buffer
module Int_map = Map.Make (CCInt)

type 'a iter = ('a -> unit) -> unit

module Out = struct
  type t = {
    char: char -> unit;
        (** Output a single char. The char is assumed not to be ['\n']. *)
    sub_string: string -> int -> int -> unit;
        (** Output a string slice (optim for [string]) *)
    string: string -> unit;  (** Output a string *)
    newline: unit -> unit;  (** Output a newline *)
  }

  let of_buffer (buf : Buffer.t) : t =
    let char = B.add_char buf in
    let sub_string = B.add_substring buf in
    let string = B.add_string buf in
    let newline () = B.add_char buf '\n' in
    { char; sub_string; string; newline }

  let[@inline] char self c = self.char c
  let[@inline] string self s = self.string s
  let[@inline] sub_string self s i len = self.sub_string s i len
  let[@inline] newline self = self.newline ()
end

module Ext = struct
  type view = ..

  type 'a key = {
    id: int;
    inject: 'a -> view;
    extract: view -> 'a option;
  }

  type map = view Int_map.t

  let empty : map = Int_map.empty

  let get k (self : map) : _ option =
    try k.extract @@ Int_map.find k.id self with Not_found -> None

  let add k v self : map = Int_map.add k.id (k.inject v) self

  type 'a t = {
    name: string;
    k: 'a key;
    width: 'a -> int;
    pre: Out.t -> inside:'a option -> 'a -> unit;
    post: Out.t -> inside:'a option -> 'a -> unit;
  }

  let key_counter_ = ref 0

  let make (type a) ?(width = fun _ -> 0) ~name ~pre ~post () : a t =
    let module M = struct
      type view += V of a
    end in
    let k =
      {
        id = !key_counter_;
        inject = (fun x -> M.V x);
        extract =
          (function
          | M.V x -> Some x
          | _ -> None);
      }
    in
    incr key_counter_;
    { name; k; width; pre; post }
end

type t = {
  view: view;  (** Document view *)
  wfl: int;  (** Width if flattened *)
}

and view =
  | Nil
  | Newline of int
  | Nest of int * t
  | Append of t * t
  | Char of char
  | Text of string
  | Text_sub of string * int * int
  | Text_zero_width of string
  | Group of t
  | Fill of {
      sep: t;
      l: t list;
    }
  | Wrap : 'a Ext.t * 'a * t -> view

(* debug printer *)
let rec debug out (self : t) : unit =
  match self.view with
  | Nil -> Format.fprintf out "nil"
  | Newline 1 -> Format.fprintf out "nl"
  | Newline i -> Format.fprintf out "nl(%d)" i
  | Nest (i, x) -> Format.fprintf out "(@[nest %d@ %a@])" i debug x
  | Append (a, b) -> Format.fprintf out "@[%a ^@ %a@]" debug a debug b
  | Char c -> Format.fprintf out "%C" c
  | Text s -> Format.fprintf out "%S" s
  | Text_zero_width s -> Format.fprintf out "(zw %S)" s
  | Text_sub (s, i, len) -> Format.fprintf out "%S" (String.sub s i len)
  | Group d -> Format.fprintf out "(@[group@ %a@])" debug d
  | Fill { sep = _; l } ->
    Format.fprintf out "(@[fill@ %a@])" (Format.pp_print_list debug) l
  | Wrap (e, _, d) -> Format.fprintf out "(@[ext.%s@ %a@])" e.name debug d

let nil : t = { view = Nil; wfl = 0 }
let newline : t = { view = Newline 1; wfl = 1 }

let newline_or_spaces n : t =
  if n < 0 then invalid_arg "Containers_pp.newline_or_spaces";
  { view = Newline n; wfl = n }

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

let ext (ext : _ Ext.t) v d : t =
  let wfl = d.wfl + ext.width v in
  { view = Wrap (ext, v, d); wfl }

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
  !cur

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
    let rec loop (ext_map : Ext.map) (d : t) =
      match d.view with
      | Nil | Newline 0 -> ()
      | Char c -> out.char c
      | Newline 1 -> out.char ' '
      | Newline n ->
        for _i = 1 to n do
          out.char ' '
        done
      | Nest (_, x) -> loop ext_map x
      | Append (x, y) ->
        loop ext_map x;
        loop ext_map y
      | Text s | Text_zero_width s -> out.string s
      | Text_sub (s, i, len) -> out.sub_string s i len
      | Group x -> loop ext_map x
      | Fill { sep; l } ->
        List.iteri
          (fun i x ->
            if i > 0 then loop ext_map sep;
            loop ext_map x)
          l
      | Wrap (ext, v, d) ->
        let inside = Ext.get ext.k ext_map in
        ext.pre out ~inside v;
        let ext_map' = Ext.add ext.k v ext_map in
        loop ext_map' d;
        ext.post out ~inside v
    in
    loop Ext.empty self

  let to_buffer buf (self : t) : unit =
    let out = Out.of_buffer buf in
    to_out out self

  let to_string self : string =
    let buf = Buffer.create 32 in
    to_buffer buf self;
    Buffer.contents buf
end

module Pretty = struct
  type st = {
    out: Out.t;
    width: int;
    ext_map: Ext.map;
  }

  (** Add [i] spaces of indentation. *)
  let add_indent st (i : int) =
    for _i = 1 to i do
      st.out.char ' '
    done

  let rec pp_flatten (st : st) (self : t) : int =
    match self.view with
    | Nil | Newline 0 -> 0
    | Char c ->
      st.out.char c;
      1
    | Newline n ->
      for _i = 1 to n do
        st.out.char ' '
      done;
      n
    | Nest (_i, x) -> pp_flatten st x
    | Append (x, y) ->
      let n = pp_flatten st x in
      n + pp_flatten st y
    | Text s ->
      st.out.string s;
      String.length s
    | Text_zero_width s ->
      st.out.string s;
      0
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
      let inside = Ext.get ext.k st.ext_map in
      ext.pre st.out ~inside v;
      let st' = { st with ext_map = Ext.add ext.k v st.ext_map } in
      let n = pp_flatten st' d in
      ext.post st.out ~inside v;
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
      pp_rec_top st ~k ~i d (fun st k -> pp_rec st k stack_tl)

  (** Print [d] at indentation [i], with [k] chars already printed
      on the current line, then calls [kont] with the
      new [k]. *)
  and pp_rec_top st ~k ~i d (kont : st -> int -> unit) : unit =
    match d.view with
    | Nil -> kont st k
    | Char c ->
      st.out.char c;
      kont st (k + 1)
    | Newline _ ->
      pp_newline st i;
      kont st i
    | Nest (j, x) -> pp_rec_top st ~k ~i:(i + j) x kont
    | Append (x, y) ->
      (* print [x], then print [y] *)
      pp_rec_top st ~k ~i x (fun st k -> pp_rec_top st ~k ~i y kont)
    | Text s ->
      st.out.string s;
      kont st (k + String.length s)
    | Text_zero_width s ->
      st.out.string s;
      kont st k
    | Text_sub (s, i, len) ->
      st.out.sub_string s i len;
      kont st (k + len)
    | Group x ->
      if fits_flattened st k x then (
        (* print flattened *)
        let w_x = pp_flatten st x in
        assert (w_x = x.wfl);
        kont st (k + w_x)
      ) else
        pp_rec_top st ~k ~i x kont
    | Fill { sep; l } -> pp_fill st ~k ~i sep l kont
    | Wrap (ext, v, d) ->
      let old_ext_map = st.ext_map in
      let inside = Ext.get ext.k st.ext_map in
      ext.pre st.out ~inside v;
      let st' = { st with ext_map = Ext.add ext.k v st.ext_map } in
      pp_rec_top st' ~k ~i d (fun st k ->
          ext.post st.out ~inside v;
          kont { st with ext_map = old_ext_map } k)

  and pp_fill st ~k ~i sep l (kont : st -> int -> unit) : unit =
    (* [k] is the current offset in the line *)
    let rec loop st idx k l =
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
          loop st (idx + 1) (k + w_x + w_sep) tl
        ) else (
          (* print, followed by a newline and resume filling with [k=i] *)
          let pp_and_continue st k =
            pp_rec_top st ~k ~i x (fun st k -> loop st (idx + 1) k tl)
          in
          if idx > 0 then
            (* separator, then item *)
            pp_rec_top st ~k ~i sep pp_and_continue
          else
            pp_and_continue st k
        )
      | [] -> kont st k
    in
    loop st 0 k l

  let to_out ~width out (self : t) : unit =
    let st = { out; width; ext_map = Ext.empty } in
    pp_rec st 0 [ 0, self ]

  let to_buffer ~width (buf : Buffer.t) (self : t) : unit =
    to_out ~width (Out.of_buffer buf) self

  let to_string ~width (self : t) : string =
    let buf = Buffer.create 32 in
    to_buffer ~width buf self;
    Buffer.contents buf

  let to_format ~width out self : unit =
    (* TODO: more efficient implementation based on out *)
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
let text_zero_width s : t = { view = Text_zero_width s; wfl = 0 }

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

let bracket l d r : t = group (text l ^ nest (String.length l) d ^ text r)
let bracket2 l d r : t = group (text l ^ nest 2 (nl ^ d) ^ nl ^ text r)
let sexp_l l : t = char '(' ^ nest 1 (group (append_nl l ^ char ')'))
let sexp_apply a l : t = sexp_l (text a :: l)
let surround ?(width = 1) l b r = group (l ^ nest width b ^ r)

module Char = struct
  let bang = char '!'
  let at = char '@'
  let hash = char '#'
  let dollar = char '$'
  let tilde = char '~'
  let backquote = char '`'
  let percent = char '%'
  let caret = char '^'
  let ampersand = char '&'
  let star = char '*'
  let minus = char '-'
  let underscore = char '_'
  let plus = char '+'
  let equal = char '='
  let pipe = char '|'
  let slash = char '/'
  let backslash = char '\\'
  let colon = char ':'
  let semi = char ';'
  let guillemet = char '"'
  let quote = char '\''
  let comma = char ','
  let dot = char '.'
  let question = char '?'
  let lparen = char '('
  let rparen = char ')'
  let lbrace = char '{'
  let rbrace = char '}'
  let lbracket = char '['
  let rbracket = char ']'
  let langle = char '<'
  let rangle = char '>'
end

module Dump = struct
  let list l : t =
    let sep = char ';' ^ nl in
    group (char '[' ^ nest 1 (fill sep l) ^ char ']')

  let parens d = surround Char.lparen d Char.rparen
  let braces d = surround Char.lbrace d Char.rbrace
  let brackets d = surround Char.lbracket d Char.rbracket
  let angles d = surround Char.langle d Char.rangle

  let of_iter ?(sep = nil) g it =
    let r = ref nil in
    it (fun elt -> r := !r ^ sep ^ g elt);
    !r

  let of_array ?(sep = nil) g arr =
    let r = ref nil in
    for i = 0 to Array.length arr - 1 do
      r := !r ^ sep ^ g arr.(i)
    done;
    !r
end

module Term_color = struct
  type color =
    [ `Black
    | `Red
    | `Yellow
    | `Green
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    ]

  type style =
    [ `FG of color (* foreground *)
    | `BG of color (* background *)
    | `Bold
    | `Reset
    | `Underline
    ]

  let int_of_color_ = function
    | `Black -> 0
    | `Red -> 1
    | `Green -> 2
    | `Yellow -> 3
    | `Blue -> 4
    | `Magenta -> 5
    | `Cyan -> 6
    | `White -> 7

  let code_of_style : style -> int = function
    | `FG c -> 30 + int_of_color_ c
    | `BG c -> 40 + int_of_color_ c
    | `Bold -> 1
    | `Reset -> 0
    | `Underline -> 4

  let spf = Printf.sprintf
  let string_of_style a = spf "\x1b[%dm" (code_of_style a)
  let reset = string_of_style `Reset

  let string_of_style_list = function
    | [] -> reset
    | [ a ] -> string_of_style a
    | [ a; b ] -> spf "\x1b[%d;%dm" (code_of_style a) (code_of_style b)
    | [ a; b; c ] ->
      spf "\x1b[%d;%d;%dm" (code_of_style a) (code_of_style b) (code_of_style c)
    | l ->
      let buf = Buffer.create 32 in
      let pp_num c = Buffer.add_string buf (string_of_int (code_of_style c)) in
      Buffer.add_string buf "\x1b[";
      List.iteri
        (fun i c ->
          if i > 0 then Buffer.add_char buf ';';
          pp_num c)
        l;
      Buffer.add_string buf "m";
      Buffer.contents buf

  let ext_style_ : style list Ext.t =
    Ext.make ~name:"termcolor"
      ~pre:(fun out ~inside:_ l -> Out.string out (string_of_style_list l))
      ~post:(fun out ~inside _l ->
        let style =
          CCOption.map_or ~default:reset string_of_style_list inside
        in
        Out.string out style)
      ()

  (** Set the foreground color. *)
  let color (c : color) (d : t) : t = ext ext_style_ [ `FG c ] d

  (** Set a full style for this document. *)
  let style_l (l : style list) (d : t) : t = ext ext_style_ l d
end
