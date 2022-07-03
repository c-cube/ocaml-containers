(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Helpers for Format} *)

type 'a iter = ('a -> unit) -> unit

include Format

type t = Format.formatter
type -'a printer = t -> 'a -> unit

(** {2 Combinators} *)

let silent _fmt _ = ()
let return fmt_str out () = Format.fprintf out "%(%)" fmt_str
let unit fmt () = Format.pp_print_string fmt "()"
let int fmt i = Format.pp_print_string fmt (string_of_int i)
let string = Format.pp_print_string
let bool = Format.pp_print_bool
let float3 fmt f = Format.fprintf fmt "%.3f" f
let float fmt f = Format.pp_print_string fmt (string_of_float f)
let char = Format.pp_print_char
let int32 fmt n = Format.fprintf fmt "%ld" n
let int64 fmt n = Format.fprintf fmt "%Ld" n
let nativeint fmt n = Format.fprintf fmt "%nd" n
let string_quoted fmt s = Format.fprintf fmt "\"%s\"" s
let flush = Format.pp_print_flush
let space = Format.pp_print_space
let cut = Format.pp_print_cut
let break fmt (m, n) = Format.pp_print_break fmt m n
let newline = Format.pp_force_newline
let substring out (s, i, len) : unit = string out (String.sub s i len)
let text = Format.pp_print_text

let string_lines out (s : string) : unit =
  fprintf out "@[<v>";
  let i = ref 0 in
  let n = String.length s in
  while !i < n do
    let j = try String.index_from s !i '\n' with Not_found -> n in
    if !i > 0 then fprintf out "@,";
    substring out (s, !i, j - !i);
    i := j + 1
  done;
  fprintf out "@]"

let list ?(sep = return ",@ ") pp fmt l =
  let rec pp_list l =
    match l with
    | x :: (_ :: _ as l) ->
      pp fmt x;
      sep fmt ();
      pp_list l
    | [ x ] -> pp fmt x
    | [] -> ()
  in
  pp_list l

let array ?(sep = return ",@ ") pp fmt a =
  for i = 0 to Array.length a - 1 do
    if i > 0 then sep fmt ();
    pp fmt a.(i)
  done

let arrayi ?(sep = return ",@ ") pp fmt a =
  for i = 0 to Array.length a - 1 do
    if i > 0 then sep fmt ();
    pp fmt (i, a.(i))
  done

let seq ?(sep = return ",@ ") pp fmt seq =
  let first = ref true in
  CCSeq.iter
    (fun x ->
      if !first then
        first := false
      else
        sep fmt ();
      pp fmt x)
    seq

let iter ?(sep = return ",@ ") pp fmt seq =
  let first = ref true in
  seq (fun x ->
      if !first then
        first := false
      else
        sep fmt ();
      pp fmt x)

let opt pp fmt x =
  match x with
  | None -> Format.pp_print_string fmt "none"
  | Some x -> Format.fprintf fmt "some %a" pp x

let pair ?(sep = return ",@ ") ppa ppb fmt (a, b) =
  Format.fprintf fmt "%a%a%a" ppa a sep () ppb b

let triple ?(sep = return ",@ ") ppa ppb ppc fmt (a, b, c) =
  Format.fprintf fmt "%a%a%a%a%a" ppa a sep () ppb b sep () ppc c

let quad ?(sep = return ",@ ") ppa ppb ppc ppd fmt (a, b, c, d) =
  Format.fprintf fmt "%a%a%a%a%a%a%a" ppa a sep () ppb b sep () ppc c sep () ppd
    d

let append ppa ppb fmt () =
  ppa fmt ();
  ppb fmt ()

let append_l ppl fmt () = List.iter (fun pp -> pp fmt ()) ppl

let within a b p out x =
  string out a;
  p out x;
  string out b

let map f pp fmt x =
  pp fmt (f x);
  ()

let vbox ?(i = 0) pp out x =
  Format.pp_open_vbox out i;
  pp out x;
  Format.pp_close_box out ()

let hovbox ?(i = 0) pp out x =
  Format.pp_open_hovbox out i;
  pp out x;
  Format.pp_close_box out ()

let hvbox ?(i = 0) pp out x =
  Format.pp_open_hvbox out i;
  pp out x;
  Format.pp_close_box out ()

let hbox pp out x =
  Format.pp_open_hbox out ();
  pp out x;
  Format.pp_close_box out ()

let of_to_string f out x = Format.pp_print_string out (f x)
let exn = of_to_string Printexc.to_string
let const pp x out () = pp out x

let some pp out = function
  | None -> ()
  | Some x -> pp out x

let const_string s out _ = string out s
let opaque out _ = string out "opaque"
let lazy_force pp out (lazy x) = pp out x

let lazy_or ?(default = return "<lazy>") pp out x =
  if Lazy.is_val x then
    pp out (Lazy.force x)
  else
    default out ()

(** {2 IO} *)

let output fmt pp x = pp fmt x

let to_string pp x =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let fprintf = Format.fprintf
let stdout = Format.std_formatter
let stderr = Format.err_formatter
let of_chan = Format.formatter_of_out_channel

let with_out_chan oc f =
  let fmt = of_chan oc in
  try
    let x = f fmt in
    Format.pp_print_flush fmt ();
    x
  with e ->
    Format.pp_print_flush fmt ();
    raise e

let tee a b =
  let fa = Format.pp_get_formatter_out_functions a () in
  let fb = Format.pp_get_formatter_out_functions b () in
  Format.make_formatter
    (fun str i len ->
      fa.Format.out_string str i len;
      fb.Format.out_string str i len)
    (fun () ->
      fa.Format.out_flush ();
      fb.Format.out_flush ())

let to_file filename format =
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  Format.kfprintf
    (fun fmt ->
      Format.pp_print_flush fmt ();
      close_out_noerr oc)
    fmt format

module ANSI_codes = struct
  type color =
    [ `Black | `Red | `Yellow | `Green | `Blue | `Magenta | `Cyan | `White ]

  let int_of_color_ = function
    | `Black -> 0
    | `Red -> 1
    | `Green -> 2
    | `Yellow -> 3
    | `Blue -> 4
    | `Magenta -> 5
    | `Cyan -> 6
    | `White -> 7

  type style =
    [ `FG of color (* foreground *)
    | `BG of color (* background *)
    | `Bold
    | `Reset ]

  let code_of_style : style -> int = function
    | `FG c -> 30 + int_of_color_ c
    | `BG c -> 40 + int_of_color_ c
    | `Bold -> 1
    | `Reset -> 0

  let string_of_style a = Printf.sprintf "\x1b[%dm" (code_of_style a)
  let clear_line = "\x1b[2K\r"
  let reset = string_of_style `Reset

  let string_of_style_list = function
    | [] -> reset
    | [ a ] -> string_of_style a
    | [ a; b ] ->
      Printf.sprintf "\x1b[%d;%dm" (code_of_style a) (code_of_style b)
    | [ a; b; c ] ->
      Printf.sprintf "\x1b[%d;%d;%dm" (code_of_style a) (code_of_style b)
        (code_of_style c)
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

  exception No_such_style

  (* parse a string tag. *)
  let style_of_tag_ s =
    match String.trim s with
    | "reset" -> [ `Reset ]
    | "black" -> [ `FG `Black ]
    | "red" -> [ `FG `Red ]
    | "green" -> [ `FG `Green ]
    | "yellow" -> [ `FG `Yellow ]
    | "blue" -> [ `FG `Blue ]
    | "magenta" -> [ `FG `Magenta ]
    | "cyan" -> [ `FG `Cyan ]
    | "white" -> [ `FG `White ]
    | "bold" -> [ `Bold ]
    | "Black" -> [ `FG `Black; `Bold ]
    | "Red" -> [ `FG `Red; `Bold ]
    | "Green" -> [ `FG `Green; `Bold ]
    | "Yellow" -> [ `FG `Yellow; `Bold ]
    | "Blue" -> [ `FG `Blue; `Bold ]
    | "Magenta" -> [ `FG `Magenta; `Bold ]
    | "Cyan" -> [ `FG `Cyan; `Bold ]
    | "White" -> [ `FG `White; `Bold ]
    | _ -> raise No_such_style
end

let color_enabled = ref false

let mark_open_style st style =
  Stack.push style st;
  if !color_enabled then
    ANSI_codes.string_of_style_list style
  else
    ""

let mark_close_style st : string =
  let style =
    try
      ignore (Stack.pop st);
      (* pop current style (if well-scoped …) *)
      Stack.top st
      (* look at previous style *)
    with Stack.Empty -> [ `Reset ]
  in
  if !color_enabled then
    ANSI_codes.string_of_style_list style
  else
    ""

[@@@ifge 4.8]

type stag += Style of ANSI_codes.style list

let pp_open_tag out s = pp_open_stag out (String_tag s)
let pp_close_tag out () = pp_close_stag out ()

(* either prints the tag of [s] or delegate to [or_else] *)
let mark_open_stag st ~or_else (tag : stag) : string =
  match tag with
  | Style style -> mark_open_style st style
  | String_tag s ->
    let open ANSI_codes in
    (try
       let style = style_of_tag_ s in
       mark_open_style st style
     with No_such_style -> or_else tag)
  | _ -> or_else tag

let mark_close_stag st ~or_else (tag : stag) : string =
  match tag with
  | Style _ -> mark_close_style st
  | String_tag s ->
    let open ANSI_codes in
    (* check if it's indeed about color *)
    (match style_of_tag_ s with
    | _ -> mark_close_style st
    | exception No_such_style -> or_else tag)
  | _ -> or_else tag

let with_styling stl out f =
  pp_open_stag out (Style stl);
  try
    let x = f () in
    pp_close_stag out ();
    x
  with e ->
    pp_close_stag out ();
    raise e

let styling stl pp out x = with_styling stl out @@ fun () -> pp out x

(* add color handling to formatter [ppf] *)
let set_color_tag_handling ppf =
  let st = Stack.create () in
  (* stack of styles *)
  pp_set_mark_tags ppf true;
  (* enable tags *)
  let funs = pp_get_formatter_stag_functions ppf () in
  let funs' =
    {
      funs with
      mark_open_stag = mark_open_stag st ~or_else:funs.mark_open_stag;
      mark_close_stag = mark_close_stag st ~or_else:funs.mark_close_stag;
    }
  in
  pp_set_formatter_stag_functions ppf funs'

[@@@else_]

(* either prints the tag of [s] or delegate to [or_else] *)
let mark_open_tag st ~or_else (s : string) : string =
  let open ANSI_codes in
  try
    let style = style_of_tag_ s in
    mark_open_style st style
  with No_such_style -> or_else s

let mark_close_tag st ~or_else (s : string) : string =
  let open ANSI_codes in
  (* check if it's indeed about color *)
  match style_of_tag_ s with
  | _ -> mark_close_style st
  | exception No_such_style -> or_else s

(* add color handling to formatter [ppf] *)
let set_color_tag_handling ppf =
  let st = Stack.create () in
  (* stack of styles *)
  pp_set_mark_tags ppf true;
  (* enable tags *)
  let funs = pp_get_formatter_tag_functions ppf () in
  let functions =
    {
      funs with
      mark_open_tag = mark_open_tag st ~or_else:funs.mark_open_tag;
      mark_close_tag = mark_close_tag st ~or_else:funs.mark_close_tag;
    }
  in
  pp_set_formatter_tag_functions ppf functions

[@@@endif]

let set_color_default =
  let first = ref true in
  fun b ->
    if b && not !color_enabled then (
      color_enabled := true;
      if !first then (
        first := false;
        set_color_tag_handling stdout;
        set_color_tag_handling stderr
      )
    ) else if (not b) && !color_enabled then
      color_enabled := false

let with_color s pp out x =
  pp_open_tag out s;
  pp out x;
  pp_close_tag out ()

let with_colorf s out fmt =
  pp_open_tag out s;
  Format.kfprintf (fun out -> pp_close_tag out ()) out fmt

(* c: whether colors are enabled *)
let sprintf_ c format =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  if c && !color_enabled then set_color_tag_handling fmt;
  Format.kfprintf
    (fun _fmt ->
      Format.pp_print_flush fmt ();
      Buffer.contents buf)
    fmt format

let with_color_ksf ~f s fmt =
  let buf = Buffer.create 64 in
  let out = Format.formatter_of_buffer buf in
  if !color_enabled then set_color_tag_handling out;
  pp_open_tag out s;
  Format.kfprintf
    (fun out ->
      pp_close_tag out ();
      Format.pp_print_flush out ();
      f (Buffer.contents buf))
    out fmt

let with_color_sf s fmt = with_color_ksf ~f:(fun s -> s) s fmt
let sprintf fmt = sprintf_ true fmt
let sprintf_no_color fmt = sprintf_ false fmt
let sprintf_dyn_color ~colors fmt = sprintf_ colors fmt

let fprintf_dyn_color ~colors out fmt =
  let old_tags = Format.pp_get_mark_tags out () in
  Format.pp_set_mark_tags out colors;
  (* enable/disable tags *)
  Format.kfprintf (fun out -> Format.pp_set_mark_tags out old_tags) out fmt

let ksprintf ?margin ~f fmt =
  let buf = Buffer.create 32 in
  let out = Format.formatter_of_buffer buf in
  if !color_enabled then set_color_tag_handling out;
  (match margin with
  | None -> ()
  | Some m -> pp_set_margin out m);
  Format.kfprintf
    (fun _ ->
      Format.pp_print_flush out ();
      f (Buffer.contents buf))
    out fmt

module Dump = struct
  type 'a t = 'a printer

  let unit = unit
  let int = int
  let string = string_quoted
  let bool = bool
  let float = float
  let char = char
  let int32 = int32
  let int64 = int64
  let nativeint = nativeint
  let list pp = within "[" "]" (hovbox (list ~sep:(return ";@,") pp))
  let array pp = within "[|" "|]" (hovbox (array ~sep:(return ";@,") pp))

  let option pp out x =
    match x with
    | None -> Format.pp_print_string out "None"
    | Some x -> Format.fprintf out "Some %a" pp x

  let pair p1 p2 = within "(" ")" (hovbox (pair p1 p2))
  let triple p1 p2 p3 = within "(" ")" (hovbox (triple p1 p2 p3))
  let quad p1 p2 p3 p4 = within "(" ")" (hovbox (quad p1 p2 p3 p4))

  let result' pok perror out = function
    | Ok x -> Format.fprintf out "(@[Ok %a@])" pok x
    | Error e -> Format.fprintf out "(@[Error %a@])" perror e

  let result pok = result' pok string
  let to_string = to_string
end

module Infix = struct
  let ( ++ ) = append
end

include Infix
