(*
copyright (c) 2013, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Helpers for Format} *)

type 'a sequence = ('a -> unit) -> unit

type t = Format.formatter
type 'a printer = t -> 'a -> unit

(** {2 Combinators} *)

let silent _fmt _ = ()

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

let list ?(start="[") ?(stop="]") ?(sep=", ") pp fmt l =
  let rec pp_list l = match l with
  | x::((_::_) as l) ->
    pp fmt x;
    Format.pp_print_string fmt sep;
    Format.pp_print_cut fmt ();
    pp_list l
  | x::[] -> pp fmt x
  | [] -> ()
  in
  Format.pp_print_string fmt start;
  pp_list l;
  Format.pp_print_string fmt stop

let array ?(start="[") ?(stop="]") ?(sep=", ") pp fmt a =
  Format.pp_print_string fmt start;
  for i = 0 to Array.length a - 1 do
    if i > 0 then (
      Format.pp_print_string fmt sep;
      Format.pp_print_cut fmt ();
    );
    pp fmt a.(i)
  done;
  Format.pp_print_string fmt stop

let arrayi ?(start="[") ?(stop="]") ?(sep=", ") pp fmt a =
  Format.pp_print_string fmt start;
  for i = 0 to Array.length a - 1 do
    if i > 0 then (
      Format.pp_print_string fmt sep;
      Format.pp_print_cut fmt ();
    );
    pp fmt (i, a.(i))
  done;
  Format.pp_print_string fmt stop

let seq ?(start="[") ?(stop="]") ?(sep=", ") pp fmt seq =
  Format.pp_print_string fmt start;
  let first = ref true in
  seq (fun x ->
    (if !first then first := false else (
      Format.pp_print_string fmt sep;
      Format.pp_print_cut fmt ();
    ));
    pp fmt x);
  Format.pp_print_string fmt stop

let opt pp fmt x = match x with
  | None -> Format.pp_print_string fmt "none"
  | Some x -> Format.fprintf fmt "some %a" pp x

let pair ppa ppb fmt (a, b) =
  Format.fprintf fmt "(%a,@ %a)" ppa a ppb b

let triple ppa ppb ppc fmt (a, b, c) =
  Format.fprintf fmt "(%a,@ %a,@ %a)" ppa a ppb b ppc c

let quad ppa ppb ppc ppd fmt (a, b, c, d) =
  Format.fprintf fmt "(%a,@ %a,@ %a,@ %a)" ppa a ppb b ppc c ppd d

let map f pp fmt x =
  pp fmt (f x);
  ()

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

let _with_file_out filename f =
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  begin try
    let x = f fmt in
    Format.pp_print_flush fmt ();
    close_out oc;
    x
  with e ->
    Format.pp_print_flush fmt ();
    close_out_noerr oc;
    raise e
  end

let to_file filename format =
  _with_file_out filename (fun fmt -> Format.fprintf fmt format)

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
  | `Reset
  ]

let code_of_style : style -> int = function
  | `FG c -> 30 + int_of_color_ c
  | `BG c -> 40 + int_of_color_ c
  | `Bold -> 1
  | `Reset -> 0

let ansi_l_to_str_ = function
  | [] -> "\x1b[0m"
  | [a] -> Format.sprintf "\x1b[%dm" (code_of_style a)
  | [a;b] -> Format.sprintf "\x1b[%d;%dm" (code_of_style a) (code_of_style b)
  | l ->
      let pp_num out c = int out (code_of_style c) in
      to_string (list ~start:"\x1b[" ~stop:"m" ~sep:";" pp_num) l

(* parse a tag *)
let style_of_tag_ s = match String.trim s with
  | "reset" -> [`Reset]
  | "black" -> [`FG `Black]
  | "red" -> [`FG `Red]
  | "green" -> [`FG `Green]
  | "yellow" -> [`FG `Yellow]
  | "blue" -> [`FG `Blue]
  | "magenta" -> [`FG `Magenta]
  | "cyan" -> [`FG `Cyan]
  | "white" -> [`FG `White]
  | "Black" -> [`FG `Black]
  | "Red" -> [`FG `Red; `Bold]
  | "Green" -> [`FG `Green; `Bold]
  | "Yellow" -> [`FG `Yellow; `Bold]
  | "Blue" -> [`FG `Blue; `Bold]
  | "Magenta" -> [`FG `Magenta; `Bold]
  | "Cyan" -> [`FG `Cyan; `Bold]
  | "White" -> [`FG `White; `Bold]
  | s -> failwith ("unknown style: " ^ s)

let color_enabled = ref false

(* either prints the tag of [s] or delegate to [or_else] *)
let mark_open_tag st ~or_else s =
  try
    let style = style_of_tag_ s in
    Stack.push style st;
    if !color_enabled then ansi_l_to_str_ style else ""
  with Not_found -> or_else s

let mark_close_tag st ~or_else s =
  try
    let _ = style_of_tag_ s in (* check if it's indeed about color *)
    let style =
      try
        ignore (Stack.pop st); (* pop current style (if well-scoped...) *)
        Stack.top st (* look at previous style *)
      with Stack.Empty ->
        [`Reset]
    in
    if !color_enabled then ansi_l_to_str_ style else ""
  with Not_found -> or_else s

(* add color handling to formatter [ppf] *)
let set_color_tag_handling ppf =
  let open Format in
  let functions = pp_get_formatter_tag_functions ppf () in
  let st = Stack.create () in (* stack of styles *)
  let functions' = {functions with
    mark_open_tag=(mark_open_tag st ~or_else:functions.mark_open_tag);
    mark_close_tag=(mark_close_tag st ~or_else:functions.mark_close_tag);
  } in
  pp_set_mark_tags ppf true; (* enable tags *)
  pp_set_formatter_tag_functions ppf functions'

let set_color_default =
  let first = ref true in
  fun b ->
    if b && not !color_enabled then (
      color_enabled := true;
      if !first then (
        first := false;
        set_color_tag_handling stdout;
        set_color_tag_handling stderr;
      );
    ) else if not b && !color_enabled then color_enabled := false

(*$R
  set_color_default true;
  let s = sprintf
    "what is your @{<White>favorite color@}? @{<blue>blue@}! No, @{<red>red@}! Ahhhhhhh@."
  in
  assert_equal ~printer:CCFun.id
    "what is your \027[37;1mfavorite color\027[0m? \027[34mblue\027[0m! No, \027[31mred\027[0m! Ahhhhhhh\n"
    s
*)

let sprintf format =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  if !color_enabled then set_color_tag_handling fmt;
  Format.kfprintf
    (fun _fmt -> Format.pp_print_flush fmt (); Buffer.contents buf)
    fmt
    format

(*$T
  sprintf "yolo %s %d" "a b" 42 = "yolo a b 42"
  sprintf "%d " 0 = "0 "
*)

let ksprintf ~f fmt =
  let buf = Buffer.create 32 in
  let out = Format.formatter_of_buffer buf in
  if !color_enabled then set_color_tag_handling out;
  Format.kfprintf
    (fun _ -> Format.pp_print_flush out (); f (Buffer.contents buf))
    out fmt
