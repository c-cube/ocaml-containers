
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Helpers for Format}

    @since 0.8 *)

type 'a iter = ('a -> unit) -> unit

(* include Format, and alias all its types.
   see https://discuss.ocaml.org/t/extend-existing-module/1389/4
*)
include module type of struct include Format end

type t = Format.formatter
type 'a printer = t -> 'a -> unit

(** {2 Combinators} *)

val silent : 'a printer
(** Prints nothing. *)

val unit : unit printer
(** Prints "()". *)

val int : int printer
val string : string printer
val bool : bool printer
val float3 : float printer (* 3 digits after . *)
val float : float printer

val exn : exn printer
(** Printer using {!Printexc.to_string}.
    @since NEXT_RELEASE *)

val newline : unit printer
(** Force newline (see {!Format.pp_force_newline}).
    @since 1.2 *)

val substring : (string * int * int) printer
(** [substring (s,i,len)] prints the substring [(s,i,len)], where [i] is the offset
    in [s] and [len] the number of bytes in the substring.
    @raise Invalid_argument if the triple [(s,i,len)] does not
    describe a proper substring.
    @since 1.2 *)

val text : string printer
(** Print string, but replacing spaces with breaks and newlines
    with {!newline}.
    See [pp_print_text] on recent versions of OCaml.
    @since 1.2 *)

val char : char printer (** @since 0.14 *)

val int32 : int32 printer (** @since 0.14 *)

val int64 : int64 printer (** @since 0.14 *)

val nativeint : nativeint printer (** @since 0.14 *)

val flush : unit printer
(** Alias to {!Format.pp_print_flush}.
    @since 1.2 *)

val string_quoted : string printer
(** Similar to {!CCString.print}.
    @since 0.14 *)

val list : ?sep:unit printer -> 'a printer -> 'a list printer
val array : ?sep:unit printer -> 'a printer -> 'a array printer
val arrayi : ?sep:unit printer -> (int * 'a) printer -> 'a array printer
val seq : ?sep:unit printer -> 'a printer -> 'a Seq.t printer
val iter : ?sep:unit printer -> 'a printer -> 'a iter printer

val opt : 'a printer -> 'a option printer
(** [opt pp] prints options as follows:
    - [Some x] will become "some foo" if [pp x ---> "foo"].
    - [None] will become "none". *)

(** In the tuple printers, the [sep] argument is only available.
    @since 0.17 *)

val pair : ?sep:unit printer -> 'a printer -> 'b printer -> ('a * 'b) printer
val triple : ?sep:unit printer -> 'a printer -> 'b printer -> 'c printer -> ('a * 'b * 'c) printer
val quad : ?sep:unit printer -> 'a printer -> 'b printer ->
  'c printer -> 'd printer -> ('a * 'b * 'c * 'd) printer

val within : string -> string -> 'a printer -> 'a printer
(** [within a b p] wraps [p] inside the strings [a] and [b]. Convenient,
    for instances, for brackets, parenthesis, quotes, etc.
    @since 0.17 *)

val map : ('a -> 'b) -> 'b printer -> 'a printer

val vbox : ?i:int -> 'a printer -> 'a printer
(** Wrap the printer in a vertical box.
    @param i level of indentation within the box (default 0).
    @since 0.16 *)

val hvbox : ?i:int -> 'a printer -> 'a printer
(** Wrap the printer in a horizontal/vertical box.
    @param i level of indentation within the box (default 0).
    @since 0.16 *)

val hovbox : ?i:int -> 'a printer -> 'a printer
(** Wrap the printer in a horizontal or vertical box.
    @param i level of indentation within the box (default 0).
    @since 0.16 *)

val hbox : 'a printer -> 'a printer
(** Wrap the printer in an horizontal box.
    @since 0.16 *)

val return : ('a, _, _, 'a) format4 -> unit printer
(** [return "some_format_string"] takes a argument-less format string
    and returns a printer actionable by [()].
    Examples:
    - [return ",@ "]
    - [return "@{<Red>and then@}@,"]
    - [return "@[<v>a@ b@]"]

    @since 1.0
*)

val of_to_string : ('a -> string) -> 'a printer
(** [of_to_string f] converts its input to a string using [f],
    then prints the string.
    @since 1.0 *)

val const : 'a printer -> 'a -> unit printer
(** [const pp x] is a unit printer that uses [pp] on [x].
    @since 1.0 *)

val some : 'a printer -> 'a option printer
(** [some pp] will print options as follows:
    - [Some x] is printed using [pp] on [x]
    - [None] is not printed at all
    @since 1.0
*)

val lazy_force : 'a printer -> 'a lazy_t printer
(** [lazy_force pp out x] forces [x] and prints the result with [pp].
    @since 2.0 *)

val lazy_or : ?default:unit printer -> 'a printer -> 'a lazy_t printer
(** [lazy_or ?default pp out x] prints [default] if [x] is not
    evaluated yet, or uses [pp] otherwise.
    @since 2.0 *)

(** {2 ANSI codes}

    Use ANSI escape codes https://en.wikipedia.org/wiki/ANSI_escape_code
    to put some colors on the terminal.

    This uses {b tags} in format strings to specify the style. Current styles
    are the following:

    {ul
    {- "reset" resets style}
    {- "black" }
    {- "red" }
    {- "green" }
    {- "yellow" }
    {- "blue" }
    {- "magenta" }
    {- "cyan" }
    {- "white" }
    {- "bold" bold font}
    {- "Black" bold black}
    {- "Red" bold red }
    {- "Green" bold green }
    {- "Yellow" bold yellow }
    {- "Blue" bold blue }
    {- "Magenta" bold magenta }
    {- "Cyan" bold cyan }
    {- "White" bold white }
    }

    Example:

    {[
      set_color_default true;;

      Format.printf
        "what is your @{<White>favorite color@}? @{<blue>blue@}! No, @{<red>red@}! Ahhhhhhh@.";;
    ]}

    {b status: unstable}
    @since 0.15 *)

val set_color_tag_handling : t -> unit
(** Add functions to support color tags to the given formatter.
    @since 0.15 *)

val set_color_default : bool -> unit
(** [set_color_default b] enables color handling on the standard formatters
    (stdout, stderr) if [b = true] as well as on {!sprintf} formatters;
    it disables the color handling if [b = false]. *)

val with_color : string -> 'a printer -> 'a printer
(** [with_color "Blue" pp] behaves like the printer [pp], but with the given
    style.

    {b status: unstable}
    @since 0.16 *)

val with_colorf : string -> t -> ('a, t, unit, unit) format4 -> 'a
(** [with_colorf "Blue" out "%s %d" "yolo" 42] will behave like {!Format.fprintf},
    but wrapping the content with the given style.

    {b status: unstable}
    @since 0.16 *)

val with_color_sf : string -> ('a, t, unit, string) format4 -> 'a
(** [with_color_sf "Blue" out "%s %d" "yolo" 42] will behave like
    {!sprintf}, but wrapping the content with the given style.

    Example:
    {[
      CCFormat.with_color_sf "red" "%a" CCFormat.Dump.(list int) [1;2;3] |> print_endline;;
    ]}

    {b status: unstable}
    @since 0.21 *)

val with_color_ksf : f:(string -> 'b) -> string -> ('a, t, unit, 'b) format4 -> 'a
(** [with_color_ksf "Blue" ~f "%s %d" "yolo" 42] will behave like
    {!ksprintf}, but wrapping the content with the given style.

    Example:
    the following with raise [Failure] with a colored message
    {[
      CCFormat.with_color_ksf "red" ~f:failwith "%a" CCFormat.Dump.(list int) [1;2;3];;
    ]}
    @since 1.2 *)

(** {2 IO} *)

val output : t -> 'a printer -> 'a -> unit
val to_string : 'a printer -> 'a -> string

val of_chan : out_channel -> t
(** Alias to {!Format.formatter_of_out_channel}.
    @since 1.2 *)

val with_out_chan : out_channel -> (t -> 'a) -> 'a
(** [with_out_chan oc f] turns [oc] into a formatter [fmt], and call [f fmt].
    Behaves like [f fmt] from then on, but whether the call to [f] fails
    or returns, [fmt] is flushed before the call terminates.
    @since 1.2 *)

val stdout : t
val stderr : t

val tee : t -> t -> t
(** [tee a b] makes a new formatter that writes in both [a] and [b].
    @since 1.0 *)

val sprintf : ('a, t, unit, string) format4 -> 'a
(** Print into a string any format string that would usually be compatible
    with {!fprintf}. Like {!Format.asprintf}. *)

val sprintf_no_color : ('a, t, unit, string) format4 -> 'a
(** Like {!sprintf} but never prints colors.
    @since 0.16 *)

val sprintf_dyn_color : colors:bool -> ('a, t, unit, string) format4 -> 'a
(** Like {!sprintf} but enable/disable colors depending on [colors].

    Example:
    {[
      (* with colors *)
      CCFormat.sprintf_dyn_color ~colors:true "@{<Red>%a@}"
        CCFormat.Dump.(list int) [1;2;3] |> print_endline;;

      (* without colors *)
      CCFormat.sprintf_dyn_color ~colors:false "@{<Red>%a@}"
        CCFormat.Dump.(list int) [1;2;3] |> print_endline;;
    ]}
    @since 0.21 *)

val fprintf : t -> ('a, t, unit ) format -> 'a
(** Alias to {!Format.fprintf}.
    @since 0.14 *)

val fprintf_dyn_color : colors:bool -> t -> ('a, t, unit ) format -> 'a
(** Like {!fprintf} but enable/disable colors depending on [colors].
    @since 0.21 *)

val ksprintf :
  ?margin:int ->
  f:(string -> 'b) ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a
(** [ksprintf fmt ~f] formats using [fmt], in a way similar to {!sprintf},
    and then calls [f] on the resulting string.
    @param margin set margin (since 2.1)
    @since 0.14 *)

val to_file : string -> ('a, t, unit, unit) format4 -> 'a
(** Print to the given file. *)

(** {2 Dump}

    Print structures as OCaml values, so that they can be parsed back
    by OCaml (typically, in the toplevel, for debugging).

    Example:
    {[
      Format.printf "%a@." CCFormat.Dump.(list int) CCList.(1 -- 200);;

      Format.printf "%a@." CCFormat.Dump.(array (list (pair int bool)))
        [| [1, true; 2, false]; []; [42, false] |];;
    ]}

    @since 0.21 *)

module Dump : sig
  type 'a t = 'a printer
  val unit : unit t
  val int : int t
  val string : string t
  val bool : bool t
  val float : float t
  val char : char t
  val int32 : int32 t
  val int64 : int64 t
  val nativeint : nativeint t
  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t
  val option : 'a t -> 'a option t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad :
    'a t -> 'b t -> 'c t -> 'd t ->
    ('a * 'b * 'c * 'd) t
  val result : 'a t -> ('a, string) result t
  val result' : 'a t -> 'e t -> ('a, 'e) result t
  val to_string : 'a t -> 'a -> string
  (** Alias to {!CCFormat.to_string}. *)
end
