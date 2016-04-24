
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Helpers for Format}

@since 0.8 *)

type 'a sequence = ('a -> unit) -> unit

type t = Format.formatter
type 'a printer = t -> 'a -> unit

(** {2 Combinators} *)

val silent : 'a printer (** Prints nothing *)

val unit : unit printer
val int : int printer
val string : string printer
val bool : bool printer
val float3 : float printer (* 3 digits after . *)
val float : float printer

val char : char printer (** @since 0.14 *)
val int32 : int32 printer (** @since 0.14 *)
val int64 : int64 printer (** @since 0.14 *)
val nativeint : nativeint printer (** @since 0.14 *)

val string_quoted : string printer
(** Similar to {!CCString.print}.
    @since 0.14 *)

val list : ?start:string -> ?stop:string -> ?sep:string -> 'a printer -> 'a list printer
val array : ?start:string -> ?stop:string -> ?sep:string -> 'a printer -> 'a array printer
val arrayi : ?start:string -> ?stop:string -> ?sep:string ->
            (int * 'a) printer -> 'a array printer
val seq : ?start:string -> ?stop:string -> ?sep:string -> 'a printer -> 'a sequence printer

val opt : 'a printer -> 'a option printer
(** [opt pp] prints options as follows:
    [Some x] will become "some foo" if [pp x ---> "foo"]
    [None] will become "none" *)

(** In the tuple printers, the [sep] argument is only available
    @since 0.17 *)

val pair : ?sep:string -> 'a printer -> 'b printer -> ('a * 'b) printer
val triple : ?sep:string -> 'a printer -> 'b printer -> 'c printer -> ('a * 'b * 'c) printer
val quad : ?sep:string -> 'a printer -> 'b printer ->
            'c printer -> 'd printer -> ('a * 'b * 'c * 'd) printer

val within : string -> string -> 'a printer -> 'a printer
(** [within a b p] wraps [p] inside the strings [a] and [b]. Convenient,
    for instances, for brackets, parenthesis, quotes, etc.
    @since 0.17 *)

val map : ('a -> 'b) -> 'b printer -> 'a printer

val vbox : ?i:int -> 'a printer -> 'a printer
(** Wrap the printer in a vertical box
    @param i level of indentation within the box (default 0)
    @since 0.16 *)

val hvbox : ?i:int -> 'a printer -> 'a printer
(** Wrap the printer in a horizontal/vertical box
    @param i level of indentation within the box (default 0)
    @since 0.16 *)

val hovbox : ?i:int -> 'a printer -> 'a printer
(** Wrap the printer in a horizontal or vertical box
    @param i level of indentation within the box (default 0)
    @since 0.16 *)

val hbox : 'a printer -> 'a printer
(** Wrap the printer in an horizontal box
    @since 0.16 *)

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

  {b status: experimental}
  @since 0.15 *)

val set_color_tag_handling : t -> unit
(** adds functions to support color tags to the given formatter.
    @since 0.15 *)

val set_color_default : bool -> unit
(** [set_color_default b] enables color handling on the standard formatters
    (stdout, stderr) if [b = true] as well as on {!sprintf} formatters;
    it disables the color handling if [b = false]. *)

val with_color : string -> 'a printer -> 'a printer
(** [with_color "Blue" pp] behaves like the printer [pp], but with the given
    style.
    {b status: experimental}
    @since 0.16 *)

val with_colorf : string -> t -> ('a, t, unit, unit) format4 -> 'a
(** [with_colorf "Blue" out "%s %d" "yolo" 42] will behave like {!Format.fprintf},
    but wrapping the content with the given style
    {b status: experimental}
    @since 0.16 *)

(** {2 IO} *)

val output : t -> 'a printer -> 'a -> unit
val to_string : 'a printer -> 'a -> string

val stdout : t
val stderr : t

val sprintf : ('a, t, unit, string) format4 -> 'a
(** Print into a string any format string that would usually be compatible
    with {!fprintf}. Similar to {!Format.asprintf}. *)

val sprintf_no_color : ('a, t, unit, string) format4 -> 'a
(** Similar to {!sprintf} but never prints colors
    @since 0.16 *)

val fprintf : t -> ('a, t, unit ) format -> 'a
(** Alias to {!Format.fprintf}
    @since 0.14 *)

val ksprintf :
  f:(string -> 'b) ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a
(** [ksprintf fmt ~f] formats using [fmt], in a way similar to {!sprintf},
    and then calls [f] on the resulting string.
    @since 0.14 *)

(*$= & ~printer:CCFormat.(to_string (opt string))
  (Some "hello world") \
    (ksprintf "hello %a" CCFormat.string "world" ~f:(fun s -> Some s))
*)

val to_file : string -> ('a, t, unit, unit) format4 -> 'a
(** Print to the given file *)
