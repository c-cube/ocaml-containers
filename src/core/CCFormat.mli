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

val pair : 'a printer -> 'b printer -> ('a * 'b) printer
val triple : 'a printer -> 'b printer -> 'c printer -> ('a * 'b * 'c) printer
val quad : 'a printer -> 'b printer -> 'c printer -> 'd printer -> ('a * 'b * 'c * 'd) printer

val map : ('a -> 'b) -> 'b printer -> 'a printer

(** {2 ASCII codes}

  Use ANSI escape codes https://en.wikipedia.org/wiki/ANSI_escape_code
  to put some colors on the terminal.

  This uses {b tags} in format strings to specify  the style. Current styles
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
  @since NEXT_RELEASE *)

val set_color_tag_handling : t -> unit
(** adds functions to support color tags to the given formatter.
    @since NEXT_RELEASE *)

val set_color_default : bool -> unit
(** [set_color_default b] enables color handling on the standard formatters
    (stdout, stderr) if [b = true] as well as on {!sprintf} formatters;
    it disables the color handling if [b = false]. *)

(** {2 IO} *)

val output : t -> 'a printer -> 'a -> unit
val to_string : 'a printer -> 'a -> string

val stdout : t
val stderr : t

val sprintf : ('a, t, unit, string) format4 -> 'a
(** Print into a string any format string that would usually be compatible
    with {!fprintf}. Similar to {!Format.asprintf}. *)

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
