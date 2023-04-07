(** Pretty printing of documents.

    A document is a structured tree of text with
    formatting instructions.

    It can be rendered into a string ("pretty printed"),
    see {!Pretty}.

    This follows Wadler's paper "A prettier printer", but with
    some changes in the rendering part because we can't rely on lazyness
    to make the algebraic implementation efficient.

    Some general considerations: the type [t] is the type of documents,
    a tree with text leaves that is pretty printed within a given width.

    Layout is controlled via the combination of a few primitives:
    - [newline] will either print a space or a newline. It is similar
      to {!Format}'s ["@ "] in that sense. A big difference with [Format]
      is that by default [newline] is actually a newline. It only
      becomes a space if it's in a [group] small enough to fit
      in the remainder of the current line.
    - [group d] tries to write [d] on a single line if there's room.
      If not, it has no effect.
    - [nest n d] increases the indentation level inside [d]. Any newline
      that is rendered as a new line is indented by [n] more spaces (which
      are cumulative with surrounding [nest] calls).
    - [append a b] (or [a ^ b]) just prints [a] followed by [b].
    - [fill d] is a bit like [group] but it will try to cram
      as much as possible on each line. It is not all-or-nothing
      like [group].
*)

(** {2 Core} *)

type t
(** The type of documents *)

val nil : t
(** Empty document *)

val char : char -> t
(** Single char. *)

val text : string -> t
(** Text. The string will be split on ['\n'], which are replaced
    by {!newline}. *)

val textpf : ('a, unit, string, t) format4 -> 'a
(** Text, with a {!Printf}-compatible format.

    For example, [textpf "%d-%d" 4 2] is like [text "4-2"]. *)

val textf : ('a, Format.formatter, unit, t) format4 -> 'a
(** Text, with a {!Format}-compatible format.

    Note that this will bake-in any formatting done by {!Format}.
    Newlines introduced by format will become hard newlines
    in the resulting document. *)

val nest : int -> t -> t
(** [nest n d] increases indentation by [n] inside [d].
    If current indentation is [m], then every newline inside [d]
    will be followed by [n + m] leading spaces. *)

val group : t -> t
(** Group the documents inside this.

    Newlines immediately inside this group will either
    render as new lines or as spaces, depending on the width available. *)

val append : t -> t -> t
(** Concatenation. *)

val newline : t
(** A line break. *)

val nl : t
(** Alias for {!newline} *)

val fill : t -> t list -> t
(** [fill sep l] resembles [group (append_l ~sep l)], except it tries
    to put as many items of [l] as possible on each line.

    In terms of {!Format}, this is like the hov box. *)

(** {2 Output device} *)

(** Arbitrary output.

    This is used for user-provided output. *)
module Out : sig
  type t = {
    char: char -> unit;
        (** Output a single char. The char is assumed not to be ['\n']. *)
    sub_string: string -> int -> int -> unit;
        (** Output a string slice (optim for [string]) *)
    string: string -> unit;  (** Output a string *)
    newline: unit -> unit;  (** Output a newline *)
  }

  val of_buffer : Buffer.t -> t
  val char : t -> char -> unit
  val string : t -> string -> unit
  val sub_string : t -> string -> int -> int -> unit
  val newline : t -> unit
end

(** {2 Extensibility} *)

(** Extension node.

    Custom nodes can be used to add user-defined behavior to
    the rendered output. For example, documents
    might be annotated with ANSI-terminal colors, or
    with HTML tags. *)
module Ext : sig
  type 'a t = {
    pre: Out.t -> 'a -> unit;  (** Printed before the wrapped value. *)
    post: Out.t -> 'a -> unit;  (** Printed after the wrapped value. *)
  }
  (**  An extension is a custom document node. It takes a value of type ['a],
      and a document [d], and can output what it wants based
      on the custom value before and after [d] is printed.

      The extension is considered to have width [0]. *)
end

val ext : 'a Ext.t -> 'a -> t -> t
(** [ext e v d] wraps [d] with value [v].

    It is a document that has the same
    shape (and size) as [d], except that additional data will
    be output when it is rendered using extension [e].

    When this is rendered, first [e.pre out v] is called;
    then [d] is printed; then [e.post out v] is called.
    Here [out] is the output buffer/stream for rendering.

*)

(** {2 Pretty print and rendering} *)

(** Pretty-printing.

    These functions are parametrized by a width,
    and will try to fit the result within this width. *)
module Pretty : sig
  val to_out : width:int -> Out.t -> t -> unit
  (** Render to an arbitrary output. *)

  val to_string : width:int -> t -> string
  (** Render to a string. *)

  val to_buffer : width:int -> Buffer.t -> t -> unit
  (** Render to a buffer. *)

  val to_format : width:int -> Format.formatter -> t -> unit
end

(** Trivial printing, on a single line.

    This is generally ugly, but it's simple and fast when we do not
    care about looks. *)
module Flatten : sig
  val to_out : Out.t -> t -> unit
  val to_buffer : Buffer.t -> t -> unit
  val to_string : t -> string
end

val pp : Format.formatter -> t -> unit
(** Pretty-print, using {!Pretty} and an unspecified margin. *)

val debug : Format.formatter -> t -> unit
(** Debug printer. This prints the structure of the document,
    it does {b not} pretty-print it. See {!pp} or {!Pretty}. *)

(** {2 Convenience functions} *)

module Infix : sig
  val ( ^ ) : t -> t -> t
  (** Alias of {!append}. *)

  val ( ^+ ) : t -> t -> t
  (** [x ^+ y] is [x ^ text " " ^ y] *)

  val ( ^/ ) : t -> t -> t
  (** [x ^/ y] is [x ^ newline ^ y] *)
end

include module type of Infix

val sp : t
(** A single space *)

val append_l : ?sep:t -> t list -> t
(** [append_l ?sep l] is the concatenation of elements of
    [l], separated by [sep] (default [nil]) *)

val append_sp : t list -> t
(** [append_sp l] is the concatenation of elements of [l], separated by [' '] *)

val append_nl : t list -> t
(** Same as {!append_l} with [sep=nl] *)

val fill_map : t -> ('a -> t) -> 'a list -> t
(** [fill_map sep f l] is [fill sep (List.map f l)] *)

val bool : bool -> t
val int : int -> t
val float : float -> t
val float_hex : float -> t

val text_quoted : string -> t
(** [text_quoted s] is [text (spf "%S" s)] *)

val text_zero_width : string -> t
(** Text but we assume it takes no space on screen. *)

val of_list : ?sep:t -> ('a -> t) -> 'a list -> t
(** [of_list f l] maps each element of [l] to a document
    and concatenates them.
    @param sep separator inserted between elements (default [nil]) *)

val of_seq : ?sep:t -> ('a -> t) -> 'a Seq.t -> t
(** Same as {!of_list} but with sequences. *)

val bracket : string -> t -> string -> t
(** [bracket l d r] groups [d], between brackets [l] and [r] *)

val bracket2 : string -> t -> string -> t
(** [bracket2 l d r] groups [d], indented by 2, between brackets [l] and [r] *)

val sexp_apply : string -> t list -> t
(** [sexp_apply a l] is the S-expr ["(text a …l)"], pretty-printed *)

val sexp_l : t list -> t
(** [sexp_l [l1;…ln]] is the S-expr ["(l1 l2…ln)"], pretty-printed *)

(** Printers that correspond closely to OCaml's syntax. *)
module Dump : sig
  val list : t list -> t
end

(** Simple colors in terminals *)
module Term_color : sig
  type color =
    [ `Black
    | `Blue
    | `Cyan
    | `Green
    | `Magenta
    | `Red
    | `White
    | `Yellow
    ]

  type style =
    [ `BG of color
    | `Bold
    | `FG of color
    | `Reset
    | `Underline
    ]

  val color : color -> t -> t

  val style_l : style list -> t -> t
end
