(** Pretty printing of documents.

    A document is a structured tree of text with
    formatting instructions.

    It can be rendered into a string ("pretty printed"),
    see {!Pretty}.

    This follows Wadler's paper "A prettier printer", but with
    some changes in the rendering part because we can't rely on lazyness
    to make the algebraic implementation efficient.
*)

(** {2 Core} *)

type t
(** The type of documents *)

val nil : t
(** Empty document *)

val char : char -> t
(** Single char. *)

val text : string -> t
(** Text. The string will be split on ['\n']. *)

val textpf : ('a, unit, string, t) format4 -> 'a
(** Text, with a {!Printf}-compatible format.

    For example, [textpf "%d-%d" 4 2] is like [text "4-2"]. *)

val textf : ('a, Format.formatter, unit, t) format4 -> 'a
(** Text, with a {!Format}-compatible format.

    Note that this will bake-in any formatting done by {!Format}.
    Newlines introduced by format will become hard newlines
    in the resulting document. *)

val nest : int -> t -> t
(** Increase indentation by [n]. *)

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

(** Extension node.

    In here, we can stuff custom printer nodes. *)
module Ext : sig
  module type OUT = sig
    val char : char -> unit
    val sub_string : string -> int -> int -> unit
    val string : string -> unit
    val newline : unit -> unit
  end

  type out = (module OUT)

  module type S = sig
    type t

    val pre : out -> t -> unit
    val post : out -> t -> unit
  end

  type 'a t = (module S with type t = 'a)
end

val wrap : 'a Ext.t -> 'a -> t -> t
(** [wrap ext v d] wraps [d] with value [v].

    It is a document that has the same
    shape (and size) as [d], except that additional data will
    be output when it is rendered.

    Let [(module Ext)] be [ext], and [out]
    be the output buffer/stream for rendering.

    When this is rendered, first [Ext.pre out v] is called;
    then [d] is printed; then [Exp.post out v] is called.
*)

(** {2 Pretty print and rendering} *)

(** Pretty-printing.

    These functions are parametrized by a width,
    and will try to fit the result within this width. *)
module Pretty : sig
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
  val to_buffer : Buffer.t -> t -> unit
  val to_string : t -> string
end

val pp : Format.formatter -> t -> unit

(** {2 Convenience functions *)

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

val bool : bool -> t
val int : int -> t
val float : float -> t
val float_hex : float -> t

val of_list : ?sep:t -> ('a -> t) -> 'a list -> t
(** [of_list f l] maps each element of [l] to a document
    and concatenates them.
    @param sep separator inserted between elements (default [nil]) *)

val bracket : string -> t -> string -> t
(** [bracket l d r] groups [d], indented, between brackets [l] and [r] *)

val sexp_apply : string -> t list -> t
(** [sexp_apply a l] is the S-expr ["(text a …l)"], pretty-printed *)

val sexp_l : t list -> t
(** [sexp_l [l1;…ln]] is the S-expr ["(l1 l2…ln)"], pretty-printed *)

(** Printers that correspond closely to OCaml's syntax. *)
module Dump : sig
  val list : t list -> t
end
