(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bit Field}

  This module defines efficient bitfields
  up to 30 or 62 bits (depending on the architecture) in
  a relatively type-safe way.

{[
module B = CCBitField.Make(struct end);;

#install_printer B.pp;;

module X = (val B.int ~name:"x" ~width:3 ());;
module Y = (val B.int ~name:"y" ~width:2 ());;
module Z = (val B.bool ~name:"z" ());;

let f = B.empty |> X.set 3 |> Y.set 1;;

Z.get f ;;

f |> Z.set true |> Z.get ;;

Format.printf "f: %a@." B.pp f;;

]}

{b status: experimental}

@since 0.13
*)

exception TooManyFields
(** Raised when too many fields are packed into one bitfield *)

exception Frozen
(** Raised when a frozen bitfield is modified *)

module type EMPTY = sig end
(** Used for generativity on versions of OCaml older than 4.02 *)

val max_width : int
(** System-dependent maximum width for a bitfield, typically 30 or 62 *)

(** {2 Bitfield Signature} *)
module type S = sig
  type t = private int
  (** Generative type of bitfields. Each instantiation of the functor
      should create a new, incompatible type *)

  val empty : t
  (** Empty bitfields (all bits 0) *)

  type _ field_kind =
    | Bool : bool field_kind
    | Int : int field_kind

  (** Field of type [value], with a given width and position within the
      bitfield type *)
  module type FIELD = sig
    type value
    (** Values contained in the field *)

    val get : t -> value

    val set : value -> t -> t

    val width : int

    val name : string

    val kind : value field_kind
  end

  type 'a field = (module FIELD with type value = 'a)

  val bool : ?name:string -> unit -> bool field
  (** New field of type bool
      @raise Frozen if [freeze ()] was called
      @raise TooManyFields if there is no room *)

  val int2 : ?name:string -> unit -> int field
  (** New field of type 2-bits int (same as [int ~width:2])
      @raise Frozen if [freeze ()] was called
      @raise TooManyFields if there is no room *)

  val int3 : ?name:string -> unit -> int field
  (** New field of type 3-bits int (same as [int ~width:3])
      @raise Frozen if [freeze ()] was called
      @raise TooManyFields if there is no room *)

  val int : ?name:string -> width:int -> unit -> int field
  (** New field for [width] bits.
      @raise Frozen if [freeze ()] was called
      @raise Invalid_argument if width is not [<= 1]
      @raise TooManyFields if there is no room *)

  val freeze : unit -> unit
  (** Prevent new fields from being added. From now on, creating
      a field will raise Frozen *)

  val total_width : unit -> int
  (** Current width of the bitfield *)

  type any_field = AnyField : (module FIELD with type value = 'a) * 'a field_kind -> any_field

  val iter_fields : (any_field -> unit) -> unit
  (** Iterate on all currently present fields *)

  val pp : Format.formatter -> t -> unit
  (** Print the bitfield using the current list of fields *)
end

(** Create a new bitfield type *)
module Make(X : EMPTY) : S

(*$R
  let module B = CCBitField.Make(struct end) in

  let module X = (val B.bool ()) in
  let module Y = (val B.int2 ()) in
  let module Z = (val B.bool ()) in
  let module U = (val B.int ~width:4 ()) in

  assert_equal 2 Y.width ;
  assert_equal 4 U.width ;

  let f = B.empty
  |> Y.set 3
  |> Z.set true
  in

  assert_equal 14 (f :> int) ;

  assert_equal false (X.get f) ;
  assert_equal 3 (Y.get f) ;
  assert_equal true (Z.get f);

  let f' = U.set 13 f in

  assert_equal false (X.get f') ;
  assert_equal 3 (Y.get f') ;
  assert_equal true (Z.get f');
  assert_equal 13 (U.get f');

  ()
*)


(**/**)

val all_bits_ : int -> int -> int
(** Undocumented, do not use. Exposed for testing purpose *)

(**/**)
