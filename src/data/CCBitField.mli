(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bit Field}

  This module defines efficient bitfields
  up to 30 or 62 bits (depending on the architecture) in
  a relatively type-safe way.

  {b status: experimental}
  @since NEXT_RELEASE *)

exception TooManyFields
(** Raised when too many fields are packed into one bitfield *)

exception Frozen
(** Raised when a frozen bitfield is modified *)

module type EMPTY = sig end

val max_width : int
(** System-dependent maximum width for a bitfield *)

(** {2 Bitfield Signature} *)
module type BITFIELD = sig
  type t = private int

  val empty : t

  type 'a field

  val get : 'a field -> t -> 'a
  (** Get a field of type ['a] *)

  val set : 'a field -> 'a -> t -> t
  (** Set a field of type ['a] *)

  val width : _ field -> int
  (** Number of bits of the field *)

  val name : _ field -> string
  (** Informal name of the field *)

  val bool : ?name:string -> unit -> bool field
  (** New field of type boo
      @raise Frozen if [freeze ()] was called
      @raise TooManyFields if there is no room *)

  val int2 : ?name:string -> unit -> int field
  (** New field of type 2-bits int
      @raise Frozen if [freeze ()] was called
      @raise TooManyFields if there is no room *)

  val int3 : ?name:string -> unit -> int field
  (** New field for 3-bits int
      @raise Frozen if [freeze ()] was called
      @raise TooManyFields if there is no room *)

  val int : ?name:string -> width:int -> int field
  (** New field for [width] bits.
      @raise Frozen if [freeze ()] was called
      @raise TooManyFields if there is no room *)

  val freeze : unit -> unit
  (** Prevent new fields from being added *)

  val total_width : unit -> int
  (** Current width of the bitfield *)

  type any_field = AnyField : 'a field -> any_field

  val iter_fields : (any_field -> unit) -> unit
  (** Iterate on all currently present fields *)

  val pp : Format.formatter -> t -> unit
  (** Print the bitfield using the current list of fields *)
end

(** Create a new bitfield type *)
module Make(X : EMPTY) : BITFIELD

(*$R
  let module B = CCBitField.Make(struct end) in

  let x = B.bool () in
  let y = B.int2 () in
  let z = B.bool () in
  let u = B.int 4 in

  assert_equal 2 (B.width y) ;
  assert_equal 4 (B.width u) ;

  let f = B.empty
  |> B.set y 3
  |> B.set z true
  in

  assert_equal 14 (f :> int) ;

  assert_equal false (B.get x f) ;
  assert_equal 3 (B.get y f) ;
  assert_equal true (B.get z f);

  let f' = B.set u 13 f in

  assert_equal false (B.get x f') ;
  assert_equal 3 (B.get y f') ;
  assert_equal true (B.get z f');
  assert_equal 13 (B.get u f');

  ()
*)


(**/**)

val all_bits_ : int -> int -> int
(** Undocumented, do not use. Exposed for testing purpose *)

(**/**)
