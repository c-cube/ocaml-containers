(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bit Field}

  This module defines efficient bitfields
  up to 30 or 62 bits (depending on the architecture) in
  a relatively type-safe way.

  {b status: experimental}
  @since NEXT_RELEASE *)

module type EMPTY = sig end

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

  val bool : unit -> bool field
  (** New field of type bool *)

  val int2 : unit -> int field
  (** New field of type 2-bits int *)
end

(** Create a new bitfield type
 
Example:

{[
  module B = CCBitField.Make(struct end);;

  let x = B.bool ();;
  let y = B.int2 ();;
  let z = B.bool ();;

  B.width y ;; (* = 2 *)

  let f = B.empty
  |> B.set y 3
  |> B.set z true ;;
  (* = 14 *)

  B.get x f ;; (* false *)
  B.get y f ;; (* 3 *)
  B.get z f ;; (* true *)

]}
 
 *)
module Make(X : EMPTY) : BITFIELD
