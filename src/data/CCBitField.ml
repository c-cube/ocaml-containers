(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bit Field} *)

exception TooManyFields
exception Frozen

let max_width = Sys.word_size - 2

module type S = sig
  type t = private int
  (** Generative type of bitfields. Each instantiation of the functor
      should create a new, incompatible type *)

  val empty : t
  (** Empty bitfields (all bits 0) *)

  type field

  val get : field -> t -> bool
  (** Get the value of this field *)

  val set : field -> bool -> t -> t
  (** Set the value of this field *)

  val mk_field : unit -> field
  (** Make a new field *)

  val freeze : unit -> unit
  (** Prevent new fields from being added. From now on, creating
      a field will raise Frozen *)

  val total_width : unit -> int
  (** Current width of the bitfield *)
end

(* all bits from 0 to w-1 set to true *)
let rec all_bits_ acc w =
  if w = 0 then
    acc
  else (
    let acc = acc lor ((1 lsl w) - 1) in
    all_bits_ acc (w - 1)
  )

(* increment and return previous value *)
let get_then_incr n =
  let x = !n in
  incr n;
  x

module Make () : S = struct
  type t = int

  let empty = 0
  let width_ = ref 0
  let frozen_ = ref false

  type field = int (* a mask *)

  let get field x = x land field <> 0

  let set field b x =
    if b then
      x lor field
    else
      x land lnot field

  let mk_field () =
    if !frozen_ then raise Frozen;
    let n = get_then_incr width_ in
    if n > max_width then raise TooManyFields;
    let mask = 1 lsl n in
    mask

  let freeze () = frozen_ := true
  let total_width () = !width_
end
