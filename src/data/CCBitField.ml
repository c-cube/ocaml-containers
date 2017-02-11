(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bit Field} *)

exception TooManyFields
exception Frozen

let max_width = Sys.word_size - 2

(*$R
  let module B = CCBitField.Make(struct end) in
  let x = B.mk_field () in
  let y = B.mk_field () in
  let z = B.mk_field () in

  let f = B.empty |> B.set x true |> B.set y true in

  assert_bool "z_false" (not (B.get z f)) ;

  assert_bool "z_true" (f |> B.set z true |> B.get z);
*)

(*$R
  let module B = CCBitField.Make(struct end) in
  let _ = B.mk_field () in
  B.freeze();
  assert_bool "must raise"
    (try ignore (B.mk_field()); false with Frozen -> true);

*)

(*$R
  let module B = CCBitField.Make(struct end) in

  let x = B.mk_field () in
  let y = B.mk_field () in
  let z = B.mk_field () in
  let u = B.mk_field () in
  B.freeze();

  let f = B.empty
    |> B.set y true
    |> B.set z true
  in

  assert_equal ~printer:CCInt.to_string 6 (f :> int) ;

  assert_equal false (B.get x f) ;
  assert_equal true (B.get y f) ;
  assert_equal true (B.get z f);

  let f' = B.set u true f in

  assert_equal false (B.get x f') ;
  assert_equal true (B.get y f') ;
  assert_equal true (B.get z f');
  assert_equal true (B.get u f');

  ()
*)


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
  if w=0 then acc
  else
    let acc = acc lor (1 lsl w-1) in
    all_bits_ acc (w-1)

(*$T
  all_bits_ 0 1 = 1
  all_bits_ 0 2 = 3
  all_bits_ 0 3 = 7
  all_bits_ 0 4 = 15
*)

(* increment and return previous value *)
let get_then_incr n =
  let x = !n in
  incr n;
  x

module Make(X : sig end) : S = struct
  type t = int

  let empty = 0

  let width_ = ref 0
  let frozen_ = ref false

  type field = int (* a mask *)

  let get field x = (x land field) <> 0

  let set field b x =
    if b then x lor field else x land (lnot field)

  let mk_field () =
    if !frozen_ then raise Frozen;
    let n = get_then_incr width_ in
    if n > max_width then raise TooManyFields;
    let mask = 1 lsl n in
    mask

  let freeze () = frozen_ := true

  let total_width () = !width_
end
