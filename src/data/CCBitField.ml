(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bit Field} *)

exception TooManyFields
exception Frozen

let max_width = Sys.word_size - 2

module type EMPTY = sig end

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

let get_then_add n offset =
  let x = !n in
  n := !n + offset;
  x

module Make(X : EMPTY) : S = struct
  type t = int

  let empty = 0

  type _ field_kind =
    | Bool : bool field_kind
    | Int : int field_kind

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

  type any_field = AnyField : (module FIELD with type value = 'a) * 'a field_kind -> any_field

  let width_ = ref 0
  let frozen_ = ref false
  let fields_ = Queue.create()
  let register_
    : type a. (module FIELD with type value = a) -> unit
    = fun f ->
      if !width_ > max_width then raise TooManyFields;
      if !frozen_ then raise Frozen;
      let (module F) = f in
      Queue.push (AnyField (f, F.kind)) fields_

  let new_name_ () =
    "field_" ^ string_of_int (Queue.length fields_)

  let bool ?(name=new_name_()) () : bool field =
    let module B = struct
      type value = bool
      let n = get_then_incr width_
      let mask = 1 lsl n
      let name = name
      let width = 1
      let get x = (x land mask) <> 0
      let set b x =
        if b then x lor mask else x land (lnot mask)
      let kind = Bool
    end in
    let f = (module B : FIELD with type value = bool) in
    register_ f;
    f

  let int2 ?(name=new_name_()) () =
    let module Int2 = struct
      type value = int
      let n = get_then_add width_ 2
      let name = name
      let mask = 3 lsl n
      let kind = Int
      let width=2
      let get x = (x land mask) lsr n
      let set v x =
        assert (x >= 0 && x < 4);
        let x = x land (lnot mask) in
        x lor (v lsl n)
    end in
    let f = (module Int2 : FIELD with type value = int) in
    register_ f;
    f


  let int3 ?(name=new_name_()) () =
    let module Int3 = struct
      type value = int
      let name = name
      let n = get_then_add width_ 3
      let mask = 7 lsl n
      let width = 3
      let kind = Int
      let get x = (x land mask) lsr n
      let set v x =
      assert (x >= 0 && x < 8);
      let x = x land (lnot mask) in
      x lor (v lsl n)
    end in
    let f = (module Int3 : FIELD with type value = int) in
    register_ f;
    f

  let int ?(name=new_name_()) ~width:w () =
    let module F = struct
      type value = int
      let n = get_then_add width_ w
      let mask_unshifted = all_bits_ 0 w
      let mask = mask_unshifted lsl n
      let kind = Int
      let name = name
      let width = w
      let get x = (x land mask) lsr n
      let set v x =
      assert (x >= 0 && x <= mask_unshifted);
      let x = x land (lnot mask) in
      x lor (v lsl n)
    end in
    let f = (module F : FIELD with type value = int) in
    register_ f;
    f

  let freeze () = frozen_ := true

  let total_width () = !width_

  let iter_fields f = Queue.iter f fields_

  let pp out x =
    let ppf = Format.fprintf in
    ppf out "{@[<hv>";
    let first=ref true in
    Queue.iter
      (fun (AnyField ((module F), kind)) ->
        if !first then first := false else ppf out ",@ ";
        match kind with
        | Bool ->
            let b = F.get x in
            ppf out "%s=%b" F.name b
        | Int ->
            let i = F.get x in
            ppf out "%s=%u" F.name i
      ) fields_;
    ppf out "@]}"
end
