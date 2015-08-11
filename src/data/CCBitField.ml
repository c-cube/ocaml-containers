(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bit Field} *)

exception TooManyFields
exception Frozen

let max_width = Sys.word_size - 2

module type EMPTY = sig end

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

module Make(X : EMPTY) : BITFIELD = struct
  type t = int

  let empty = 0

  type _ field_kind =
    | Bool : bool field_kind
    | Int : int field_kind

  type 'a field = {
    kind : 'a field_kind;
    name : string;
    width : int;
    get : t -> 'a;
    set : 'a -> t -> t;
  }

  type any_field = AnyField : 'a field -> any_field

  let width_ = ref 0
  let frozen_ = ref false
  let fields_ = Queue.create()
  let register_ f = Queue.push (AnyField f) fields_

  let get f x = f.get x
  let set f v x = f.set v x
  let width f = f.width
  let name f = f.name

  let make_field f =
    if !width_ > max_width then raise TooManyFields;
    if !frozen_ then raise Frozen;
    register_ f;
    f

  let new_name_ () =
    "field_" ^ string_of_int (Queue.length fields_)

  let bool ?(name=new_name_()) () =
    let n = !width_ in
    incr width_;
    let mask = 1 lsl n in
    make_field {
      kind = Bool;
      name;
      width=1;
      get=(fun x -> (x land mask) <> 0);
      set=(fun b x ->
        if b then x lor mask else x land (lnot mask)
      );
    }

  let int2 ?(name=new_name_()) () =
    let n = !width_ in
    width_ := n+2;
    let mask = 3 lsl n in
    make_field {
      kind = Int;
      name;
      width=2;
      get=(fun x -> (x land mask) lsr n);
      set=(fun v x ->
        assert (x >= 0 && x < 4);
        let x = x land (lnot mask) in
        x lor (v lsl n)
      )
    }

  let int3 ?(name=new_name_()) () =
    let n = !width_ in
    width_ := n+3;
    let mask = 7 lsl n in
    make_field {
      kind = Int;
      name;
      width=3;
      get=(fun x -> (x land mask) lsr n);
      set=(fun v x ->
        assert (x >= 0 && x < 8);
        let x = x land (lnot mask) in
        x lor (v lsl n)
      )
    }

  let int ?(name=new_name_()) ~width:w =
    let n = !width_ in
    width_ := n+w;
    let mask_unshifted = all_bits_ 0 w in
    let mask = mask_unshifted lsl n in
    make_field {
      kind = Int;
      name;
      width=w;
      get=(fun x -> (x land mask) lsr n);
      set=(fun v x ->
        assert (x >= 0 && x <= mask_unshifted);
        let x = x land (lnot mask) in
        x lor (v lsl n)
      )
    }

  let freeze () = frozen_ := true

  let total_width () = !width_

  let iter_fields f = Queue.iter f fields_

  let pp out x =
    let ppf = Format.fprintf in
    ppf out "{@[<hv>";
    Queue.iter
      (fun (AnyField f) ->
        match f.kind with
        | Bool ->
            let b = get f x in
            ppf out "%s: %b,@ " f.name b
        | Int ->
            let i = get f x in
            ppf out "%s: %ui@, " f.name i
      ) fields_;
    ppf out "@]}"
end
