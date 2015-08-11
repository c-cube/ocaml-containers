(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bit Field} *)

exception TooManyFields

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

  val bool : unit -> bool field
  (** New field of type bool *)

  val int2 : unit -> int field
  (** New field of type 2-bits int *)
end

module Make(X : EMPTY) : BITFIELD = struct
  type t = int

  let empty = 0

  let width_ = ref 0

  type 'a field = {
    start : int;
    width : int;
    get : t -> 'a;
    set : 'a -> t -> t;
  }

  let get f x = f.get x
  let set f v x = f.set v x
  let width f = f.width

  let bool () =
    let n = !width_ in
    incr width_;
    if !width_ > max_width then raise TooManyFields;
    let mask = 1 lsl n in
    {
      start=n;
      width=1;
      get=(fun x -> (x land mask) <> 0);
      set=(fun b x ->
        if b then x lor mask else x land (lnot mask)
      );
    }

  let int2 () =
    let n = !width_ in
    width_ := n+2;
    if !width_ > max_width then raise TooManyFields;
    let mask = 3 lsl n in
    {
      start=n;
      width=2;
      get=(fun x -> (x land mask) lsr n);
      set=(fun v x ->
        assert (x >= 0 && x < 4);
        let x = x land (lnot mask) in
        x lor (v lsl n)
      )
    }
end
