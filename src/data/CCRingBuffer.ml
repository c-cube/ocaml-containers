(* This file is free software, part of containers. See file "license" for more details. *)

(* Copyright (C) 2015 Simon Cruanes, Carmelo Piccione *)

(** Generic Circular Buffer for IO, with bulk operations.
    The bulk operations (e.g. based on {!Array.blit} or {!Bytes.blit})
    are more efficient than item-by-item copy.

    See https://en.wikipedia.org/wiki/Circular_buffer for an overview. *)

module Array = struct
  (** The abstract type for arrays *)
  module type S = sig
    type elt
    (** The element type *)

    type t
    (** The type of an array instance *)

    val dummy : elt
    (** A dummy element used for empty slots in the array
        @since 2.4 *)

    val create : int -> t
    (** Make an array of the given size, filled with dummy elements *)

    val length : t -> int
    (** [length t] gets the total number of elements currently in [t] *)

    val get : t -> int -> elt
    (** [get t i] gets the element at position [i] *)

    val set : t -> int -> elt -> unit
    (** [set t i e] sets the element at position [i] to [e] *)

    val sub : t -> int -> int -> t
    (** [sub t i len] gets the subarray of [t] from
        position [i] to [i + len] *)

    val copy : t -> t
    (** [copy t] makes a fresh copy of the array [t] *)

    val blit : t -> int -> t -> int -> int -> unit
    (** [blit t s arr i len] copies [len] elements from [arr] starting at [i]
        to position [s] from [t] *)

    val iter : (elt -> unit) -> t -> unit
    (** [iter f t] iterates over the array [t] invoking [f] with
        the current element, in array order *)
  end

  module Byte : S with type elt = char and type t = Bytes.t = struct
    type elt = char

    let dummy = '\x00'

    include Bytes
  end

  module Make (Elt : sig
    type t

    val dummy : t
  end) : S with type elt = Elt.t and type t = Elt.t array = struct
    type elt = Elt.t
    type t = Elt.t array

    let dummy = Elt.dummy
    let create size = Array.make size Elt.dummy
    let length = Array.length
    let get = Array.get
    let set = Array.set
    let copy = Array.copy
    let blit = Array.blit
    let iter = Array.iter
    let sub = Array.sub
  end
end

module type S = sig
  module Array : Array.S
  (** The module type of Array for this ring buffer *)

  type t
  (** Defines the bounded ring buffer type *)

  exception Empty
  (** Raised in querying functions when the buffer is empty *)

  val create : int -> t
  (** [create size] creates a new bounded buffer with given size.
      The underlying array is allocated immediately and no further (large)
      allocation will happen from now on.
      @raise Invalid_argument if the arguments is [< 1] *)

  val copy : t -> t
  (** Make a fresh copy of the buffer. *)

  val capacity : t -> int
  (** Length of the inner buffer. *)

  val length : t -> int
  (** Number of elements currently stored in the buffer. *)

  val is_full : t -> bool
  (** true if pushing an element would erase another element.
      @since 1.3 *)

  val blit_from : t -> Array.t -> int -> int -> unit
  (** [blit_from buf from_buf o len] copies the slice [o, ... o + len - 1] from
      a input buffer [from_buf] to the end of the buffer.
      If the slice is too large for the buffer, only the last part of the array
      will be copied.
      @raise Invalid_argument if [o,len] is not a valid slice of [s] *)

  val blit_into : t -> Array.t -> int -> int -> int
  (** [blit_into buf to_buf o len] copies at most [len] elements from [buf]
      into [to_buf] starting at offset [o] in [s].
      @return the number of elements actually copied ([min len (length buf)]).
      @raise Invalid_argument if [o,len] is not a valid slice of [s]. *)

  val append : t -> into:t -> unit
  (** [append b ~into] copies all data from [b] and adds it at the
      end of [into]. Erases data of [into] if there is not enough room. *)

  val to_list : t -> Array.elt list
  (** Extract the current content into a list *)

  val clear : t -> unit
  (** Clear the content of the buffer. Doesn't actually destroy the content. *)

  val is_empty : t -> bool
  (** Is the buffer empty (i.e. contains no elements)? *)

  val junk_front : t -> unit
  (** Drop the front element from [t].
      @raise Empty if the buffer is already empty. *)

  val junk_back : t -> unit
  (** Drop the back element from [t].
      @raise Empty if the buffer is already empty. *)

  val skip : t -> int -> unit
  (** [skip b len] removes [len] elements from the front of [b].
      @raise Invalid_argument if [len > length b]. *)

  val iter : t -> f:(Array.elt -> unit) -> unit
  (** [iter b ~f] calls [f i t] for each element [t] in [buf] *)

  val iteri : t -> f:(int -> Array.elt -> unit) -> unit
  (** [iteri b ~f] calls [f i t] for each element [t] in [buf], with [i]
      being its relative index within [buf]. *)

  val get_front : t -> int -> Array.elt
  (** [get_front buf i] returns the [i]-th element of [buf] from the front, ie
      the one returned by [take_front buf] after [i-1] calls to [junk_front buf].
      @raise Invalid_argument if the index is invalid (> [length buf]) *)

  val get_back : t -> int -> Array.elt
  (** [get_back buf i] returns the [i]-th element of [buf] from the back, ie
      the one returned by [take_back buf] after [i-1] calls to [junk_back buf].
      @raise Invalid_argument if the index is invalid (> [length buf]) *)

  val push_back : t -> Array.elt -> unit
  (** Push value at the back of [t].
      If [t.bounded=false], the buffer will grow as needed,
      otherwise the oldest elements are replaced first. *)

  val peek_front : t -> Array.elt option
  (** First value from front of [t], without modification. *)

  val peek_front_exn : t -> Array.elt
  (** First value from front of [t], without modification.
      @raise Empty if buffer is empty.
      @since 1.3 *)

  val peek_back : t -> Array.elt option
  (** Get the last value from back of [t], without modification. *)

  val peek_back_exn : t -> Array.elt
  (** Get the last value from back of [t], without modification.
      @raise Empty if buffer is empty.
      @since 1.3 *)

  val take_back : t -> Array.elt option
  (** Take and remove the last value from back of [t], if any *)

  val take_back_exn : t -> Array.elt
  (** Take and remove the last value from back of [t].
      @raise Empty if buffer is already empty. *)

  val take_front : t -> Array.elt option
  (** Take and remove the first value from front of [t], if any *)

  val take_front_exn : t -> Array.elt
  (** Take and remove the first value from front of [t].
      @raise Empty if buffer is already empty. *)

  val of_array : Array.t -> t
  (** Create a buffer from an initial array, but doesn't take ownership
      of it (stills allocates a new internal array)
      @since 0.11 *)

  val to_array : t -> Array.t
  (** Create an array from the elements, in order.
      @since 0.11 *)
end

module MakeFromArray (A : Array.S) : S with module Array = A = struct
  module Array = A

  type t = {
    mutable start: int;
    mutable stop: int; (* excluded *)
    buf: Array.t;
  }

  exception Empty

  let create size =
    if size < 1 then invalid_arg "CCRingBuffer.create";
    {
      start = 0;
      stop = 0;
      buf = A.create (size + 1) (* keep room for extra slot *);
    }

  let copy b = { b with buf = A.copy b.buf }

  let capacity b =
    let len = A.length b.buf in
    match len with
    | 0 -> 0
    | l -> l - 1

  let length b =
    if b.stop >= b.start then
      b.stop - b.start
    else
      A.length b.buf - b.start + b.stop

  let is_full b = length b + 1 = Array.length b.buf

  let next_ b i =
    let j = i + 1 in
    if j = A.length b.buf then
      0
    else
      j

  let incr_start_ b = b.start <- next_ b b.start
  let incr_stop_ b = b.stop <- next_ b b.stop

  let push_back b e =
    A.set b.buf b.stop e;
    incr_stop_ b;
    if b.start = b.stop then incr_start_ b;
    (* overwritten one element *)
    ()

  let blit_from b from_buf o len =
    if len = 0 then
      ()
    else if o + len > A.length from_buf then
      invalid_arg "CCRingBuffer.blit_from"
    else
      for i = o to o + len - 1 do
        push_back b (A.get from_buf i)
      done

  let blit_into b to_buf o len =
    if o + len > A.length to_buf then invalid_arg "CCRingBuffer.blit_into";
    if b.stop >= b.start then (
      let n = min (b.stop - b.start) len in
      A.blit b.buf b.start to_buf o n;
      n
    ) else (
      let len_end = A.length b.buf - b.start in
      A.blit b.buf b.start to_buf o (min len_end len);
      if len_end >= len then
        len
      (* done *)
      else (
        let n = min b.stop (len - len_end) in
        A.blit b.buf 0 to_buf (o + len_end) n;
        n + len_end
      )
    )

  let is_empty b = b.start = b.stop

  let take_front_exn b =
    if b.start = b.stop then raise Empty;
    let c = A.get b.buf b.start in
    A.set b.buf b.start A.dummy;
    b.start <- next_ b b.start;
    c

  let take_front b = try Some (take_front_exn b) with Empty -> None

  let take_back_exn b =
    if b.start = b.stop then raise Empty;
    if b.stop = 0 then
      b.stop <- A.length b.buf - 1
    else
      b.stop <- b.stop - 1;
    let c = A.get b.buf b.stop in
    A.set b.buf b.stop A.dummy;
    c

  let take_back b = try Some (take_back_exn b) with Empty -> None

  let junk_front b =
    if b.start = b.stop then raise Empty;
    A.set b.buf b.start A.dummy;
    if b.start + 1 = A.length b.buf then
      b.start <- 0
    else
      b.start <- b.start + 1

  let junk_back b =
    if b.start = b.stop then raise Empty;
    if b.stop = 0 then
      b.stop <- A.length b.buf - 1
    else
      b.stop <- b.stop - 1;
    A.set b.buf b.stop A.dummy

  let skip b len =
    if len > length b then invalid_arg "CCRingBuffer.skip";
    for _ = 1 to len do
      junk_front b
    done

  let clear b = skip b (length b)

  let iter b ~f =
    if b.stop >= b.start then
      for i = b.start to b.stop - 1 do
        f (A.get b.buf i)
      done
    else (
      for i = b.start to A.length b.buf - 1 do
        f (A.get b.buf i)
      done;
      for i = 0 to b.stop - 1 do
        f (A.get b.buf i)
      done
    )

  let iteri b ~f =
    if b.stop >= b.start then
      for i = b.start to b.stop - 1 do
        f i (A.get b.buf i)
      done
    else (
      for i = b.start to A.length b.buf - 1 do
        f i (A.get b.buf i)
      done;
      for i = 0 to b.stop - 1 do
        f i (A.get b.buf i)
      done
    )

  let get b i =
    if b.stop >= b.start then
      if i >= b.stop - b.start then
        invalid_arg "CCRingBuffer.get"
      else
        A.get b.buf (b.start + i)
    else (
      let len_end = A.length b.buf - b.start in
      if i < len_end then
        A.get b.buf (b.start + i)
      else if i - len_end > b.stop then
        invalid_arg "CCRingBuffer.get"
      else
        A.get b.buf (i - len_end)
    )

  let get_front b i =
    if is_empty b then
      invalid_arg "CCRingBuffer.get_front"
    else
      get b i

  let get_back b i =
    let offset = length b - i - 1 in
    if offset < 0 then
      invalid_arg "CCRingBuffer.get_back"
    else
      get b offset

  let to_list b =
    let len = length b in
    let rec build l i =
      if i < 0 then
        l
      else
        build (get_front b i :: l) (i - 1)
    in
    build [] (len - 1)

  (* TODO: more efficient version, with one or two blit *)
  let append b ~into = iter b ~f:(push_back into)

  let peek_front_exn b =
    if is_empty b then
      raise Empty
    else
      A.get b.buf b.start

  let peek_front b = try Some (peek_front_exn b) with Empty -> None

  let peek_back_exn b =
    if is_empty b then
      raise Empty
    else (
      let i =
        if b.stop = 0 then
          A.length b.buf - 1
        else
          b.stop - 1
      in
      A.get b.buf i
    )

  let peek_back b = try Some (peek_back_exn b) with Empty -> None

  let of_array a =
    let b = create (max (A.length a) 16) in
    blit_from b a 0 (A.length a);
    b

  let to_array b =
    let a = A.create (length b) in
    let n = blit_into b a 0 (length b) in
    assert (n = length b);
    a
end

module Byte = MakeFromArray (Array.Byte)

module Make (Elt : sig
  type t

  val dummy : t
end) =
  MakeFromArray (Array.Make (Elt))
