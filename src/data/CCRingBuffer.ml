(*
 * CCRingBuffer - Polymorphic circular buffer with
 * deque semantics for accessing both the head and tail.
 *
 * Copyright (C) 2015 Simon Cruanes, Carmelo Piccione
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Polymorphic Circular Buffer for IO *)

module Array = struct
  (** The abstract type for arrays *)
  module type S = sig
    (** The element type *)
    type elt

    (** The type of an array instance *)
    type t

    val empty : t
    (** The empty array *)

    val make: int -> elt -> t
    (** [make s e] makes an array of size [s] with [e] elements *)

    val length: t -> int
    (** [length t] gets the total number of elements currently in [t] *)

    val get: t -> int -> elt
    (** [get t i] gets the element at position [i] *)

    val set: t -> int -> elt -> unit
    (** [set t i e] sets the element at position [i] to [e] *)

    val sub: t -> int -> int -> t
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

  module Byte :
    S with type elt = char and type t = Bytes.t = struct
    type elt = char
    include Bytes
  end

  module Make(Elt:sig type t end) :
    S with type elt = Elt.t and type t = Elt.t array = struct
    type elt = Elt.t
    type t = Elt.t array
    let make = Array.make
    let length = Array.length
    let get = Array.get
    let set = Array.set
    let copy = Array.copy
    let blit = Array.blit
    let iter = Array.iter
    let sub = Array.sub
    let empty = Array.of_list []
  end
end

module type S = sig
  (** The module type of Array for this ring buffer *)
  module Array : Array.S

  (** Defines the ring buffer type, with both bounded and
      unbounded flavors *)
  type t

  (** Raised in querying functions when the buffer is empty *)
  exception Empty

  val create : ?bounded:bool -> int -> t
  (** [create ?bounded size] creates a new buffer with given size.
      Defaults to [bounded=false]. *)

  val copy : t -> t
  (** Make a fresh copy of the buffer. *)

  val capacity : t -> int
  (** Length of the inner buffer. *)

  val max_capacity : t -> int option
  (** Maximum length of the inner buffer, or [None] if unbounded. *)

  val length : t -> int
  (** Number of elements currently stored in the buffer. *)

  val blit_from : t -> Array.t -> int -> int -> unit
  (** [blit_from buf from_buf o len] copies the slice [o, ... o + len - 1] from
      a input buffer [from_buf] to the end of the buffer.
      @raise Invalid_argument if [o,len] is not a valid slice of [s] *)

  val blit_into : t ->  Array.t -> int -> int -> int
  (** [blit_into buf to_buf o len] copies at most [len] elements from [buf]
      into [to_buf] starting at offset [o] in [s].
      @return the number of elements actually copied ([min len (length buf)]).
      @raise Invalid_argument if [o,len] is not a valid slice of [s] *)

  val append : t -> into:t -> unit
  (** [append b ~into] copies all data from [b] and adds it at the
      end of [into] *)

  val to_list : t -> Array.elt list
  (** Extract the current content into a list *)

  val clear : t -> unit
  (** Clear the content of the buffer. Doesn't actually destroy the content. *)

  val reset : t -> unit
  (** Clear the content of the buffer, and also resize it to a default size *)

  val is_empty :t -> bool
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

  val peek_front : t -> Array.elt
  (** First value from front of [t].
      @raise Empty if buffer is empty. *)

  val peek_back : t -> Array.elt
  (** Get the last value from back of [t].
      @raise Empty if buffer is empty. *)

  val take_back : t -> Array.elt option
  (** Take the last value from back of [t], if any *)

  val take_back_exn : t -> Array.elt
  (** Take the last value from back of [t].
      @raise Empty if buffer is already empty. *)

  val take_front : t -> Array.elt option
  (** Take the first value from front of [t], if any *)

  val take_front_exn : t -> Array.elt
  (** Take the first value from front of [t].
      @raise Empty if buffer is already empty. *)

  val of_array : Array.t -> t
  (** Create a buffer from an initial array, but doesn't take ownership
      of it (stills allocates a new internal array) *)

  val to_array : t -> Array.t
  (** Create an array from the elements, in order.
      @since 0.11 *)
end

module MakeFromArray(A:Array.S) = struct
  module Array = A

  type t = {
    mutable start : int;
    mutable stop : int; (* excluded *)
    mutable buf : Array.t;
    bounded : bool;
    size : int
  }

  exception Empty

  let create ?(bounded=false) size =
    { start=0;
      stop=0;
      bounded;
      size;
      buf = A.empty
    }

  let copy b =
    { b with buf=A.copy b.buf; }

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    let b' = Byte.copy b in \
    try Byte.iteri b (fun i c -> if Byte.get_front b' i <> c then raise Exit); true with Exit -> false)
  *)

  (*$T
    let b = Byte.of_array (Bytes.of_string "abc") in \
    let b' = Byte.copy b in \
    Byte.clear b; \
    Byte.to_array b' = (Bytes.of_string "abc") && Byte.to_array b = Bytes.empty
  *)

  let capacity b =
    let len = A.length b.buf in
    match len with 0 -> 0 | l -> l - 1

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    Byte.capacity b >= s_len)
  *)

  (*$Q
    (Q.pair Q.small_int Q.printable_string) (fun (i, s) -> let s = Bytes.of_string s in \
    let i = abs i in \
    let s_len = Bytes.length s in \
    let b = Byte.create ~bounded:true i in \
    Byte.blit_from b s 0 s_len; \
    Byte.capacity b <= i)
  *)

  let max_capacity b = if b.bounded then Some b.size else None

  (*$Q
    Q.small_int (fun i -> \
    let i = abs i in \
    let b = Byte.create i in \
    Byte.max_capacity b = None)
  *)

  (*$Q
    Q.small_int (fun i -> \
    let i = abs i in \
    let b = Byte.create ~bounded:true i in \
    Byte.max_capacity b = Some i)
  *)

  let length b =
    if b.stop >= b.start
    then b.stop - b.start
    else (A.length b.buf - b.start) + b.stop

  (*$Q
    (Q.pair Q.small_int Q.printable_string) (fun (i, s) -> let s = Bytes.of_string s in \
    let i = abs i in \
    let s_len = Bytes.length s in \
    let b = Byte.create i in \
    Byte.blit_from b s 0 s_len; \
    Byte.length b = s_len)
  *)

  (*$Q
    (Q.pair Q.small_int Q.printable_string) (fun (i, s) -> let s = Bytes.of_string s in \
    let i = abs i in \
    let s_len = Bytes.length s in \
    let b = Byte.create ~bounded:true i in \
    Byte.blit_from b s 0 s_len; \
    Byte.length b >= 0 && Byte.length b <= i)
  *)

  (* resize [b] so that inner capacity is [cap] *)
  let resize b cap elem =
    assert (cap >= A.length b.buf);
    let buf' = A.make cap elem in
    (* copy into buf' *)
    if b.stop >= b.start
    then
      A.blit b.buf b.start buf' 0 (b.stop - b.start)
    else begin
      let len_end = A.length b.buf - b.start in
      A.blit b.buf b.start buf' 0 len_end;
      A.blit b.buf 0 buf' len_end b.stop;
    end;
    b.buf <- buf'

  let blit_from_bounded b from_buf o len =
    let cap = capacity b - length b in
    (* resize if needed, with a constant to amortize *)
    if cap < len then (
      let new_size =
        let desired = A.length b.buf + len + 24 in
        min (b.size+1) desired in
      resize b new_size (A.get from_buf 0);
      let good = capacity b = b.size || capacity b - length b >= len in
      assert good;
    );
    let sub = A.sub from_buf o len in
    let iter x =
      let capacity = A.length b.buf in
      A.set b.buf b.stop x;
      if b.stop = capacity-1 then b.stop <- 0 else b.stop <- b.stop + 1;
      if b.start = b.stop then
        if b.start = capacity-1 then b.start <- 0 else b.start <- b.start + 1
    in
    A.iter iter sub


  let blit_from_unbounded b from_buf o len =
    let cap = capacity b - length b in
    (* resize if needed, with a constant to amortize *)
    if cap < len
      then resize b (max (b.size+1) (A.length b.buf + len + 24)) (A.get from_buf 0);
    let good = capacity b - length b >= len in
    assert good;
    if b.stop >= b.start
    then (*  [_______ start xxxxxxxxx stop ______] *)
      let len_end = A.length b.buf - b.stop in
      if len_end >= len
      then (A.blit from_buf o b.buf b.stop len;
            b.stop <- b.stop + len)
      else (A.blit from_buf o b.buf b.stop len_end;
            A.blit from_buf (o+len_end) b.buf 0 (len-len_end);
            b.stop <- len-len_end)
    else begin (* [xxxxx stop ____________ start xxxxxx] *)
      let len_middle = b.start - b.stop in
      assert (len_middle >= len);
      A.blit from_buf o b.buf b.stop len;
      b.stop <- b.stop + len
    end;
    ()

  let blit_from b from_buf o len =
    if A.length from_buf = 0 then () else
    if b.bounded then
      blit_from_bounded b from_buf o len
    else
      blit_from_unbounded b from_buf o len

  (*$Q
    (Q.pair Q.printable_string Q.printable_string) (fun (s,s') -> \
    let s = Bytes.of_string s in let s' = Bytes.of_string s' in  \
    (let b = Byte.create 24 in \
    Byte.blit_from b s 0 (Bytes.length s); \
    Byte.blit_from b s' 0 (Bytes.length s'); \
    Byte.length b = Bytes.length s + Bytes.length s'))
  *)


  (*$Q
    (Q.pair Q.printable_string Q.printable_string) (fun (s,s') -> \
    let s = Bytes.of_string s in let s' = Bytes.of_string s' in  \
    (let b = Byte.create ~bounded:true (Bytes.length s + Bytes.length s') in \
    Byte.blit_from b s 0 (Bytes.length s); \
    Byte.blit_from b s' 0 (Bytes.length s'); \
    Byte.length b = Bytes.length s + Bytes.length s'))
  *)


  let blit_into b to_buf o len =
    if o+len > A.length to_buf
    then invalid_arg "CCRingBuffer.blit_into";
    if b.stop >= b.start
    then
      let n = min (b.stop - b.start) len in
      let _ = A.blit b.buf b.start to_buf o n in
      n
    else begin
      let len_end = A.length b.buf - b.start in
      A.blit b.buf b.start to_buf o (min len_end len);
      if len_end >= len
      then len  (* done *)
      else begin
        let n = min b.stop (len - len_end) in
        A.blit b.buf 0 to_buf (o+len_end) n;
        n + len_end
      end
    end

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let b = Byte.create (Bytes.length s) in \
    Byte.blit_from b s 0 (Bytes.length s); \
    let to_buf = Bytes.create (Bytes.length s) in \
    let len = Byte.blit_into b to_buf 0 (Bytes.length s) in \
    to_buf = s && len = Bytes.length s)
  *)

  let clear b =
    b.stop <- 0;
    b.start <- 0;
    ()

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    Byte.clear b; \
    Byte.length b = 0)
  *)


  let reset b =
    clear b;
    b.buf <- A.empty

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    Byte.reset b; \
    Byte.length b = 0 && Byte.capacity b = 0)
  *)


  let is_empty b = b.start = b.stop

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    Byte.skip b s_len; \
    Byte.is_empty b)
  *)

  let take_front_exn b =
    if b.start = b.stop then raise Empty;
    let c = A.get b.buf b.start in
    if b.start + 1 = A.length b.buf
    then b.start <- 0
    else b.start <- b.start + 1;
    c

  let take_front b = try Some (take_front_exn b) with Empty -> None

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    try let front = Byte.take_front_exn b in \
    front = Bytes.get s 0 with Byte.Empty -> s_len = 0)
  *)

  let take_back_exn b =
    if b.start = b.stop then raise Empty;
    if b.stop - 1 = 0
    then b.stop <- A.length b.buf - 1
    else b.stop <- b.stop - 1;
    A.get b.buf b.stop

  let take_back b = try Some (take_back_exn b) with Empty -> None

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    try let back = Byte.take_back_exn b in \
    back = Bytes.get s (Bytes.length s - 1) with Byte.Empty -> s_len = 0)
  *)

  let junk_front b =
    if b.start = b.stop then raise Empty;
    if b.start + 1 = A.length b.buf
    then b.start <- 0
    else b.start <- b.start + 1

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    try let () = Byte.junk_front b in \
    s_len - 1 = Byte.length b with Byte.Empty -> s_len = 0)
  *)

  let junk_back b =
    if b.start = b.stop then raise Empty;
    if b.stop = 0
    then b.stop <- A.length b.buf - 1
    else b.stop <- b.stop - 1

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    try let () = Byte.junk_back b in \
    s_len - 1 = Byte.length b with Byte.Empty -> s_len = 0)
  *)

  let skip b len =
    if len > length b then
      invalid_arg ("CCRingBuffer.skip: " ^ string_of_int len);
    if b.stop >= b.start
    then b.start <- b.start + len
    else
      let len_end = A.length b.buf - b.start in
      if len > len_end
      then b.start <- len-len_end  (* wrap to the beginning *)
      else b.start <- b.start + len

  (*$Q
    (Q.pair Q.printable_string Q.printable_string) (fun (s,s') -> \
    let s = Bytes.of_string s in let s' = Bytes.of_string s' in  \
    (let b = Byte.create 24 in \
    Byte.blit_from b s 0 (Bytes.length s); \
    Byte.blit_from b s' 0 (Bytes.length s'); \
    let h = Bytes.of_string "hello world" in \
    Byte.blit_from b h 0 (Bytes.length h); (* big enough *) \
    let l = Byte.length b in let l' = l/2 in Byte.skip b l'; \
    Byte.length b + l' = l))
  *)

  let iter b ~f =
    if b.stop >= b.start
    then for i = b.start to b.stop - 1 do f (A.get b.buf i) done
    else (
      for i = b.start to A.length b.buf -1 do f (A.get b.buf i) done;
      for i = 0 to b.stop - 1 do f (A.get b.buf i) done;
    )

  let iteri b ~f =
    if b.stop >= b.start
    then for i = b.start to b.stop - 1 do f i (A.get b.buf i) done
    else (
      for i = b.start to A.length b.buf -1 do f i (A.get b.buf i) done;
      for i = 0 to b.stop - 1 do f i (A.get b.buf i) done;
    )

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    try Byte.iteri b (fun i c -> if Byte.get_front b i <> c then raise Exit); \
      true with Exit -> false)
  *)

  let get b i =
    if b.stop >= b.start
    then
      if i >= b.stop - b.start
      then invalid_arg ("CCRingBuffer.get:" ^ string_of_int i)
      else A.get b.buf (b.start + i)
    else
      let len_end = A.length b.buf - b.start in
      if i < len_end
      then A.get b.buf (b.start + i)
      else if i - len_end > b.stop
      then invalid_arg ("CCRingBuffer.get: " ^ string_of_int i)
      else A.get b.buf (i - len_end)

  let get_front b i =
    if is_empty b then
      invalid_arg ("CCRingBuffer.get_front: " ^ string_of_int i)
    else
      get b i

  (*$Q
    (Q.pair Q.small_int Q.printable_string) (fun (i, s) -> \
    let s = Bytes.of_string (s ^ " ") in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    let index = abs (i mod Byte.length b) in \
    let front = Byte.get_front b index in \
    front = Bytes.get s index)
  *)

  let get_back b i =
    let offset = ((length b) - i - 1) in
    if offset < 0 then
      raise (Invalid_argument ("CCRingBuffer.get_back:" ^ string_of_int i))
    else get b offset

  (*$Q
    (Q.pair Q.small_int Q.printable_string) (fun (i, s) -> \
    let s = Bytes.of_string (s ^ " ") in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    let index = abs (i mod Byte.length b) in \
    let back = Byte.get_back b index in \
    back = Bytes.get s (s_len - index - 1))
  *)


  let to_list b =
    let len = length b in
    let rec build l i =
      if i < 0 then l else
        build ((get_front b i)::l) (i-1) in
    build [] (len-1)

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    let l = Byte.to_list b in \
    let explode s = let rec exp i l = \
     if i < 0 then l else exp (i - 1) (Bytes.get s i :: l) in \
     exp (Bytes.length s - 1) [] in \
    explode s = l)
  *)

  let push_back b e = blit_from b (A.make 1 e) 0 1

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    Byte.push_back b 'X'; \
    Byte.peek_back b = 'X')
  *)

  (* TODO: more efficient version *)
  let append b ~into =
    iter b ~f:(push_back into)

  let peek_front b =
    if is_empty b then raise Empty
    else A.get b.buf b.start

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    try let back = Byte.peek_front b in \
    back = Bytes.get s 0 with Byte.Empty -> s_len = 0)
  *)

  let peek_back b = if is_empty b
    then raise Empty
    else A.get b.buf
        (if b.stop = 0 then capacity b - 1 else b.stop-1)

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create s_len in \
    Byte.blit_from b s 0 s_len; \
    try let back = Byte.peek_back b in \
    back = Bytes.get s (s_len - 1) with Byte.Empty -> s_len = 0)
  *)

  let of_array a =
    let b = create (max (A.length a) 16) in
    blit_from b a 0 (A.length a);
    b

  let to_array b =
    if is_empty b then A.empty
    else (
      let a = A.make (length b) (peek_front b) in
      let n = blit_into b a 0 (length b) in
      assert (n = length b);
      a
    )

  (*$Q
    Q.printable_string (fun s -> let s = Bytes.of_string s in \
      let b = Byte.of_array s in let s' = Byte.to_array b in \
      s = s')
  *)
end

module Byte = MakeFromArray(Array.Byte)

module Make(Elt:sig type t end) = MakeFromArray(Array.Make(Elt))
