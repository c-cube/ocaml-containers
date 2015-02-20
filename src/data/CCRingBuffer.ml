(*
 * CCRingBuffer - Polymorphic circular buffer with
 * deque semantics for accessing both the head and tail.
 *
 * Copyright (C) 2014 Simon Cruanes
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

  module type S = sig
    type elt
    type t

    val empty : t

    val make: int -> elt -> t

    val length: t -> int

    val get: t -> int -> elt

    val set: t -> int -> elt -> unit

    val sub: t -> int -> int -> t

    val copy : t -> t

    val blit : t -> int -> t -> int -> int -> unit

    val iter : (elt -> unit) -> t -> unit
  end

  module ByteArray :
    S with type elt = char and type t = bytes = struct
    type elt = char
    include Bytes
  end

  module FloatArray :
    S with type elt = float and type t = float array = struct
    type t = float array
    type elt = float
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


  module IntArray :
    S with type elt = int and type t = int array = struct
    type t = int array
    type elt = int
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


  module BoolArray :
    S with type elt = bool and type t = bool array = struct
    type t = bool array
    type elt = bool
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

module type S =
sig

  module Array : Array.S

  type t = private {
    mutable start : int;
    mutable stop : int; (* excluded *)
    mutable buf : Array.t;
    bounded: bool;
    size : int
  }
  exception Empty

  val create : ?bounded:bool -> int -> t

  val copy : t -> t

  val capacity : t -> int

  val max_capacity : t -> int option

  val length : t -> int

  val blit_from : t -> Array.t -> int -> int -> unit

  val blit_into : t ->  Array.t -> int -> int -> int

  val to_list : t -> Array.elt list

  val clear : t -> unit

  val reset : t -> unit

  val is_empty :t -> bool

  val junk_front : t -> unit

  val junk_back : t -> unit

  val skip : t -> int -> unit

  val iteri : t -> (int -> Array.elt -> unit) -> unit

  val get_front : t -> int -> Array.elt

  val get_back : t -> int -> Array.elt

  val push_back : t -> Array.elt -> unit

  val peek_front : t -> Array.elt

  val peek_back : t -> Array.elt

  val take_back : t -> Array.elt

  val take_front : t -> Array.elt

end

module Make_array(Array:Array.S) =
struct

  module Array = Array
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
      buf = Array.empty
    }

  let copy b =
    { b with buf=Array.copy b.buf; }

(*$T
  let b = ByteBuffer.create 3 in \
  let s = Bytes.of_string "hello world" in \
  ByteBuffer.blit_from b s 0 (Bytes.length s); \
    let b' = ByteBuffer.copy b in \
    try ByteBuffer.iteri b (fun i c -> if ByteBuffer.get_front b' i <> c then raise Exit); true with Exit -> false
*)


  let capacity b = Array.length b.buf

  let max_capacity b = if b.bounded then Some b.size else None

  let length b =
    if b.stop >= b.start
    then b.stop - b.start
    else (Array.length b.buf - b.start) + b.stop

  (* resize [b] so that inner capacity is [cap] *)
  let resize b cap elem =
    assert (cap >= Array.length b.buf);
    let buf' = Array.make cap elem in
    (* copy into buf' *)
    let _:int =
      if b.stop >= b.start
      then begin
        Array.blit b.buf b.start buf' 0 (b.stop - b.start);
        b.stop - b.start
      end else begin
        let len_end = Array.length b.buf - b.start in
        Array.blit b.buf b.start buf' 0 len_end;
        Array.blit b.buf 0 buf' len_end b.stop;
        len_end + b.stop
      end
    in
    b.buf <- buf'

  let blit_from_bounded b from_buf o len =
    let cap = capacity b - len in
    (* resize if needed, with a constant to amortize *)
    if cap < len then begin
      let new_size =
        let desired = Array.length b.buf + len + 24 in
        min (b.size+1) desired in
      resize b new_size from_buf.(0)
    end;
    let sub = Array.sub from_buf o len in
    let iter x =
      let capacity = capacity b in
      Array.set b.buf b.stop x;
      if b.stop = capacity-1 then b.stop <- 0 else b.stop <- b.stop + 1;
      if b.start = b.stop then
        begin
          if b.start = capacity-1 then b.start <- 0 else b.start <- b.start + 1
        end
    in
    Array.iter iter sub


  let blit_from_unbounded b from_buf o len =
    let cap = capacity b - len in
    (* resize if needed, with a constant to amortize *)
    if cap < len then resize b (max b.size (Array.length b.buf + len + 24)) from_buf.(0);
    assert (capacity b - length b >= len);
    if b.stop >= b.start
    then (*  [_______ start xxxxxxxxx stop ______] *)
      let len_end = Array.length b.buf - b.stop in
      if len_end >= len
      then (Array.blit from_buf o b.buf b.stop len;
            b.stop <- b.stop + len)
      else (Array.blit from_buf o b.buf b.stop len_end;
            Array.blit from_buf (o+len_end) b.buf 0 (len-len_end);
            b.stop <- len-len_end)
    else begin (* [xxxxx stop ____________ start xxxxxx] *)
      let len_middle = b.start - b.stop in
      assert (len_middle >= len);
      Array.blit from_buf o b.buf b.stop len;
      b.stop <- b.stop + len
    end;
    ()

  let blit_from b from_buf o len =
    if (Array.length from_buf) = 0 then () else
    if b.bounded then
      blit_from_bounded b from_buf o len
    else
      blit_from_unbounded b from_buf o len

  let blit_into b to_buf o len =
    if o+len > Array.length to_buf
    then raise (Invalid_argument "BufferIO.blit_into");
    if b.stop >= b.start
    then
      let n = min (b.stop - b.start) len in
      let _ = Array.blit b.buf b.start to_buf o n in
      n
    else begin
      let len_end = Array.length b.buf - b.start in
      Array.blit b.buf b.start to_buf o (min len_end len);
      if len_end >= len
      then len  (* done *)
      else begin
        let n = min b.stop (len - len_end) in
        Array.blit b.buf 0 to_buf (o+len_end) n;
        n + len_end
      end
    end

	let clear b =
    b.stop <- 0;
    b.start <- 0;
    ()

  let reset b =
    clear b;
    b.buf <- Array.empty

  let is_empty b = b.start = b.stop

  let take_front b =
    if b.start = b.stop then raise Empty;
    let c = b.buf.(b.start) in
    if b.start + 1 = Array.length b.buf
    then b.start <- 0
    else b.start <- b.start + 1;
    c

  let take_back b =
    if b.start = b.stop then raise Empty;
    if b.stop - 1 = 0
    then b.stop <- Array.length b.buf - 1
    else b.stop <- b.stop - 1;
    b.buf.(b.stop)

  let junk_front b =
    if b.start = b.stop then raise Empty;
    if b.start + 1 = Array.length b.buf
    then b.start <- 0
    else b.start <- b.start + 1

  let junk_back b =
    if b.start = b.stop then raise Empty;
    if b.stop - 1 = 0
    then b.stop <- Array.length b.buf - 1
    else b.stop <- b.stop - 1

  let skip b len =
    if len > length b then raise (Invalid_argument "BufferIO.skip");
    if b.stop >= b.start
    then b.start <- b.start + len
    else
      let len_end = Array.length b.buf - b.start in
      if len > len_end
      then b.start <- len-len_end  (* wrap to the beginning *)
      else b.start <- b.start + len

  (*$Q
    (Q.pair Q.printable_string Q.printable_string) (fun (s,s') -> \
    (let b = ByteBuffer.create 24 in ByteBuffer.blit_from b s 0 (Bytes.length s);
    ByteBuffer.blit_from b s' 0 (Bytes.length s'); \
    ByteBuffer.blit_from b "hello world" 0 (Bytes.length "hello word"); (* big enough *) \
    let l = ByteBuffer.length b in let l' = l/2 in ByteBuffer.skip b l'; \
    ByteBuffer.length b + l' = l))
  *)

  let iteri b f =
    if b.stop >= b.start
    then for i = b.start to b.stop - 1 do f i b.buf.(i) done
    else (
      for i = b.start to Array.length b.buf -1 do f i b.buf.(i) done;
      for i = 0 to b.stop - 1 do f i b.buf.(i) done;
    )

  (*$T
    let s = "hello world" in \
    let b = of_string s in \
    try iteri b (fun i c -> if s.[i] <> c then raise Exit); true with Exit -> false
  *)

  let get b i =
    if b.stop >= b.start
    then
      if i >= b.stop - b.start
      then raise (Invalid_argument "CCRingBuffer.get")
      else b.buf.(b.start + i)
    else
      let len_end = Array.length b.buf - b.start in
      if i < len_end
      then b.buf.(b.start + i)
      else if i - len_end > b.stop
      then raise (Invalid_argument "CCRingBuffer.get")
      else b.buf.(i - len_end)

  let get_front b i =
    if is_empty b then
      raise (Invalid_argument "CCRingBuffer.get_front")
    else
      get b i

  let get_back b i =
    let offset = ((length b) - i - 1) in
      if offset < 0 then
        raise (Invalid_argument "CCRingBuffer.get_back")
      else get b offset

  let to_list b =
    let len = length b in
    let rec build l i =
      if i < 0 then l else
      build ((get_front b i)::l) (i-1) in
    build [] (len-1)

  let push_back b e = blit_from b (Array.make 1 e) 0 1

  let peek_front b = if is_empty b then
      raise Empty else Array.get b.buf b.start

  let peek_back b = if is_empty b then
      raise Empty else Array.get b.buf
                         (if b.stop = 0 then capacity b - 1 else b.stop-1)
end

module ByteBuffer = Make_array(Array.ByteArray)

module Make(Elt:sig type t end) = Make_array(Array.Make(Elt))

