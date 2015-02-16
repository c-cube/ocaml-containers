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

module type ARRAY = sig
  type elt
  type t

  val make: int -> elt -> t
  val length: t -> int

  val get: t -> int -> elt

  val set: t -> int -> elt -> unit

  val sub: t -> int -> int -> t
  val max_length: int

  val copy : t -> t
  val of_list : elt list -> t
  val to_list : t -> elt list
  val blit : t -> int -> t -> int -> int -> unit

  val iter : (elt -> unit) -> t -> unit
end

module Make(Array:ARRAY) =
struct

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
      buf = Array.of_list []
    }

  let copy b =
    { b with buf=Array.copy b.buf; }


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

  let add b s = blit_from b s 0 (Array.length s)

  (*$Q
    (Q.pair Q.printable_string Q.printable_string) (fun (s,s') -> \
    let b = create 24 in add b s; add_string b s'; \
    Array.length s + String.length s' = length b)
  *)

  let clear b =
    b.stop <- 0;
    b.start <- 0;
    ()

  let reset b =
    clear b;
    b.buf <- Array.of_list []

  let is_empty b = b.start = b.stop

  let next b =
    if b.start = b.stop then raise Empty;
    b.buf.(b.start)

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

  let junk b =
    if b.start = b.stop then raise Empty;
    if b.start + 1 = Array.length b.buf
    then b.start <- 0
    else b.start <- b.start + 1

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
    let b = create 24 in add_string b s; add_string b s'; \
    add_string b "hello world"; (* big enough *) \
    let l = length b in let l' = l/2 in skip b l'; \
    length b + l' = l)
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
      then raise (Invalid_argument "BufferIO.get")
      else b.buf.(b.start + i)
    else
      let len_end = Array.length b.buf - b.start in
      if i < len_end
      then b.buf.(b.start + i)
      else if i - len_end > b.stop
      then raise (Invalid_argument "BufferIO.get")
      else b.buf.(i - len_end)

  let to_list b =
    if (b.stop >= b.start) 
    then Array.to_list (Array.sub b.buf b.start (b.stop-b.start))
    else List.append 
           (Array.to_list (Array.sub b.buf b.start (Array.length b.buf - b.start)))
           (Array.to_list (Array.sub b.buf 0 b.stop)) 

  let push_back b e = add b (Array.of_list [e])

  let peek_front b = if is_empty b then 
      raise Empty else Array.get b.buf b.start

  let peek_back b = if is_empty b then
      raise Empty else Array.get b.buf 
                         (if b.stop = 0 then capacity b - 1 else b.stop-1)
end
