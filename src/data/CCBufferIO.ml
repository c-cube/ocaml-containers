(*
 * CCBufferIO - Polymorphic circular buffer
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

type 'a t = {
  mutable start : int;
  mutable stop : int; (* excluded *)
  mutable buf : 'a array;
  size: int
}

exception Empty

let create size =
  { start=0;
    stop=0;
    size;
    buf = Array.of_list [];
  }

let copy b =
  { b with buf=Array.copy b.buf; }


let capacity b = Array.length b.buf

let length b =
  if b.stop >= b.start
  then b.stop - b.start
  else (Array.length b.buf - b.start) + b.stop

(* resize [b] so that inner capacity is [cap] *)
let resize b cap elem =
  assert (cap >= Array.length b.buf);
  let buf' = Array.make cap elem in
  (* copy into buf' *)
  let len =
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
  b.buf <- buf';
  ()

let blit_from b from_buf o len =
  if (Array.length from_buf) = 0 then () else
  let cap = capacity b - length b in
  (* resize if needed, with a constant to amortize *)
   if cap < len then
     resize b (min b.size (Array.length b.buf + len + 24)) from_buf.(0);
  let sub = Array.sub from_buf o len in
    let iter i x = 
      b.start <- i mod capacity b;
      Array.set b.buf x b.start in
    Array.iteri iter sub
     
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

let pop b =
  if b.start = b.stop then raise Empty;
  let c = b.buf.(b.start) in
  if b.start + 1 = Array.length b.buf
  then b.start <- 0
  else b.start <- b.start + 1;
  c

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
    then Array.to_list (Array.sub b.buf b.start b.stop)
  else List.append 
    (Array.to_list (Array.sub b.buf b.start (Array.length b.buf)))
    (Array.to_list (Array.sub b.buf 0 b.stop)) 
    

