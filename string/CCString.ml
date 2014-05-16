
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Basic String Utils} *)

type t = string

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit

let is_sub ~sub i s j =
  let rec check k =
    if i + k = String.length sub
      then true
      else sub.[i + k] = s.[j+k] && check (k+1)
  in
  check 0

(* note: quite inefficient if [by] is long *)
let split_gen ~by s =
  let len_by = String.length by in
  assert (len_by > 0);
  let n = String.length s in
  let prev = ref 0 in
  let stop = ref false in
  let rec search i =
    if !stop then None
    else if i >= n
      then (
        stop := true;
        Some (String.sub s !prev (n- !prev))  (* done *)
      )
    else if is_prefix i 0
      then (
        let p = !prev in
        prev := i+len_by;
        Some (String.sub s p (i-p))
      )
    else search (i+1)
  and is_prefix i j =
    if j = len_by
      then true
    else if i = n
      then false
    else s.[i] = by.[j] && is_prefix (i+1) (j+1)
  in
  fun () ->
    search !prev

let split_seq ~by s k =
  let rec aux g = match g () with
    | None -> ()
    | Some x -> k x; aux g
  in aux (split_gen ~by s)

let split ~by s =
  let rec aux g acc = match g () with
    | None -> List.rev acc
    | Some x -> aux g (x::acc)
  in aux (split_gen ~by s) []

(*$T
  split ~by:"," "aa,bb,cc" = ["aa"; "bb"; "cc"]
  split ~by:"--" "a--b----c--" = ["a"; "b"; ""; "c"; ""]
*)

(* note: inefficient *)
let find ?(start=0) ~sub s =
  let n = String.length sub in
  let i = ref start in
  try
    while !i + n < String.length s do
      if is_sub ~sub 0 s !i then raise Exit;
      incr i
    done;
    -1
  with Exit ->
    !i

let repeat s n =
  assert (n>=0);
  let len = String.length s in
  assert(len > 0);
  let buf = String.create (len * n) in
  for i = 0 to n-1 do
    String.blit s 0 buf (i * len) len;
  done;
  buf

let prefix ~pre s =
  String.length pre <= String.length s &&
  (let i = ref 0 in
    while !i < String.length pre && s.[!i] = pre.[!i] do incr i done;
    !i = String.length pre)


let to_gen s =
  let i = ref 0 in
  fun () ->
    if !i = String.length s then None
    else (
      let c = String.unsafe_get s !i in
      incr i;
      Some c
    )

let of_gen g =
  let b = Buffer.create 32 in
  let rec aux () = match g () with
    | None -> Buffer.contents b
    | Some c -> Buffer.add_char b c; aux ()
  in aux ()

let to_seq s k = String.iter k s

let of_seq seq =
  let b= Buffer.create 32 in
  seq (Buffer.add_char b);
  Buffer.contents b

let pp = Buffer.add_string
