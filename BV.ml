
(*
copyright (c) 2013, simon cruanes
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

(** {2 Imperative Bitvectors} *)

let __width = Sys.word_size - 2

(* int with [n] ones *)
let rec __shift bv n =
  if n = 0
    then bv
    else __shift ((bv lsl 1) lor 1) (n-1)

(* only ones *)
let __all_ones = __shift 0 __width

type t = int array

let create ~size default =
  if size = 0 then [| 0 |]
    else begin
      let n = if size mod __width = 0 then size / __width else (size / __width) + 1 in
      let arr = if default
        then Array.make n __all_ones
        else Array.make n 0
      in
      (* adjust last bits *)
      if default && (size mod __width) <> 0
        then arr.(n-1) <- __shift 0 (size - (n-1) * __width);
      arr
    end

let copy = Array.copy

let length bv = Array.length bv

let resize bv ~size default =
  failwith "not implemented"

(* count the 1 bits in [n]. See https://en.wikipedia.org/wiki/Hamming_weight *)
let __count_bits n =
  let rec recurse count n =
    if n = 0 then count else recurse (count+1) (n land (n-1))
  in
  if n < 0
    then recurse 1 (n lsr 1)   (* only on unsigned *)
    else recurse 0 n

let cardinal bv =
  let n = ref 0 in
  for i = 0 to Array.length bv - 1 do
    n := !n + __count_bits bv.(i)
  done;
  !n

let is_empty bv =
  try
    for i = 0 to Array.length bv - 1 do
      if bv.(i) <> 0 then raise Exit
    done;
    true
  with Exit ->
    false

let get bv i =
  let n = i / __width in
  let i = i - n * __width in
  bv.(n) land (1 lsl i) <> 0

let set bv i =
  let n = i / __width in
  let i = i - n * __width in
  bv.(n) <- bv.(n) lor (1 lsl i)

let reset bv i =
  let n = i / __width in
  let i = i - n * __width in
  bv.(n) <- bv.(n) land (lnot (1 lsl i))

let flip bv i =
  let n = i / __width in
  let i = i - n * __width in
  bv.(n) <- bv.(n) lxor (1 lsl i)

let clear bv =
  Array.iteri (fun i _ -> bv.(i) <- 0) bv

let iter bv f =
  for n = 0 to Array.length bv - 1 do
    let j = __width * n in
    for i = 0 to __width - 1 do
      f (j+i) (bv.(n) land (1 lsl i) <> 0)
    done
  done

let iter_true bv f =
  for n = 0 to Array.length bv - 1 do
    let j = __width * n in
    for i = 0 to __width - 1 do
      if bv.(n) land (1 lsl i) <> 0
        then f (j+i)
    done
  done

let to_list bv =
  let l = ref [] in
  iter_true bv (fun i -> l := i :: !l);
  !l

let of_list l =
  let size = List.fold_left max 0 l in
  let bv = create ~size false in
  List.iter (fun i -> set bv i) l;
  bv

let union_into ~into bv =
  assert (length into >= length bv);
  for i = 0 to Array.length bv - 1 do
    into.(i) <- into.(i) lor bv.(i)
  done

let union bv1 bv2 =
  let size = __width * (max (Array.length bv1) (Array.length bv2)) in
  let bv = create ~size false in
  union_into ~into:bv bv1;
  union_into ~into:bv bv2;
  bv

let inter_into ~into bv =
  let n = min (length into) (length bv) in
  for i = 0 to n - 1 do
    into.(i) <- into.(i) land bv.(i)
  done

let inter bv1 bv2 =
  if length bv1 < length bv2
    then
      let bv = copy bv1 in
      let () = inter_into ~into:bv bv2 in
      bv
    else
      let bv = copy bv2 in
      let () = inter_into ~into:bv bv1 in
      bv

let select bv arr =
  let l = ref [] in
  begin try
    iter_true bv
      (fun i ->
        if i >= Array.length arr
          then raise Exit
          else l := (arr.(i), i) :: !l)
  with Exit -> ()
  end;
  !l
