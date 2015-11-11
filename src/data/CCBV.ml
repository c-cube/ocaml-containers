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

type t = {
  mutable a : int array;
}

let empty () = { a = [| |] }

let create ~size default =
  if size = 0 then { a = [| |] }
    else begin
      let n = if size mod __width = 0 then size / __width else (size / __width) + 1 in
      let arr = if default
        then Array.make n __all_ones
        else Array.make n 0
      in
      (* adjust last bits *)
      if default && (size mod __width) <> 0
        then arr.(n-1) <- __shift 0 (size - (n-1) * __width);
      { a = arr }
    end

(*$T
  create ~size:17 true |> cardinal = 17
  create ~size:32 true |> cardinal= 32
  create ~size:132 true |> cardinal = 132
  create ~size:200 false |> cardinal = 0
  create ~size:29 true |> to_sorted_list = CCList.range 0 28
*)

let copy bv = { a=Array.copy bv.a; }

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let bv = of_list l in to_list bv = to_list (copy bv))
*)

let length bv = Array.length bv.a

let resize bv len =
  if len > Array.length bv.a
    then begin
      let a' = Array.make len 0 in
      Array.blit bv.a 0 a' 0 (Array.length bv.a);
      bv.a <- a'
    end

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
  for i = 0 to length bv - 1 do
    n := !n + __count_bits bv.a.(i)
  done;
  !n

(*$R
  let bv1 = CCBV.create ~size:87 true in
  assert_equal ~printer:string_of_int 87 (CCBV.cardinal bv1);
  *)

(*$Q
  Q.small_int (fun n -> CCBV.cardinal (CCBV.create ~size:n true) = n)
  *)

let is_empty bv =
  try
    for i = 0 to Array.length bv.a - 1 do
      if bv.a.(i) <> 0 then raise Exit
    done;
    true
  with Exit ->
    false

let get bv i =
  let n = i / __width in
  if n < Array.length bv.a
    then
      let i = i - n * __width in
      bv.a.(n) land (1 lsl i) <> 0
    else false

(*$R
  let bv = CCBV.create ~size:99 false in
  assert_bool "32 must be false" (not (CCBV.get bv 32));
  assert_bool "88 must be false" (not (CCBV.get bv 88));
  assert_bool "5 must be false" (not (CCBV.get bv 5));
  CCBV.set bv 32;
  CCBV.set bv 88;
  CCBV.set bv 5;
  assert_bool "32 must be true" (CCBV.get bv 32);
  assert_bool "88 must be true" (CCBV.get bv 88);
  assert_bool "5 must be true" (CCBV.get bv 5);
  assert_bool "33 must be false" (not (CCBV.get bv 33));
  assert_bool "44 must be false" (not (CCBV.get bv 44));
  assert_bool "1 must be false" (not (CCBV.get bv 1));
*)

let set bv i =
  let n = i / __width in
  if n >= Array.length bv.a
    then resize bv (n+1);
  let i = i - n * __width in
  bv.a.(n) <- bv.a.(n) lor (1 lsl i)

(*$T
  let bv = create ~size:3 false in set bv 0; get bv 0
  let bv = create ~size:3 false in set bv 1; not (get bv 0)
*)

let reset bv i =
  let n = i / __width in
  if n >= Array.length bv.a
    then resize bv (n+1);
  let i = i - n * __width in
  bv.a.(n) <- bv.a.(n) land (lnot (1 lsl i))

(*$T
  let bv = create ~size:3 false in set bv 0; reset bv 0; not (get bv 0)
*)

let flip bv i =
  let n = i / __width in
  if n >= Array.length bv.a
    then resize bv (n+1);
  let i = i - n * __width in
  bv.a.(n) <- bv.a.(n) lxor (1 lsl i)

(*$R
  let bv = of_list [1;10; 11; 30] in
  flip bv 10;
  assert_equal [1;11;30] (to_sorted_list bv);
  assert_equal false (get bv 10);
  flip bv 10;
  assert_equal true (get bv 10);
  flip bv 5;
  assert_equal [1;5;10;11;30] (to_sorted_list bv);
  assert_equal true (get bv 5);
  flip bv 100;
  assert_equal [1;5;10;11;30;100] (to_sorted_list bv);
  assert_equal true (get bv 100);
*)

let clear bv =
  Array.iteri (fun i _ -> bv.a.(i) <- 0) bv.a

(*$T
let bv = create ~size:37 true in cardinal bv = 37 && (clear bv; cardinal bv= 0)
*)

(*$R
  let bv = CCBV.of_list [1; 5; 200] in
  assert_equal ~printer:string_of_int 3 (CCBV.cardinal bv);
  CCBV.clear bv;
  assert_equal ~printer:string_of_int 0 (CCBV.cardinal bv);
  assert_bool "must be empty" (CCBV.is_empty bv);
*)

let iter bv f =
  let len = Array.length bv.a in
  for n = 0 to len - 1 do
    let j = __width * n in
    for i = 0 to __width - 1 do
      f (j+i) (bv.a.(n) land (1 lsl i) <> 0)
    done
  done

(*$R
  let bv = create ~size:30 false in
  set bv 5;
  let n = ref 0 in
  iter bv (fun i b -> incr n; assert_equal b (i=5));
  assert_bool "at least 30" (!n >= 30)
*)

let iter_true bv f =
  let len = Array.length bv.a in
  for n = 0 to len - 1 do
    let j = __width * n in
    for i = 0 to __width - 1 do
      if bv.a.(n) land (1 lsl i) <> 0
        then f (j+i)
    done
  done

(*$T
  of_list [1;5;7] |> iter_true |> Sequence.to_list |> List.sort CCOrd.compare = [1;5;7]
*)

(*$inject
  let _gen = Q.Gen.(map of_list (list nat))
  let _pp bv = Q.Print.(list string) (List.map string_of_int (to_list bv))
  let _small bv = length bv

  let gen_bv = Q.make ~small:_small ~print:_pp _gen
*)

(*$QR
  gen_bv (fun bv ->
    let l' = Sequence.to_rev_list (CCBV.iter_true bv) in
    let bv' = CCBV.of_list l' in
    CCBV.cardinal bv = CCBV.cardinal bv'
  )
*)

let to_list bv =
  let l = ref [] in
  iter_true bv (fun i -> l := i :: !l);
  !l

(*$R
  let bv = CCBV.of_list [1; 5; 156; 0; 222] in
  assert_equal ~printer:string_of_int 5 (CCBV.cardinal bv);
  CCBV.set bv 201;
  assert_equal ~printer:string_of_int 6 (CCBV.cardinal bv);
  let l = CCBV.to_list bv in
  let l = List.sort compare l in
  assert_equal [0;1;5;156;201;222] l;
*)

let to_sorted_list bv =
  List.rev (to_list bv)

let of_list l =
  let size = List.fold_left max 0 l in
  let bv = create ~size false in
  List.iter (fun i -> set bv i) l;
  bv

(*$T
  of_list [1;32;64] |> CCFun.flip get 64
  of_list [1;32;64] |> CCFun.flip get 32
  of_list [1;31;63] |> CCFun.flip get 63
*)

exception FoundFirst of int

let first bv =
  try
    iter_true bv (fun i -> raise (FoundFirst i));
    raise Not_found
  with FoundFirst i ->
    i

(*$T
  of_list [50; 10; 17; 22; 3; 12] |> first = 3
*)

let filter bv p =
  iter_true bv
    (fun i -> if not (p i) then reset bv i)

(*$T
  let bv = of_list [1;2;3;4;5;6;7] in filter bv (fun x->x mod 2=0); \
    to_sorted_list bv = [2;4;6]
*)

let union_into ~into bv =
  if length into < length bv
    then resize into (length bv);
  let len = Array.length bv.a in
  for i = 0 to len - 1 do
    into.a.(i) <- into.a.(i) lor bv.a.(i)
  done

let union bv1 bv2 =
  let bv = copy bv1 in
  union_into ~into:bv bv2;
  bv

(*$R
  let bv1 = CCBV.of_list [1;2;3;4] in
  let bv2 = CCBV.of_list [4;200;3] in
  let bv = CCBV.union bv1 bv2 in
  let l = List.sort compare (CCBV.to_list bv) in
  assert_equal [1;2;3;4;200] l;
  ()
*)

(*$T
union (of_list [1;2;3;4;5]) (of_list [7;3;5;6]) |> to_sorted_list = CCList.range 1 7
*)

let inter_into ~into bv =
  let n = min (length into) (length bv) in
  for i = 0 to n - 1 do
    into.a.(i) <- into.a.(i) land bv.a.(i)
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

(*$T
  inter (of_list [1;2;3;4]) (of_list [2;4;6;1]) |> to_sorted_list = [1;2;4]
*)

(*$R
  let bv1 = CCBV.of_list [1;2;3;4] in
  let bv2 = CCBV.of_list [4;200;3] in
  CCBV.inter_into ~into:bv1 bv2;
  let l = List.sort compare (CCBV.to_list bv1) in
  assert_equal [3;4] l;
*)

let select bv arr =
  let l = ref [] in
  begin try
    iter_true bv
      (fun i ->
        if i >= Array.length arr
          then raise Exit
          else l := arr.(i) :: !l)
  with Exit -> ()
  end;
  !l

(*$R
  let bv = CCBV.of_list [1;2;5;400] in
  let arr = [|"a"; "b"; "c"; "d"; "e"; "f"|] in
  let l = List.sort compare (CCBV.select bv arr) in
  assert_equal ["b"; "c"; "f"] l;
*)

let selecti bv arr =
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

(*$R
  let bv = CCBV.of_list [1;2;5;400] in
  let arr = [|"a"; "b"; "c"; "d"; "e"; "f"|] in
  let l = List.sort compare (CCBV.selecti bv arr) in
  assert_equal [("b",1); ("c",2); ("f",5)] l;
*)

(*$T
  selecti (of_list [1;4;3]) [| 0;1;2;3;4;5;6;7;8 |] \
    |> List.sort CCOrd.compare = [1, 1; 3,3; 4,4]
*)

type 'a sequence = ('a -> unit) -> unit

let to_seq bv k = iter_true bv k

let of_seq seq =
  let l = ref [] and maxi = ref 0 in
  seq (fun x -> l := x :: !l; maxi := max !maxi x);
  let bv = create ~size:(!maxi+1) false in
  List.iter (fun i -> set bv i) !l;
  bv

(*$T
  CCList.range 0 10 |> CCList.to_seq |> of_seq |> to_seq \
    |> CCList.of_seq |> List.sort CCOrd.compare = CCList.range 0 10
*)

let print out bv =
  Format.pp_print_string out "bv {";
  iter bv
    (fun _i b ->
      Format.pp_print_char out (if b then '1' else '0')
    );
  Format.pp_print_string out "}"
