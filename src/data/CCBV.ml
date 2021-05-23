
(** {2 Imperative Bitvectors} *)

(*$inject
  let ppli = CCFormat.(Dump.list int)
*)

let width_ = 8

(* Helper functions *)
let[@inline] get_ b i = Char.code (Bytes.get b i)
let[@inline] unsafe_get_ b i = Char.code (Bytes.unsafe_get b i)

let[@inline] set_ b i v = Bytes.set b i (Char.unsafe_chr v)
let[@inline] unsafe_set_ b i v = Bytes.unsafe_set b i (Char.unsafe_chr v)

let[@inline] mod_ n = (n lsl (Sys.word_size - 4)) lsr (Sys.word_size - 4)

let[@inline] div_ n = n lsr 3

let[@inline] mul_ n = n lsl 3

let zero = Char.unsafe_chr 0

(** We use OCamls chars to store the bits. We index them from the
    least significant bit. We create masks to zero out the most significant
    bits that aren't used to store values. This is necessary when we are
    constructing or negating a bit vector. *)
let lsb_masks_ =
  let b = Bytes.make (width_ + 1) zero in
  for i = 1 to width_ do
    set_ b i (get_ b (i-1) lor (1 lsl (i - 1)))
  done;
  b

let all_ones_ = Bytes.get lsb_masks_ width_

let count_bits_ n =
  let table = [| 0; 1; 1; 2; 1; 2; 2; 3; 1; 2; 2; 3; 2; 3; 3; 4; 1; 2; 2; 3; 2; 3; 3; 4;
                 2; 3; 3; 4; 3; 4; 4; 5; 1; 2; 2; 3; 2; 3; 3; 4; 2; 3; 3; 4; 3; 4; 4; 5;
                 2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6; 1; 2; 2; 3; 2; 3; 3; 4;
                 2; 3; 3; 4; 3; 4; 4; 5; 2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6;
                 2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6; 3; 4; 4; 5; 4; 5; 5; 6;
                 4; 5; 5; 6; 5; 6; 6; 7; 1; 2; 2; 3; 2; 3; 3; 4; 2; 3; 3; 4; 3; 4; 4; 5;
                 2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6; 2; 3; 3; 4; 3; 4; 4; 5;
                 3; 4; 4; 5; 4; 5; 5; 6; 3; 4; 4; 5; 4; 5; 5; 6; 4; 5; 5; 6; 5; 6; 6; 7;
                 2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6; 3; 4; 4; 5; 4; 5; 5; 6;
                 4; 5; 5; 6; 5; 6; 6; 7; 3; 4; 4; 5; 4; 5; 5; 6; 4; 5; 5; 6; 5; 6; 6; 7;
                 4; 5; 5; 6; 5; 6; 6; 7; 5; 6; 6; 7; 6; 7; 7; 8; |] in
  Array.unsafe_get table n

(*  Can I access the "private" members in testing? $Q
    (Q.int_bound (Sys.word_size - 1)) (fun i -> count_bits_ lsb_masks_.(i) = i)
*)

type t = {
  mutable b : bytes;
  mutable size : int;
}

let length t = t.size

let empty () = { b = Bytes.empty ; size = 0 }

let bytes_length_of_size size =
  if mod_ size = 0 then div_ size else (div_ size) + 1

let create ~size default =
  if size = 0 then empty ()
  else (
    let n = bytes_length_of_size size in
    let b = if default
      then Bytes.make n all_ones_
      else Bytes.make n zero
    in
    (* adjust last bits *)
    let r = mod_ size in
    if default && r <> 0 then (
      Bytes.unsafe_set b (n-1) (Bytes.unsafe_get lsb_masks_ r);
    );
    { b; size }
  )

(*$Q
  (Q.pair Q.small_int Q.bool) (fun (size, b) -> create ~size b |> length = size)
*)

(*$T
  create ~size:17 true |> cardinal = 17
  create ~size:32 true |> cardinal = 32
  create ~size:132 true |> cardinal = 132
  create ~size:200 false |> cardinal = 0
  create ~size:29 true |> to_sorted_list = CCList.range 0 28
*)

let copy bv = { bv with b = Bytes.copy bv.b }

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let bv = of_list l in to_list bv = to_list (copy bv))
*)

let capacity bv = mul_ (Bytes.length bv.b)

let cardinal bv =
  if bv.size = 0 then 0
  else (
    let n = ref 0 in
    for i = 0 to Bytes.length bv.b - 1 do
      n := !n + count_bits_ (get_ bv.b i) (* MSB of last element are all 0 *)
    done;
    !n
  )

(*$Q
  Q.small_int (fun size -> create ~size true |> cardinal = size)
*)

let really_resize_ bv ~desired ~current size =
  let b = Bytes.make desired zero in
  Bytes.blit bv.b 0 b 0 current;
  bv.b <- b;
  bv.size <- size

let grow_ bv size =
  if size <= capacity bv (* within capacity *)
  then bv.size <- size
  else (
    (* beyond capacity *)
    let desired = bytes_length_of_size size in
    let current = Bytes.length bv.b in
    assert (desired > current);
    really_resize_ bv ~desired ~current size
  )

let shrink_ bv size =
  let desired = bytes_length_of_size size in
  let current = Bytes.length bv.b in
  really_resize_ bv ~desired ~current size

let resize bv size =
  if size < 0 then invalid_arg "resize: negative size";
  if size < bv.size (* shrink *)
  then shrink_ bv size
  else if size = bv.size
  then ()
  else grow_ bv size

(*$R
  let bv1 = CCBV.create ~size:87 true in
  assert_equal ~printer:string_of_int 87 (CCBV.cardinal bv1);
*)

(*$Q
  Q.small_int (fun n -> CCBV.cardinal (CCBV.create ~size:n true) = n)
*)

let is_empty bv =
  try
    for i = 0 to Bytes.length bv.b - 1 do
      if get_ bv.b i <> 0 then raise Exit
    done;
    true
  with Exit ->
    false

let get bv i =
  if i < 0 then invalid_arg "get: negative index";
  let n = div_ i in
  let i = mod_ i in
  if n < Bytes.length bv.b
  then (unsafe_get_ bv.b n) land (1 lsl i) <> 0
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
  if i < 0 then invalid_arg "set: negative index"
  else (
    let n = div_ i in
    let j = mod_ i in
    if i >= bv.size then grow_ bv (i+1);
    unsafe_set_ bv.b n ((unsafe_get_ bv.b n) lor (1 lsl j))
  )

(*$T
  let bv = create ~size:3 false in set bv 0; get bv 0
  let bv = create ~size:3 false in set bv 1; not (get bv 0)
*)

let reset bv i =
  if i < 0 then invalid_arg "reset: negative index"
  else (
    let n = div_ i in
    let j = mod_ i in
    if i >= bv.size then grow_ bv (i+1);
    unsafe_set_ bv.b n ((unsafe_get_ bv.b n) land (lnot (1 lsl j)))
  )

(*$T
  let bv = create ~size:3 false in set bv 0; reset bv 0; not (get bv 0)
*)

let flip bv i =
  if i < 0 then invalid_arg "reset: negative index"
  else (
    let n = div_ i in
    let j = mod_ i in
    if i >= bv.size then grow_ bv (i+1);
    unsafe_set_ bv.b n ((unsafe_get_ bv.b n) lxor (1 lsl j))
  )

(*$R
  let bv = of_list [1;10; 11; 30] in
  flip bv 10;
  assert_equal ~printer:Q.Print.(list int) [1;11;30] (to_sorted_list bv);
  assert_equal ~printer:Q.Print.bool false (get bv 10);
  flip bv 10;
  assert_equal ~printer:Q.Print.bool true (get bv 10);
  flip bv 5;
  assert_equal ~printer:Q.Print.(list int) [1;5;10;11;30] (to_sorted_list bv);
  assert_equal ~printer:Q.Print.bool true (get bv 5);
  flip bv 100;
  assert_equal ~printer:Q.Print.(list int) [1;5;10;11;30;100] (to_sorted_list bv);
  assert_equal ~printer:Q.Print.bool true (get bv 100);
*)

let clear bv =
  Bytes.fill bv.b 0 (Bytes.length bv.b) zero

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

let equal x y : bool =
  x.size = y.size &&
  x.b = y.b

(*$T
  equal (of_list [1; 3; 4]) (of_list [1; 3; 4])
  equal (empty()) (empty())
  not (equal (empty ()) (of_list [1]))
  not (equal (empty ()) (of_list [2; 5]))
  not (equal (of_list [1;3]) (of_list [2; 3]))
*)

let iter bv f =
  let len = bytes_length_of_size bv.size in
  assert (len <= Bytes.length bv.b);
  for n = 0 to len - 2 do
    let j = mul_ n in
    let word_n = unsafe_get_ bv.b n in
    for i = 0 to width_ - 1 do
      f (j+i) ((word_n land (1 lsl i)) <> 0)
    done
  done;
  if bv.size > 0 then (
    let j = mul_ (len - 1) in
    let r = mod_ bv.size in
    let final_length = if r = 0 then width_ else r in
    let final_word = unsafe_get_ bv.b (len-1) in
    for i = 0 to final_length - 1 do
      f (j + i) ((final_word land (1 lsl i)) <> 0)
    done
  )

(*$R
  List.iter
    (fun size ->
      let bv = create ~size false in
      set bv 5;
      let n = ref 0 in
      iter bv (fun i b -> incr n; assert_equal b (i=5));
      assert_bool "exactly size" (!n = size))
    [30; 100; 255; 256;10_000]
*)

(*$inject
  let iter_zip s k = s (fun x y -> k(x,y))
*)

(*$= & ~printer:Q.Print.(list (pair int bool))
  [] (iter (create ~size:0 false) |> iter_zip |> Iter.to_list)
  [0, false; 1, true; 2, false] \
    (iter (let bv = create ~size:3 false in set bv 1; bv) |> iter_zip |> Iter.to_list)
*)

(*$Q
  Q.(small_int) (fun n -> \
    assert (n >= 0); \
    let bv = create ~size:n true in \
    let l = iter bv |> iter_zip |> Iter.to_list in \
    List.length l = n && List.for_all (fun (_,b) -> b) l)
*)

let[@inline] iter_true bv f =
  iter bv (fun i b -> if b then f i else ())

(*$T
  of_list [1;5;7] |> iter_true |> Iter.to_list |> List.sort CCOrd.compare = [1;5;7]
*)

(*$inject
  let _gen = Q.Gen.(map of_list (list nat))
  let _pp bv = Q.Print.(list string) (List.map string_of_int (to_list bv))
  let _small bv = length bv

  let gen_bv = Q.make ~small:_small ~print:_pp _gen
*)

(*$QR
  gen_bv (fun bv ->
    let l' = Iter.to_rev_list (CCBV.iter_true bv) in
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

(*$= & ~printer:(CCFormat.to_string ppli)
  [1;2;3;4;64;130] (of_list [1;2;3;4;64;130] |> to_sorted_list)
*)

(*$Q
  Q.(small_list small_nat) (fun l -> \
    let l = List.sort_uniq CCOrd.compare l in \
    let l2 = of_list l |> to_sorted_list in \
    if l=l2 then true else Q.Test.fail_reportf "l1=%a, l2=%a" ppli l ppli l2)
  Q.(small_list small_nat) (fun l -> \
    let bv = of_list l in \
    let l1 = bv |> to_sorted_list in \
    let l2 = \
      (CCList.init (length bv) (get bv) |> List.mapi (fun i b->i,b) \
       |>CCList.filter_map (function (i,true) -> Some i| _ ->None)) in \
    if l1=l2 then true else Q.Test.fail_reportf "l1=%a, l2=%a" ppli l1 ppli l2)
  *)

(*$= & ~cmp:equal ~printer:(CCFormat.to_string pp)
  (of_list [0]) (let bv=empty() in set bv 0; bv)
*)

let to_sorted_list bv =
  List.rev (to_list bv)

(* Interpret these as indices. *)
let of_list l =
  let size = (List.fold_left max 0 l) + 1 in
  let bv = create ~size false in
  List.iter (fun i -> set bv i) l;
  bv

(*$T
  of_list [1;32;64] |> CCFun.flip get 64
  of_list [1;32;64] |> CCFun.flip get 32
  of_list [1;31;63] |> CCFun.flip get 63
*)

exception FoundFirst of int

let first_exn bv =
  try
    iter_true bv (fun i -> raise (FoundFirst i));
    raise Not_found
  with FoundFirst i ->
    i

let first bv =
  try Some (first_exn bv)
  with Not_found -> None

(*$T
  of_list [50; 10; 17; 22; 3; 12] |> first = Some 3
*)

let filter bv p =
  iter_true bv
    (fun i -> if not (p i) then reset bv i)

(*$T
  let bv = of_list [1;2;3;4;5;6;7] in filter bv (fun x->x mod 2=0); \
    to_sorted_list bv = [2;4;6]
*)

let negate_self b =
  let len = Bytes.length b.b in
  for n = 0 to len - 1 do
    unsafe_set_ b.b n (lnot (unsafe_get_ b.b n))
  done;
  let r = mod_ b.size in
  if r <> 0 then
    let l = Bytes.length b.b - 1 in
    unsafe_set_ b.b l (unsafe_get_ lsb_masks_ r land (unsafe_get_ b.b l))

(*$= & ~printer:(CCFormat.to_string ppli)
  [0;3;4;6] (let v = of_list [1;2;5;7;] in negate_self v; to_sorted_list v)
*)

let negate a =
  let b =  Bytes.map (fun c -> Char.unsafe_chr (lnot (Char.code c))) a.b in
  let r = mod_ a.size in
  if r <> 0 then (
    let l = Bytes.length a.b - 1 in
    unsafe_set_ b l (unsafe_get_ lsb_masks_ r land (unsafe_get_ b l))
  );
  { b ; size = a.size }

(*$Q
  Q.small_int (fun size -> create ~size false |> negate |> cardinal = size)
*)

let union_into_no_resize_ ~into bv =
  assert (Bytes.length into.b >= Bytes.length bv.b);
  for i = 0 to Bytes.length bv.b - 1 do
    unsafe_set_ into.b i
      ((unsafe_get_ into.b i) lor (unsafe_get_ bv.b i))
  done

(* Underlying size grows for union. *)
let union_into ~into bv =
  if into.size < bv.size then (
    grow_ into bv.size;
  );
  union_into_no_resize_ ~into bv

(* To avoid potentially 2 passes, figure out what we need to copy. *)
let union b1 b2 =
  if b1.size <= b2.size
  then (
    let into = copy b2 in
    union_into_no_resize_ ~into b1;
    into
  ) else (
    let into = copy b1 in
    union_into_no_resize_ ~into b2;
    into
  )

(*$R
  let bv1 = CCBV.of_list [1;2;3;4] in
  let bv2 = CCBV.of_list [4;200;3] in
  let bv = CCBV.union bv1 bv2 in
  let l = List.sort compare (CCBV.to_list bv) in
  assert_equal ~printer:(CCFormat.(to_string (Dump.list int)))
    [1;2;3;4;200] l;
  ()
*)

(*$R
  let bv1 = CCBV.of_list [1;2;3;4;64;130] in
  let bv2 = CCBV.of_list [4;64;3;120] in
  let bv = CCBV.union bv1 bv2 in
  assert_equal ~cmp:equal ~printer:(CCFormat.to_string pp)
    (of_list [1;2;3;4;64;120;130]) bv;
  ()
*)

(*$R
  let bv1 = CCBV.of_list [1;2;3;4] in
  let bv2 = CCBV.of_list [4;200;3] in
  let bv = CCBV.union bv1 bv2 in
  assert_equal ~cmp:equal ~printer:(CCFormat.to_string pp)
    (of_list [1;2;3;4;200]) bv;
  ()
*)

(*$R
  let v1 = CCBV.empty () in
  let () = CCBV.set v1 64 in
  let v2 = CCBV.diff (CCBV.empty ()) (CCBV.empty ()) in
  let v3 = CCBV.union v1 v2 in
  assert_equal ~printer:(CCFormat.to_string pp) ~cmp:CCBV.equal v1 v3
*)

(*$T
  union (of_list [1;2;3;4;5]) (of_list [7;3;5;6]) |> to_sorted_list = CCList.range 1 7
*)


let inter_into_no_resize_ ~into bv =
  assert (Bytes.length into.b <= Bytes.length bv.b);
  for i = 0 to (Bytes.length into.b) - 1 do
    unsafe_set_ into.b i
      ((unsafe_get_ into.b i) land (unsafe_get_ bv.b i))
  done

(* Underlying size shrinks for inter. *)
let inter_into ~into bv =
  if into.size > bv.size then (
    shrink_ into bv.size;
  );
  inter_into_no_resize_ ~into bv

let inter b1 b2 =
  if b1.size <= b2.size then (
    let into = copy b1 in
    inter_into_no_resize_ ~into b2;
    into
  ) else (
    let into = copy b2 in
    inter_into_no_resize_ ~into b1;
    into
  )

(*$T
  inter (of_list [1;2;3;4]) (of_list [2;4;6;1]) |> to_sorted_list = [1;2;4]
*)

(*$R
  let bv1 = CCBV.of_list [1;2;3;4;200;201] in
  let bv2 = CCBV.of_list [4;200;3] in
  let bv = CCBV.inter bv1 bv2 in
  let l = List.sort compare (CCBV.to_list bv) in
  assert_equal ~printer:(CCFormat.(to_string (Dump.list int)))
    [3;4;200] l;
  ()
*)

(*$R
  let bv1 = CCBV.of_list [1;2;3;4] in
  let bv2 = CCBV.of_list [4;200;3] in
  CCBV.inter_into ~into:bv1 bv2;
  let l = List.sort compare (CCBV.to_list bv1) in
  assert_equal [3;4] l;
*)

(* Underlying size depends on the 'in_' set for diff, so we don't change
   it's size! *)
let diff_into ~into bv =
  let n = min (Bytes.length into.b) (Bytes.length bv.b) in
  for i = 0 to n - 1 do
    unsafe_set_ into.b i
      ((unsafe_get_ into.b i) land (lnot (unsafe_get_ bv.b i)))
  done

let diff in_ not_in =
  let into = copy in_ in
  diff_into ~into not_in;
  into

(*$T
  diff (of_list [1;2;3])    (of_list [1;2;3])   |> to_list = [];
  diff (of_list [1;2;3])    (of_list [1;2;3;4]) |> to_list = [];
  diff (of_list [1;2;3;4])  (of_list [1;2;3])   |> to_list = [4];
  diff (of_list [1;2;3])      (of_list [1;2;3;400]) |> to_list = [];
  diff (of_list [1;2;3;400])  (of_list [1;2;3])     |> to_list = [400];
*)

(*$R
  let v1 = CCBV.empty () in
  set v1 65;
  let v2 = CCBV.diff v1 v1 in
  assert_bool (CCFormat.asprintf "bv: %a" pp v2) (CCBV.is_empty v2)
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

(*$= & ~printer:Q.Print.(list (pair int int))
  [1,1; 3,3; 4,4] (selecti (of_list [1;4;3]) [| 0;1;2;3;4;5;6;7;8 |] \
    |> List.sort CCOrd.compare)
*)

type 'a iter = ('a -> unit) -> unit

let to_iter bv k = iter_true bv k

(*$Q
  Q.(small_int) (fun i -> \
      let i = max 1 i in \
      let bv = create ~size:i true in \
      i = (to_iter bv |> Iter.length))
*)

let of_iter seq =
  let l = ref [] and maxi = ref 0 in
  seq (fun x -> l := x :: !l; maxi := max !maxi x);
  let bv = create ~size:(!maxi+1) false in
  List.iter (fun i -> set bv i) !l;
  bv

(*$T
  CCList.range 0 10 |> CCList.to_iter |> of_iter |> to_iter \
    |> CCList.of_iter |> List.sort CCOrd.compare = CCList.range 0 10
*)

let pp out bv =
  Format.pp_print_string out "bv {";
  iter bv
    (fun _i b ->
       Format.pp_print_char out (if b then '1' else '0')
    );
  Format.pp_print_string out "}"

(*$= & ~printer:CCFun.id
  "bv {00001}" (CCFormat.to_string pp (of_list [4]))
*)

let __to_word_l bv = CCString.to_list (Bytes.unsafe_to_string bv.b)
