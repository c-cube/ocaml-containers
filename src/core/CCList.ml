(* backport new functions from stdlib here *)

[@@@ocaml.warning "-32"]

let rec compare_lengths l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | _ :: tail1, _ :: tail2 -> compare_lengths tail1 tail2

let rec compare_length_with l n =
  match l, n with
  | _ when n < 0 -> 1
  | [], 0 -> 0
  | [], _ -> -1
  | _ :: tail, _ -> compare_length_with tail (n - 1)

let rec assoc_opt x = function
  | [] -> None
  | (y, v) :: _ when Stdlib.( = ) x y -> Some v
  | _ :: tail -> assoc_opt x tail

let rec assq_opt x = function
  | [] -> None
  | (y, v) :: _ when Stdlib.( == ) x y -> Some v
  | _ :: tail -> assq_opt x tail

[@@@ocaml.warning "+32"]

(* end of backport *)

include List

let empty = []

let is_empty = function
  | [] -> true
  | _ :: _ -> false

let mguard c =
  if c then
    [ () ]
  else
    []

(** max depth for direct recursion *)
let direct_depth_default_ = 1000

[@@@iflt 4.14]

let tail_map f l =
  (* Unwind the list of tuples, reconstructing the full list front-to-back.
     @param tail_acc a suffix of the final list; we append tuples' content
     at the front of it *)
  let rec rebuild tail_acc = function
    | [] -> tail_acc
    | (y0, y1, y2, y3, y4, y5, y6, y7, y8) :: bs ->
      rebuild
        (y0 :: y1 :: y2 :: y3 :: y4 :: y5 :: y6 :: y7 :: y8 :: tail_acc)
        bs
  in
  (* Create a compressed reverse-list representation using tuples
     @param tuple_acc a reverse list of chunks mapped with [f] *)
  let rec dive tuple_acc = function
    | x0 :: x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: xs ->
      let y0 = f x0 in
      let y1 = f x1 in
      let y2 = f x2 in
      let y3 = f x3 in
      let y4 = f x4 in
      let y5 = f x5 in
      let y6 = f x6 in
      let y7 = f x7 in
      let y8 = f x8 in
      dive ((y0, y1, y2, y3, y4, y5, y6, y7, y8) :: tuple_acc) xs
    | xs ->
      (* Reverse direction, finishing off with a direct map *)
      let tail = List.map f xs in
      rebuild tail tuple_acc
  in
  dive [] l

let map f l =
  let rec direct f i l =
    match l with
    | [] -> []
    | [ x ] -> [ f x ]
    | [ x1; x2 ] ->
      let y1 = f x1 in
      [ y1; f x2 ]
    | [ x1; x2; x3 ] ->
      let y1 = f x1 in
      let y2 = f x2 in
      [ y1; y2; f x3 ]
    | _ when i = 0 -> tail_map f l
    | x1 :: x2 :: x3 :: x4 :: l' ->
      let y1 = f x1 in
      let y2 = f x2 in
      let y3 = f x3 in
      let y4 = f x4 in
      y1 :: y2 :: y3 :: y4 :: direct f (i - 1) l'
  in
  direct f direct_depth_default_ l

let append l1 l2 =
  let[@inline] safe l1 l2 = List.rev_append (List.rev l1) l2 in
  let rec direct i l1 l2 =
    match l1 with
    | [] -> l2
    | [ x ] -> x :: l2
    | _ when i = 0 -> safe l1 l2
    | x :: y :: tl1 -> x :: y :: direct (i - 1) tl1 l2
  in
  direct 1000 l1 l2

[@@@eliflt 5.1]

let[@tail_mod_cons] rec map f l =
  match l with
  | [] -> []
  | x :: tl ->
    let x = f x in
    x :: map f tl

let[@tail_mod_cons] rec append l1 l2 =
  match l1 with
  | [] -> l2
  | x :: tl1 -> x :: append tl1 l2

[@@@else_]

(* TRMC functions on >= 5.1, no need to bring our own *)

[@@@endif]

(* Wrapper around [append] to optimize for the case of short [l1],
   and for the case of [l2 = []] (saves the whole copy of [l1]!) *)
let[@inline] append l1 l2 =
  match l1, l2 with
  | [], _ -> l2
  | _, [] -> l1
  | [ x ], _ -> x :: l2
  | x :: y :: tl1, _ -> x :: y :: append tl1 l2

let ( @ ) = append
let[@inline] cons' l x = x :: l

let cons_maybe o l =
  match o with
  | Some x -> x :: l
  | None -> l

let cons_when b x l =
  if b then
    x :: l
  else
    l

[@@@iflt 4.14]

let direct_depth_filter_ = 10_000

let filter p l =
  let rec direct i p l =
    match l with
    | [] -> []
    | _ when i = 0 -> safe p l []
    | x :: l' when not (p x) -> direct i p l'
    | x :: l' -> x :: direct (i - 1) p l'
  and safe p l acc =
    match l with
    | [] -> List.rev acc
    | x :: l' when not (p x) -> safe p l' acc
    | x :: l' -> safe p l' (x :: acc)
  in
  direct direct_depth_filter_ p l

[@@@eliflt 5.1]

let[@tail_mod_cons] rec filter f l =
  match l with
  | [] -> []
  | x :: tl ->
    let keep = f x in
    if keep then
      x :: filter f tl
    else
      filter f tl

[@@@else_]
(* stdlib's filter uses TRMC after 5.1 *)

[@@@endif]

let fold_right f l acc =
  let rec direct i f l acc =
    match l with
    | [] -> acc
    | _ when i = 0 -> safe f (List.rev l) acc
    | x :: l' ->
      let acc = direct (i - 1) f l' acc in
      f x acc
  and safe f l acc =
    match l with
    | [] -> acc
    | x :: l' ->
      let acc = f x acc in
      safe f l' acc
  in
  direct direct_depth_default_ f l acc

let rec fold_while f acc = function
  | [] -> acc
  | e :: l ->
    let acc, cont = f acc e in
    (match cont with
    | `Stop -> acc
    | `Continue -> fold_while f acc l)

let fold_map f acc l =
  let rec aux f acc map_acc l =
    match l with
    | [] -> acc, List.rev map_acc
    | x :: l' ->
      let acc, y = f acc x in
      aux f acc (y :: map_acc) l'
  in
  aux f acc [] l

let fold_map_i f acc l =
  let rec aux f acc i map_acc l =
    match l with
    | [] -> acc, List.rev map_acc
    | x :: l' ->
      let acc, y = f acc i x in
      aux f acc (i + 1) (y :: map_acc) l'
  in
  aux f acc 0 [] l

let fold_on_map ~f ~reduce acc l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | x :: l' ->
      let acc = reduce acc (f x) in
      aux acc l'
  in
  aux acc l

let scan_left f acc l =
  let rec aux f acc l_acc l =
    match l with
    | [] -> List.rev l_acc
    | x :: tail ->
      let acc = f acc x in
      let l_acc = acc :: l_acc in
      aux f acc l_acc tail
  in
  aux f acc [ acc ] l

let reduce f = function
  | [] -> None
  | x :: l -> Some (fold_left f x l)

let reduce_exn f = function
  | [] -> raise (Invalid_argument "CCList.reduce_exn")
  | x :: l -> fold_left f x l

let fold_map2 f acc l1 l2 =
  let rec aux f acc map_acc l1 l2 =
    match l1, l2 with
    | [], [] -> acc, List.rev map_acc
    | [], _ | _, [] -> invalid_arg "fold_map2"
    | x1 :: l1', x2 :: l2' ->
      let acc, y = f acc x1 x2 in
      aux f acc (y :: map_acc) l1' l2'
  in
  aux f acc [] l1 l2

let fold_filter_map f acc l =
  let rec aux f acc map_acc l =
    match l with
    | [] -> acc, List.rev map_acc
    | x :: l' ->
      let acc, y = f acc x in
      aux f acc (cons_maybe y map_acc) l'
  in
  aux f acc [] l

let fold_filter_map_i f acc l =
  let rec aux f acc i map_acc l =
    match l with
    | [] -> acc, List.rev map_acc
    | x :: l' ->
      let acc, y = f acc i x in
      aux f acc (i + 1) (cons_maybe y map_acc) l'
  in
  aux f acc 0 [] l

let fold_flat_map f acc l =
  let rec aux f acc map_acc l =
    match l with
    | [] -> acc, List.rev map_acc
    | x :: l' ->
      let acc, y = f acc x in
      aux f acc (List.rev_append y map_acc) l'
  in
  aux f acc [] l

let fold_flat_map_i f acc l =
  let rec aux f acc i map_acc l =
    match l with
    | [] -> acc, List.rev map_acc
    | x :: l' ->
      let acc, y = f acc i x in
      aux f acc (i + 1) (List.rev_append y map_acc) l'
  in
  aux f acc 0 [] l

[@@@iflt 4.14]

(* keep this because it's tailrec for < 5.1 *)
let init len f =
  let rec indirect_ i acc =
    if i = len then
      List.rev acc
    else (
      let x = f i in
      indirect_ (i + 1) (x :: acc)
    )
  in
  let rec direct_ i =
    if i = len then
      []
    else if i < direct_depth_default_ then (
      let x = f i in
      x :: direct_ (i + 1)
    ) else
      indirect_ i []
  in
  if len < 0 then
    invalid_arg "init"
  else if len = 0 then
    []
  else
    direct_ 0

let rec unfold_kont f seed k =
  match f seed with
  | None -> k []
  | Some (v, next) ->
    let k' tl = k (v :: tl) in
    unfold_kont f next k'

let[@inline] unfold f seed =
  let rec direct i f seed =
    if i = 0 then
      unfold_kont f seed (fun x -> x)
    else (
      match f seed with
      | None -> []
      | Some (v, next) -> v :: direct (i - 1) f next
    )
  in
  direct 100 f seed

[@@@eliflt 5.1]

let init n f =
  let[@tail_mod_cons] rec init_ i n f =
    if i = n then
      []
    else (
      let x = f i in
      x :: init_ (i + 1) n f
    )
  in
  init_ 0 n f

let[@tail_mod_cons] rec unfold f seed =
  match f seed with
  | None -> []
  | Some (v, next) -> v :: unfold f next

[@@@else_]

let[@tail_mod_cons] rec unfold f seed =
  match f seed with
  | None -> []
  | Some (v, next) -> v :: unfold f next

[@@@endif]

let rec compare f l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | x1 :: l1', x2 :: l2' ->
    let c = f x1 x2 in
    if c <> 0 then
      c
    else
      compare f l1' l2'

let rec equal f l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | [], _ | _, [] -> false
  | x1 :: l1', x2 :: l2' -> f x1 x2 && equal f l1' l2'

[@@@iflt 5.1]

let rec flat_map_kont f l kont =
  match l with
  | [] -> kont []
  | [ x ] ->
    let x = f x in
    kont x
  | x :: l' ->
    let x = f x in
    let kont' tail = kont (append x tail) in
    flat_map_kont f l' kont'

let[@inline] flat_map f l =
  match l with
  | [] -> []
  | [ x ] -> f x
  | _ :: _ -> flat_map_kont f l Fun.id

[@@@else_]

let flat_map = concat_map

[@@@endif]

let flat_map_i f l =
  let rec aux f i l kont =
    match l with
    | [] -> kont []
    | x :: l' ->
      let y = f i x in
      let kont' tail =
        match y with
        | [] -> kont tail
        | [ x ] -> kont (x :: tail)
        | [ x; y ] -> kont (x :: y :: tail)
        | l -> kont (append l tail)
      in
      aux f (i + 1) l' kont'
  in
  aux f 0 l (fun l -> l)

let flatten l = fold_right append l []

let count f l =
  fold_left
    (fun n x ->
      if f x then
        succ n
      else
        n)
    0 l

let count_true_false p l =
  fold_left
    (fun (ok, ko) x ->
      if p x then
        ok + 1, ko
      else
        ok, ko + 1)
    (0, 0) l

let[@inline] product f l1 l2 = flat_map (fun x -> map (fun y -> f x y) l2) l1

let fold_product f acc l1 l2 =
  List.fold_left
    (fun acc x1 -> List.fold_left (fun acc x2 -> f acc x1 x2) acc l2)
    acc l1

let diagonal l =
  let rec gen acc l =
    match l with
    | [] -> acc
    | x :: l' ->
      let acc = List.fold_left (fun acc y -> (x, y) :: acc) acc l' in
      gen acc l'
  in
  gen [] l

let partition_map_either f l =
  let rec iter f l1 l2 l =
    match l with
    | [] -> List.rev l1, List.rev l2
    | x :: tl ->
      (match f x with
      | CCEither.Left y -> iter f (y :: l1) l2 tl
      | CCEither.Right y -> iter f l1 (y :: l2) tl)
  in
  iter f [] [] l

let partition_filter_map f l =
  let rec iter f l1 l2 l =
    match l with
    | [] -> List.rev l1, List.rev l2
    | x :: tl ->
      (match f x with
      | `Left y -> iter f (y :: l1) l2 tl
      | `Right y -> iter f l1 (y :: l2) tl
      | `Drop -> iter f l1 l2 tl)
  in
  iter f [] [] l

let partition_map = partition_filter_map

[@@@iflt 4.14]

let combine l1 l2 =
  let rec direct i l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | _ when i = 0 -> safe l1 l2 []
    | x1 :: l1', x2 :: l2' -> (x1, x2) :: direct (i - 1) l1' l2'
    | _, _ -> invalid_arg "CCList.combine"
  and safe l1 l2 acc =
    match l1, l2 with
    | [], [] -> List.rev acc
    | x1 :: l1', x2 :: l2' -> safe l1' l2' @@ ((x1, x2) :: acc)
    | _, _ -> invalid_arg "CCList.combine"
  in
  direct direct_depth_default_ l1 l2

[@@@else_]

let[@tail_mod_cons] rec combine l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | x1 :: l1', x2 :: l2' -> (x1, x2) :: combine l1' l2'
  | _, _ -> invalid_arg "CCList.combine"

[@@@endif]

let combine_gen l1 l2 =
  let l1 = ref l1 in
  let l2 = ref l2 in
  fun () ->
    match !l1, !l2 with
    | [], _ | _, [] -> None
    | x1 :: tail1, x2 :: tail2 ->
      l1 := tail1;
      l2 := tail2;
      Some (x1, x2)

[@@@iflt 4.14]

let combine_shortest l1 l2 =
  let rec direct i l1 l2 =
    match l1, l2 with
    | _, [] | [], _ -> []
    | _ when i = 0 -> safe l1 l2 []
    | x1 :: l1', x2 :: l2' -> (x1, x2) :: direct (i - 1) l1' l2'
  and safe l1 l2 acc =
    match l1, l2 with
    | [], _ | _, [] -> List.rev acc
    | x1 :: l1', x2 :: l2' ->
      let acc = (x1, x2) :: acc in
      safe l1' l2' acc
  in
  direct direct_depth_default_ l1 l2

[@@@else_]

let[@tail_mod_cons] rec combine_shortest l1 l2 =
  match l1, l2 with
  | _, [] | [], _ -> []
  | x1 :: l1', x2 :: l2' -> (x1, x2) :: combine_shortest l1' l2'

[@@@endif]

let split l =
  let rec direct i l =
    match l with
    | [] -> [], []
    | [ (x1, y1) ] -> [ x1 ], [ y1 ]
    | [ (x1, y1); (x2, y2) ] -> [ x1; x2 ], [ y1; y2 ]
    | [ (x1, y1); (x2, y2); (x3, y3) ] -> [ x1; x2; x3 ], [ y1; y2; y3 ]
    | [ (x1, y1); (x2, y2); (x3, y3); (x4, y4) ] ->
      [ x1; x2; x3; x4 ], [ y1; y2; y3; y4 ]
    | _ when i = 0 -> split_slow [] [] l
    | (x1, y1) :: (x2, y2) :: (x3, y3) :: (x4, y4) :: (x5, y5) :: l' ->
      let rx, ry = direct (i - 1) l' in
      x1 :: x2 :: x3 :: x4 :: x5 :: rx, y1 :: y2 :: y3 :: y4 :: y5 :: ry
  and split_slow acc1 acc2 l =
    match l with
    | [] -> List.rev acc1, List.rev acc2
    | (x1, x2) :: tail ->
      let acc1 = x1 :: acc1 and acc2 = x2 :: acc2 in
      split_slow acc1 acc2 tail
  in
  direct direct_depth_default_ l

let return x = [ x ]
let pure = return
let ( <*> ) funs l = product (fun f x -> f x) funs l

let cartesian_product l =
  (* [left]: elements picked so far
     [right]: sets to pick elements from
     [acc]: accumulator for the result, to pass to continuation
     [k]: continuation *)
  let rec prod_rec left right k acc =
    match right with
    | [] -> k acc (List.rev left)
    | l1 :: tail ->
      List.fold_left (fun acc x -> prod_rec (x :: left) tail k acc) acc l1
  in
  prod_rec [] l (fun acc l' -> l' :: acc) []

(* cartesian product of lists of lists *)
let map_product_l f l =
  let l = List.map f l in
  cartesian_product l

let rec sorted_mem ~cmp x l =
  match l with
  | [] -> false
  | y :: tail ->
    (match cmp x y with
    | 0 -> true
    | n when n < 0 -> false
    | _ -> (sorted_mem [@tailcall]) ~cmp x tail)

let sorted_merge ~cmp l1 l2 =
  let rec recurse cmp acc l1 l2 =
    match l1, l2 with
    | [], _ -> List.rev_append acc l2
    | _, [] -> List.rev_append acc l1
    | x1 :: l1', x2 :: l2' ->
      let c = cmp x1 x2 in
      if c < 0 then
        recurse cmp (x1 :: acc) l1' l2
      else if c > 0 then
        recurse cmp (x2 :: acc) l1 l2'
      else
        recurse cmp (x1 :: x2 :: acc) l1' l2'
  in
  recurse cmp [] l1 l2

let sorted_diff ~cmp l1 l2 =
  let rec recurse cmp acc l1 l2 =
    match l1, l2 with
    | [], _ -> List.rev acc
    | _, [] -> List.rev_append acc l1
    | x1 :: l1', x2 :: l2' ->
      let c = cmp x1 x2 in
      if c < 0 then
        recurse cmp (x1 :: acc) l1' l2
      else if c > 0 then
        recurse cmp acc l1 l2'
      else
        recurse cmp acc l1' l2'
  in
  recurse cmp [] l1 l2

let sort_uniq ~cmp l = List.sort_uniq cmp l

let is_sorted ~cmp l =
  let rec aux cmp = function
    | [] | [ _ ] -> true
    | x :: (y :: _ as tail) -> cmp x y <= 0 && aux cmp tail
  in
  aux cmp l

let sorted_insert ~cmp ?(uniq = false) x l =
  let rec aux cmp uniq x left l =
    match l with
    | [] -> List.rev_append left [ x ]
    | y :: tail ->
      (match cmp x y with
      | 0 ->
        let l' =
          if uniq then
            l
          else
            x :: l
        in
        List.rev_append left l'
      | n when n < 0 -> List.rev_append left (x :: l)
      | _ -> aux cmp uniq x (y :: left) tail)
  in
  aux cmp uniq x [] l

let sorted_remove ~cmp ?(all = false) x l =
  let rec aux cmp all x left l =
    match l with
    | [] -> List.rev left
    | y :: tail ->
      (match cmp x y with
      | 0 ->
        if all then
          aux cmp all x left tail
        else
          List.rev_append left tail
      | n when n < 0 -> List.rev_append left l
      | _ -> aux cmp all x (y :: left) tail)
  in
  aux cmp all x [] l

let uniq_succ ~eq l =
  let rec f acc l =
    match l with
    | [] -> List.rev acc
    | [ x ] -> List.rev (x :: acc)
    | x :: (y :: _ as tail) when eq x y -> f acc tail
    | x :: tail -> f (x :: acc) tail
  in
  f [] l

let group_succ ~eq l =
  let rec f ~eq acc cur l =
    match cur, l with
    | [], [] -> List.rev acc
    | _ :: _, [] -> List.rev (List.rev cur :: acc)
    | [], x :: tl -> f ~eq acc [ x ] tl
    | y :: _, x :: tl when eq x y -> f ~eq acc (x :: cur) tl
    | _, x :: tl -> f ~eq (List.rev cur :: acc) [ x ] tl
  in
  f ~eq [] [] l

let sorted_merge_uniq ~cmp l1 l2 =
  let push ~cmp acc x =
    match acc with
    | [] -> [ x ]
    | y :: _ when cmp x y > 0 -> x :: acc
    | _ -> acc
    (* duplicate, do not yield *)
  in
  let rec recurse ~cmp acc l1 l2 =
    match l1, l2 with
    | [], l | l, [] ->
      let acc = List.fold_left (push ~cmp) acc l in
      List.rev acc
    | x1 :: l1', x2 :: l2' ->
      let c = cmp x1 x2 in
      if c < 0 then
        recurse ~cmp (push ~cmp acc x1) l1' l2
      else if c > 0 then
        recurse ~cmp (push ~cmp acc x2) l1 l2'
      else
        recurse ~cmp acc l1 l2'
    (* drop one of the [x] *)
  in
  recurse ~cmp [] l1 l2

let sorted_diff_uniq ~cmp l1 l2 =
  let push ~cmp acc x =
    match acc with
    | [] -> [ x ]
    | y :: _ when cmp x y > 0 -> x :: acc
    | _ -> acc
    (* duplicate, do not yield *)
  in
  let rec recurse ~cmp acc l1 l2 =
    match l1, l2 with
    | [], _ -> List.rev acc
    | l, [] ->
      let acc = List.fold_left (push ~cmp) acc l in
      List.rev acc
    | x1 :: l1', x2 :: l2' ->
      let c = cmp x1 x2 in
      if c < 0 then
        recurse ~cmp (push ~cmp acc x1) l1' l2
      else if c > 0 then
        recurse ~cmp acc l1 l2'
      else
        recurse ~cmp acc l1' l2'
  in
  recurse ~cmp [] l1 l2

[@@@iflt 4.14]

let take n l =
  let rec direct i n l =
    match l with
    | [] -> []
    | _ when i = 0 -> safe n [] l
    | x :: l' ->
      if n > 0 then
        x :: direct (i - 1) (n - 1) l'
      else
        []
  and safe n acc l =
    match l with
    | [] -> List.rev acc
    | _ when n = 0 -> List.rev acc
    | x :: l' -> safe (n - 1) (x :: acc) l'
  in
  direct direct_depth_default_ n l

[@@@else_]

let[@tail_mod_cons] rec take n l =
  match l with
  | [] -> []
  | x :: l' ->
    if n > 0 then
      x :: take (n - 1) l'
    else
      []

[@@@endif]

let rec drop n l =
  match l with
  | [] -> []
  | _ when n = 0 -> l
  | _ :: l' -> drop (n - 1) l'

let hd_tl = function
  | [] -> failwith "hd_tl"
  | x :: l -> x, l

let take_drop n l = take n l, drop n l

let sublists_of_len ?(last = fun _ -> None) ?offset n l =
  if n < 1 then invalid_arg "sublists_of_len: n must be > 0";
  let offset =
    match offset with
    | None -> n
    | Some o when o < 1 -> invalid_arg "sublists_of_len: offset must be > 0"
    | Some o -> o
  in
  (* add sub-lists of [l] to [acc] *)
  let rec aux acc l =
    let group = take n l in
    if is_empty group then
      acc
    (* this was the last group, we are done *)
    else if List.length group < n (* last group, with missing elements *) then (
      match last group with
      | None -> acc
      | Some group' -> group' :: acc
    ) else (
      let l' = drop offset l in
      aux (group :: acc) l' (* continue *)
    )
  in
  List.rev (aux [] l)

let chunks n l = sublists_of_len ~last:(fun x -> Some x) n l

let intersperse x l =
  let rec aux_direct i x l =
    match l with
    | [] -> []
    | [ _ ] -> l
    | _ when i = 0 -> aux_tailrec [] x l
    | y :: tail -> y :: x :: aux_direct (i - 1) x tail
  and aux_tailrec acc x l =
    match l with
    | [] -> List.rev acc
    | [ y ] -> List.rev (y :: acc)
    | y :: tail -> aux_tailrec (x :: y :: acc) x tail
  in
  aux_direct 1_000 x l

let interleave l1 l2 : _ list =
  let rec aux acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], _ -> List.rev (List.rev_append l2 acc)
    | _, [] -> List.rev (List.rev_append l1 acc)
    | x1 :: tl1, x2 :: tl2 -> aux (x2 :: x1 :: acc) tl1 tl2
  in
  aux [] l1 l2

[@@@iflt 4.14]

let take_while p l =
  let rec direct i p l =
    match l with
    | [] -> []
    | _ when i = 0 -> safe p [] l
    | x :: l' ->
      if p x then
        x :: direct (i - 1) p l'
      else
        []
  and safe p acc l =
    match l with
    | [] -> List.rev acc
    | x :: l' ->
      if p x then
        safe p (x :: acc) l'
      else
        List.rev acc
  in
  direct direct_depth_default_ p l

[@@@else_]

let[@tail_mod_cons] rec take_while p l =
  match l with
  | [] -> []
  | x :: l' ->
    if p x then
      x :: take_while p l'
    else
      []

[@@@endif]

let rec drop_while p l =
  match l with
  | [] -> []
  | x :: l' ->
    if p x then
      drop_while p l'
    else
      l

let take_drop_while p l =
  let rec direct i p l =
    match l with
    | [] -> [], []
    | _ when i = 0 -> safe p [] l
    | x :: tail ->
      if p x then (
        let l1, l2 = direct (i - 1) p tail in
        x :: l1, l2
      ) else
        [], l
  and safe p acc l =
    match l with
    | [] -> List.rev acc, []
    | x :: tail ->
      if p x then
        safe p (x :: acc) tail
      else
        List.rev acc, l
  in
  direct direct_depth_default_ p l

let last n l =
  let len = List.length l in
  if len < n then
    l
  else
    drop (len - n) l

let head_opt = function
  | [] -> None
  | x :: _ -> Some x

let tail_opt = function
  | [] -> None
  | _ :: tail -> Some tail

let rec last_opt = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: tail -> last_opt tail

let find_pred = find_opt

let find_pred_exn p l =
  match find_pred p l with
  | None -> raise Not_found
  | Some x -> x

let find_mapi f l =
  let rec aux f i = function
    | [] -> None
    | x :: l' ->
      (match f i x with
      | Some _ as res -> res
      | None -> aux f (i + 1) l')
  in
  aux f 0 l

let find_map f l = find_mapi (fun _ -> f) l

let find_idx p l =
  find_mapi
    (fun i x ->
      if p x then
        Some (i, x)
      else
        None)
    l

let remove ~eq x l =
  let rec remove' eq x acc l =
    match l with
    | [] -> List.rev acc
    | y :: tail when eq x y -> remove' eq x acc tail
    | y :: tail -> remove' eq x (y :: acc) tail
  in
  remove' eq x [] l

let filter_map f l =
  let rec recurse acc l =
    match l with
    | [] -> List.rev acc
    | x :: l' ->
      let acc' =
        match f x with
        | None -> acc
        | Some y -> y :: acc
      in
      recurse acc' l'
  in
  recurse [] l

let keep_some l = filter_map (fun x -> x) l

let keep_ok l =
  filter_map
    (function
      | Ok x -> Some x
      | Error _ -> None)
    l

let all_some l =
  try
    Some
      (map
         (function
           | Some x -> x
           | None -> raise Exit)
         l)
  with Exit -> None

let all_ok l =
  let err = ref None in
  try
    Ok
      (map
         (function
           | Ok x -> x
           | Error e ->
             err := Some e;
             raise Exit)
         l)
  with Exit ->
    (match !err with
    | None -> assert false
    | Some e -> Error e)

let split_result results =
  results
  |> partition_filter_map (fun x ->
         match x with
         | Ok o -> `Left o
         | Error e -> `Right e)

let group_by (type k) ?(hash = Hashtbl.hash) ?(eq = Stdlib.( = )) l =
  let module Tbl = Hashtbl.Make (struct
    type t = k

    let equal = eq
    let hash = hash
  end) in
  (* compute group table *)
  let tbl = Tbl.create 32 in
  List.iter
    (fun x ->
      let l = try Tbl.find tbl x with Not_found -> [] in
      Tbl.replace tbl x (x :: l))
    l;
  Tbl.fold (fun _ x acc -> x :: acc) tbl []

let join ~join_row s1 s2 : _ t =
  flat_map (fun a -> filter_map (join_row a) s2) s1

let join_by (type a) ?(eq = Stdlib.( = )) ?(hash = Hashtbl.hash) f1 f2 ~merge c1
    c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  List.iter
    (fun x ->
      let key = f1 x in
      Tbl.add tbl key x)
    c1;
  let res = ref [] in
  List.iter
    (fun y ->
      let key = f2 y in
      let xs = Tbl.find_all tbl key in
      List.iter
        (fun x ->
          match merge key x y with
          | None -> ()
          | Some z -> res := z :: !res)
        xs)
    c2;
  !res

type ('a, 'b) join_all_cell = {
  mutable ja_left: 'a list;
  mutable ja_right: 'b list;
}

let join_all_by (type a) ?(eq = Stdlib.( = )) ?(hash = Hashtbl.hash) f1 f2
    ~merge c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  (* build the map [key -> cell] *)
  List.iter
    (fun x ->
      let key = f1 x in
      try
        let c = Tbl.find tbl key in
        c.ja_left <- x :: c.ja_left
      with Not_found -> Tbl.add tbl key { ja_left = [ x ]; ja_right = [] })
    c1;
  List.iter
    (fun y ->
      let key = f2 y in
      try
        let c = Tbl.find tbl key in
        c.ja_right <- y :: c.ja_right
      with Not_found -> Tbl.add tbl key { ja_left = []; ja_right = [ y ] })
    c2;
  Tbl.fold
    (fun key cell res ->
      match merge key cell.ja_left cell.ja_right with
      | None -> res
      | Some z -> z :: res)
    tbl []

let group_join_by (type a) ?(eq = Stdlib.( = )) ?(hash = Hashtbl.hash) f c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  List.iter (fun x -> Tbl.replace tbl x []) c1;
  List.iter
    (fun y ->
      (* project [y] into some element of [c1] *)
      let key = f y in
      try
        let l = Tbl.find tbl key in
        Tbl.replace tbl key (y :: l)
      with Not_found -> ())
    c2;
  Tbl.fold (fun k v l -> (k, v) :: l) tbl []

let mem ?(eq = Stdlib.( = )) x l =
  let rec search eq x l =
    match l with
    | [] -> false
    | y :: l' -> eq x y || search eq x l'
  in
  search eq x l

let add_nodup ~eq x l =
  if mem ~eq x l then
    l
  else
    x :: l

let remove_one ~eq x l =
  let rec remove_one ~eq x acc l =
    match l with
    | [] -> assert false
    | y :: tl when eq x y -> List.rev_append acc tl
    | y :: tl -> remove_one ~eq x (y :: acc) tl
  in
  if mem ~eq x l then
    remove_one ~eq x [] l
  else
    l

let subset ~eq l1 l2 = List.for_all (fun t -> mem ~eq t l2) l1

let uniq ~eq l =
  let rec uniq eq acc l =
    match l with
    | [] -> List.rev acc
    | x :: xs when List.exists (eq x) xs -> uniq eq acc xs
    | x :: xs -> uniq eq (x :: acc) xs
  in
  uniq eq [] l

let union ~eq l1 l2 =
  let rec union eq acc l1 l2 =
    match l1 with
    | [] -> List.rev_append acc l2
    | x :: xs when mem ~eq x l2 -> union eq acc xs l2
    | x :: xs -> union eq (x :: acc) xs l2
  in
  union eq [] l1 l2

let inter ~eq l1 l2 =
  let rec inter eq acc l1 l2 =
    match l1 with
    | [] -> List.rev acc
    | x :: xs when mem ~eq x l2 -> inter eq (x :: acc) xs l2
    | _ :: xs -> inter eq acc xs l2
  in
  inter eq [] l1 l2

let mapi f l =
  let r = ref 0 in
  map
    (fun x ->
      let y = f !r x in
      incr r;
      y)
    l

let iteri f l =
  let rec aux f i l =
    match l with
    | [] -> ()
    | x :: l' ->
      f i x;
      aux f (i + 1) l'
  in
  aux f 0 l

let iteri2 f l1 l2 =
  let rec aux f i l1 l2 =
    match l1, l2 with
    | [], [] -> ()
    | [], _ | _, [] -> invalid_arg "iteri2"
    | x1 :: l1', x2 :: l2' ->
      f i x1 x2;
      aux f (i + 1) l1' l2'
  in
  aux f 0 l1 l2

let foldi f acc l =
  let rec foldi f acc i l =
    match l with
    | [] -> acc
    | x :: l' ->
      let acc = f acc i x in
      foldi f acc (i + 1) l'
  in
  foldi f acc 0 l

let foldi2 f acc l1 l2 =
  let rec foldi f acc i l1 l2 =
    match l1, l2 with
    | [], [] -> acc
    | [], _ | _, [] -> invalid_arg "foldi2"
    | x1 :: l1', x2 :: l2' ->
      let acc = f acc i x1 x2 in
      foldi f acc (i + 1) l1' l2'
  in
  foldi f acc 0 l1 l2

let rec get_at_idx_rec i l =
  match l with
  | [] -> raise Not_found
  | x :: _ when i = 0 -> x
  | _ :: l' -> get_at_idx_rec (i - 1) l'

let get_at_idx_exn i l =
  let i =
    if i < 0 then
      length l + i
    else
      i
  in
  get_at_idx_rec i l

let get_at_idx i l = try Some (get_at_idx_exn i l) with Not_found -> None

let set_at_idx i x l0 =
  let rec aux l acc i =
    match l with
    | [] -> l0
    | _ :: l' when i = 0 -> List.rev_append acc (x :: l')
    | y :: l' -> aux l' (y :: acc) (i - 1)
  in
  let i =
    if i < 0 then
      length l0 + i
    else
      i
  in
  aux l0 [] i

let insert_at_idx i x l =
  let rec aux l acc i x =
    match l with
    | [] -> List.rev_append acc [ x ]
    | y :: l' when i = 0 -> List.rev_append acc (x :: y :: l')
    | y :: l' -> aux l' (y :: acc) (i - 1) x
  in
  let i =
    if i < 0 then
      length l + i
    else
      i
  in
  aux l [] i x

let remove_at_idx i l0 =
  let rec aux l acc i =
    match l with
    | [] -> l0
    | _ :: l' when i = 0 -> List.rev_append acc l'
    | y :: l' -> aux l' (y :: acc) (i - 1)
  in
  let i =
    if i < 0 then
      length l0 + i
    else
      i
  in
  aux l0 [] i

let range_by ~step i j =
  let rec range i j acc =
    if i = j then
      i :: acc
    else
      range i (j - step) (j :: acc)
  in
  if step = 0 then
    raise (Invalid_argument "CCList.range_by")
  else if
    if step > 0 then
      i > j
    else
      i < j
  then
    []
  else
    range i (((j - i) / step * step) + i) []

let range i j =
  let rec up i j acc =
    if i = j then
      i :: acc
    else
      up i (j - 1) (j :: acc)
  and down i j acc =
    if i = j then
      i :: acc
    else
      down i (j + 1) (j :: acc)
  in
  if i <= j then
    up i j []
  else
    down i j []

let range' i j =
  if i < j then
    range i (j - 1)
  else if i = j then
    []
  else
    range i (j + 1)

let ( -- ) = range
let ( --^ ) = range'

let replicate i x =
  let rec aux acc i =
    if i = 0 then
      acc
    else
      aux (x :: acc) (i - 1)
  in
  aux [] i

let repeat i l =
  let rec aux acc i =
    if i = 0 then
      List.rev acc
    else
      aux (List.rev_append l acc) (i - 1)
  in
  aux [] i

module Assoc = struct
  type ('a, 'b) t = ('a * 'b) list

  let rec search_exn eq l x =
    match l with
    | [] -> raise Not_found
    | (y, z) :: l' ->
      if eq x y then
        z
      else
        search_exn eq l' x

  let get_exn ~eq x l = search_exn eq l x
  let get ~eq x l = try Some (search_exn eq l x) with Not_found -> None

  (* search for a binding for [x] in [l], and calls [f x (Some v) rest]
     or [f x None rest] depending on whether it finds the binding.
     [rest] is the list of the other bindings *)
  let rec search_set eq acc l x ~f =
    match l with
    | [] -> f x None acc
    | (x', y') :: l' ->
      if eq x x' then
        f x (Some y') (List.rev_append acc l')
      else
        search_set eq ((x', y') :: acc) l' x ~f

  let set ~eq x y l = search_set eq [] l x ~f:(fun x _ l -> (x, y) :: l)

  let mem ?(eq = Stdlib.( = )) x l =
    try
      ignore (search_exn eq l x);
      true
    with Not_found -> false

  let update ~eq f x l =
    search_set eq [] l x ~f:(fun x opt_y rest ->
        match f opt_y with
        | None -> rest (* drop *)
        | Some y' -> (x, y') :: rest)

  let remove ~eq x l =
    search_set eq [] l x ~f:(fun _ opt_y rest ->
        match opt_y with
        | None -> l (* keep as is *)
        | Some _ -> rest)

  let keys l = map (fun (k, _) -> k) l
  let values l = map (fun (_, v) -> v) l
  let map_values f l = map (fun (k, v) -> k, f v) l
end

let assoc = Assoc.get_exn
let assoc_opt = Assoc.get
let mem_assoc = Assoc.mem
let remove_assoc = Assoc.remove

(** {2 References on Lists} *)

module Ref = struct
  type 'a t = 'a list ref

  let push l x = l := x :: !l

  let pop l =
    match !l with
    | [] -> None
    | x :: tail ->
      l := tail;
      Some x

  let pop_exn l =
    match !l with
    | [] -> failwith "CCList.Ref.pop_exn"
    | x :: tail ->
      l := tail;
      x

  let create () = ref []
  let clear l = l := []
  let lift f l = f !l
  let push_list r l = r := List.rev_append l !r
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse (M : MONAD) = struct
  open M

  let map_m f l =
    let rec aux f acc l =
      match l with
      | [] -> return (List.rev acc)
      | x :: tail -> f x >>= fun x' -> aux f (x' :: acc) tail
    in
    aux f [] l

  let rec map_m_par f l =
    match l with
    | [] -> M.return []
    | x :: tl ->
      let x' = f x in
      let tl' = map_m_par f tl in
      x' >>= fun x' ->
      tl' >>= fun tl' -> M.return (x' :: tl')

  let sequence_m l = map_m (fun x -> x) l

  let rec fold_m f acc l =
    match l with
    | [] -> return acc
    | x :: l' -> f acc x >>= fun acc' -> fold_m f acc' l'
end

(** {2 Conversions} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let random_len len g st = init len (fun _ -> g st)

let random g st =
  let len = Random.State.int st 1_000 in
  random_len len g st

let random_non_empty g st =
  let len = 1 + Random.State.int st 1_000 in
  random_len len g st

let random_choose l =
  match l with
  | [] -> raise Not_found
  | _ :: _ ->
    let len = List.length l in
    fun st ->
      let i = Random.State.int st len in
      List.nth l i

let random_sequence l st = map (fun g -> g st) l

let to_string ?(start = "") ?(stop = "") ?(sep = ", ") item_to_string l =
  let l = List.map item_to_string l in
  start ^ String.concat sep l ^ stop

let to_iter l k = List.iter k l

let rec to_seq l () =
  match l with
  | [] -> Seq.Nil
  | x :: tl -> Seq.Cons (x, to_seq tl)

let of_iter i =
  let l = ref [] in
  i (fun x -> l := x :: !l);
  List.rev !l

let of_seq_rev l =
  let rec loop acc s =
    match s () with
    | Seq.Nil -> acc
    | Seq.Cons (x, tl) -> loop (x :: acc) tl
  in
  loop [] l

[@@@iflt 4.14]

let of_seq l =
  let rec direct i seq =
    if i <= 0 then
      List.rev (of_seq_rev seq)
    else (
      match seq () with
      | Seq.Nil -> []
      | Seq.Cons (x, tl) -> x :: direct (i - 1) tl
    )
  in
  direct direct_depth_default_ l

[@@@endif]

let to_gen l =
  let l = ref l in
  fun () ->
    match !l with
    | [] -> None
    | x :: l' ->
      l := l';
      Some x

[@@@iflt 4.14]

let of_gen g =
  let rec direct i g =
    if i = 0 then
      safe [] g
    else (
      match g () with
      | None -> []
      | Some x -> x :: direct (i - 1) g
    )
  and safe acc g =
    match g () with
    | None -> List.rev acc
    | Some x -> safe (x :: acc) g
  in
  direct direct_depth_default_ g

[@@@else_]

let[@tail_mod_cons] rec of_gen g =
  match g () with
  | None -> []
  | Some x -> x :: of_gen g

[@@@endif]

module Infix = struct
  let[@inline] ( >|= ) l f = map f l
  let[@inline] ( >>= ) l f = flat_map f l
  let ( @ ) = ( @ )
  let ( <*> ) = ( <*> )
  let ( <$> ) = map
  let ( -- ) = ( -- )
  let ( --^ ) = ( --^ )
  let ( let+ ) = ( >|= )
  let ( let* ) = ( >>= )
  let[@inline] ( and+ ) l1 l2 = product (fun x y -> x, y) l1 l2
  let ( and* ) = ( and+ )
  let ( and& ) = combine_shortest
end

include Infix

(** {2 IO} *)

let pp ?(pp_start = fun _ () -> ()) ?(pp_stop = fun _ () -> ())
    ?(pp_sep = fun fmt () -> Format.fprintf fmt ",@ ") pp_item fmt l =
  let rec print fmt l =
    match l with
    | x :: (_ :: _ as l) ->
      pp_item fmt x;
      pp_sep fmt ();
      print fmt l
    | [ x ] -> pp_item fmt x
    | [] -> ()
  in
  pp_start fmt ();
  print fmt l;
  pp_stop fmt ()
