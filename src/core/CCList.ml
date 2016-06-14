
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 complements to list} *)

(*$inject
  let lsort l = List.sort Pervasives.compare l
*)

type 'a t = 'a list

let empty = []

let is_empty = function
  | [] -> true
  | _::_ -> false

(* max depth for direct recursion *)
let direct_depth_default_ = 1000

let map f l =
  let rec direct f i l = match l with
    | [] -> []
    | [x] -> [f x]
    | [x1;x2] -> let y1 = f x1 in [y1; f x2]
    | [x1;x2;x3] -> let y1 = f x1 in let y2 = f x2 in [y1; y2; f x3]
    | _ when i=0 -> List.rev (List.rev_map f l)
    | x1::x2::x3::x4::l' ->
        let y1 = f x1 in
        let y2 = f x2 in
        let y3 = f x3 in
        let y4 = f x4 in
        y1 :: y2 :: y3 :: y4 :: direct f (i-1) l'
  in
  direct f direct_depth_default_ l

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let f x = x+1 in \
    List.rev (List.rev_map f l) = map f l)
*)

let (>|=) l f = map f l

let direct_depth_append_ = 10_000

let cons x l = x::l

let append l1 l2 =
  let rec direct i l1 l2 = match l1 with
    | [] -> l2
    | _ when i=0 -> safe l1 l2
    | x::l1' -> x :: direct (i-1) l1' l2
  and safe l1 l2 =
    List.rev_append (List.rev l1) l2
  in
  match l1 with
  | [] -> l2
  | [x] -> x::l2
  | [x;y] -> x::y::l2
  | _ -> direct direct_depth_append_ l1 l2

let (@) = append

(*$T
  [1;2;3] @ [4;5;6] = [1;2;3;4;5;6]
  (1-- 10_000) @ (10_001 -- 20_000) = 1 -- 20_000
*)

let cons_maybe o l = match o with
  | Some x -> x :: l
  | None -> l

(*$T
  cons_maybe (Some 1) [2;3] = [1;2;3]
  cons_maybe None [2;3] = [2;3]
*)

let direct_depth_filter_ = 10_000

let filter p l =
  let rec direct i p l = match l with
    | [] -> []
    | _ when i=0 -> safe p l []
    | x::l' when not (p x) -> direct i p l'
    | x::l' -> x :: direct (i-1) p l'
  and safe p l acc = match l with
    | [] -> List.rev acc
    | x::l' when not (p x) -> safe p l' acc
    | x::l' -> safe p l' (x::acc)
  in
  direct direct_depth_filter_ p l

(*$= & ~printer:CCInt.to_string
  500 (filter (fun x->x mod 2 = 0) (1 -- 1000) |> List.length)
  50_000 (filter (fun x->x mod 2 = 0) (1 -- 100_000) |> List.length)
  500_000 (filter (fun x->x mod 2 = 0) (1 -- 1_000_000) |> List.length)
*)

let fold_right f l acc =
  let rec direct i f l acc = match l with
    | [] -> acc
    | _ when i=0 -> safe f (List.rev l) acc
    | x::l' ->
        let acc = direct (i-1) f l' acc in
        f x acc
  and safe f l acc = match l with
    | [] -> acc
    | x::l' ->
        let acc = f x acc in
        safe f l' acc
  in
  direct direct_depth_default_ f l acc

(*$T
  fold_right (+) (1 -- 1_000_000) 0 = \
    List.fold_left (+) 0 (1 -- 1_000_000)
*)

(*$Q
  (Q.list Q.small_int) (fun l -> \
    l = fold_right (fun x y->x::y) l [])
*)

let rec fold_while f acc = function
  | [] -> acc
  | e::l -> let acc, cont = f acc e in
    match cont with
    | `Stop -> acc
    | `Continue -> fold_while f acc l

(*$T
  fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0 [true;true;false;true] = 2
*)

let fold_map f acc l =
  let rec aux f acc map_acc l = match l with
    | [] -> acc, List.rev map_acc
    | x :: l' ->
        let acc, y = f acc x in
        aux f acc (y :: map_acc) l'
  in
  aux f acc [] l

(*$=
  (6, ["1"; "2"; "3"]) \
    (fold_map (fun acc x->acc+x, string_of_int x) 0 [1;2;3])
*)

(*$Q
  Q.(list int) (fun l -> \
    fold_map (fun acc x -> x::acc, x) [] l = (List.rev l, l))
*)

let fold_map2 f acc l1 l2 =
  let rec aux f acc map_acc l1 l2 = match l1, l2 with
    | [], [] -> acc, List.rev map_acc
    | [], _
    | _, [] -> invalid_arg "fold_map2"
    | x1 :: l1', x2 :: l2' ->
        let acc, y = f acc x1 x2 in
        aux f acc (y :: map_acc) l1' l2'
  in
  aux f acc [] l1 l2

(*$=
  (310, ["1 10"; "2 0"; "3 100"]) \
    (fold_map2 (fun acc x y->acc+x*y, string_of_int x ^ " " ^ string_of_int y) \
    0 [1;2;3] [10;0;100])
*)

(*$T
  (try ignore (fold_map2 (fun _ _ _ -> assert false) 42 [] [1]); false \
   with Invalid_argument _ -> true)
*)

let fold_filter_map f acc l =
  let rec aux f acc map_acc l = match l with
    | [] -> acc, List.rev map_acc
    | x :: l' ->
        let acc, y = f acc x in
        aux f acc (cons_maybe y map_acc) l'
  in
  aux f acc [] l

(*$= & ~printer:Q.Print.(pair int (list int))
  (List.fold_left (+) 0 (1--10), [2;4;6;8;10]) \
  (fold_filter_map (fun acc x -> acc+x, if x mod 2 = 0 then Some x else None) \
    0 (1--10))
*)

let fold_flat_map f acc l =
  let rec aux f acc map_acc l = match l with
    | [] -> acc, List.rev map_acc
    | x :: l' ->
        let acc, y = f acc x in
        aux f acc (List.rev_append y map_acc) l'
  in
  aux f acc [] l

(*$=
  (6, ["1"; "a1"; "2"; "a2"; "3"; "a3"]) \
    (let pf = Printf.sprintf in \
      fold_flat_map (fun acc x->acc+x, [pf "%d" x; pf "a%d" x]) 0 [1;2;3])
*)

(*$Q
  Q.(list int) (fun l -> \
    fold_flat_map (fun acc x -> x::acc, [x;x+10]) [] l = \
      (List.rev l, flat_map (fun x->[x;x+10]) l) )
*)

let init len f =
  let rec init_rec acc i f =
    if i=0 then f i :: acc
    else init_rec (f i :: acc) (i-1) f
  in
  if len<0 then invalid_arg "init"
  else if len=0 then []
  else init_rec [] (len-1) f

(*$T
  init 0 (fun _ -> 0) = []
  init 1 (fun x->x) = [0]
  init 1000 (fun x->x) = 0--999
*)

let rec compare f l1 l2 = match l1, l2 with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | x1::l1', x2::l2' ->
      let c = f x1 x2 in
      if c <> 0 then c else compare f l1' l2'

let rec equal f l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _ | _, [] -> false
  | x1::l1', x2::l2' -> f x1 x2 && equal f l1' l2'

(*$T
  equal CCInt.equal (1--1_000_000) (1--1_000_000)
*)

let flat_map f l =
  let rec aux f l kont = match l with
    | [] -> kont []
    | x::l' ->
        let y = f x in
        let kont' tail = match y with
          | [] -> kont tail
          | [x] -> kont (x :: tail)
          | [x;y] -> kont (x::y::tail)
          | l -> kont (append l tail)
        in
        aux f l' kont'
  in
  aux f l (fun l->l)

(*$T
  flat_map (fun x -> [x+1; x*2]) [10;100] = [11;20;101;200]
  List.length (flat_map (fun x->[x]) (1--300_000)) = 300_000
*)

let flatten l = fold_right append l []

(*$T
  flatten [[1]; [2;3;4]; []; []; [5;6]] = 1--6
  flatten (init 300_001 (fun x->[x])) = 0--300_000
*)

let product f l1 l2 =
  flat_map (fun x -> map (fun y -> f x y) l2) l1

let fold_product f acc l1 l2 =
  List.fold_left
    (fun acc x1 ->
      List.fold_left
        (fun acc x2 -> f acc x1 x2)
        acc l2
    ) acc l1

let diagonal l =
  let rec gen acc l = match l with
  | [] -> acc
  | x::l' ->
    let acc = List.fold_left (fun acc y -> (x,y) :: acc) acc l' in
    gen acc l'
  in
  gen [] l

(*$T
  diagonal [] = []
  diagonal [1] = []
  diagonal [1;2] = [1,2]
  diagonal [1;2;3] |> List.sort Pervasives.compare = [1, 2; 1, 3; 2, 3]
*)

let partition_map f l =
  let rec iter f l1 l2 l = match l with
  | [] -> List.rev l1, List.rev l2
  | x :: tl ->
    match f x with
    | `Left y -> iter f (y :: l1) l2 tl
    | `Right y -> iter f l1 (y :: l2) tl
    | `Drop -> iter f l1 l2 tl
  in
  iter f [] [] l

(*$R
  let l1, l2 =
    partition_map (function
      | n when n = 0 -> `Drop
      | n when n mod 2 = 0 -> `Left n
      | n -> `Right n
    ) [0;1;2;3;4]
  in
  assert_equal [2;4] l1;
  assert_equal [1;3] l2
*)

let return x = [x]

let (>>=) l f = flat_map f l

let (<$>) = map

let pure = return

let (<*>) funs l = product (fun f x -> f x) funs l

let sorted_merge ?(cmp=Pervasives.compare) l1 l2 =
  let rec recurse cmp acc l1 l2 = match l1,l2 with
    | [], _ -> List.rev_append acc l2
    | _, [] -> List.rev_append acc l1
    | x1::l1', x2::l2' ->
      let c = cmp x1 x2 in
      if c < 0 then recurse cmp (x1::acc) l1' l2
      else if c > 0 then recurse cmp (x2::acc) l1 l2'
      else recurse cmp (x1::x2::acc) l1' l2'
  in
  recurse cmp [] l1 l2

(*$T
  List.sort Pervasives.compare ([(( * )2); ((+)1)] <*> [10;100]) \
    = [11; 20; 101; 200]
  sorted_merge [1;1;2] [1;2;3] = [1;1;1;2;2;3]
*)

(*$Q
  Q.(pair (list int) (list int)) (fun (l1,l2) -> \
    List.length (sorted_merge l1 l2) = List.length l1 + List.length l2)
*)

let sort_uniq (type elt) ?(cmp=Pervasives.compare) l =
  let module S = Set.Make(struct
    type t = elt
    let compare = cmp
  end) in
  let set = fold_right S.add l S.empty in
  S.elements set

(*$T
  sort_uniq [1;2;5;3;6;1;4;2;3] = [1;2;3;4;5;6]
  sort_uniq [] = []
  sort_uniq [10;10;10;10;1;10] = [1;10]
*)

let is_sorted ?(cmp=Pervasives.compare) l =
  let rec aux cmp = function
    | [] | [_] -> true
    | x :: ((y :: _) as tail) -> cmp x y <= 0 && aux cmp tail
  in
  aux cmp l

(*$Q
  Q.(list small_int) (fun l -> \
    is_sorted (List.sort Pervasives.compare l))
*)

let sorted_insert ?(cmp=Pervasives.compare) ?(uniq=false) x l =
  let rec aux cmp uniq x left l = match l with
    | [] -> List.rev_append left [x]
    | y :: tail ->
      match cmp x y with
        | 0 ->
          let l' = if uniq then l else x :: l in
          List.rev_append left l'
        | n when n<0 -> List.rev_append left (x :: l)
        | _ -> aux cmp uniq x (y::left) tail
  in
  aux cmp uniq x [] l

(*$Q
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      is_sorted (sorted_insert ~uniq:true x l))
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      is_sorted (sorted_insert ~uniq:false x l))
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      let l' = sorted_insert ~uniq:false x l in \
      List.length l' = List.length l + 1)
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      List.mem x (sorted_insert x l))
*)

let uniq_succ ?(eq=(=)) l =
  let rec f acc l = match l with
    | [] -> List.rev acc
    | [x] -> List.rev (x::acc)
    | x :: ((y :: _) as tail) when eq x y -> f acc tail
    | x :: tail -> f (x::acc) tail
  in
  f [] l

(*$T
  uniq_succ [1;1;2;3;1;6;6;4;6;1] = [1;2;3;1;6;4;6;1]
*)

let group_succ ?(eq=(=)) l =
  let rec f ~eq acc cur l = match cur, l with
    | [], [] -> List.rev acc
    | _::_, [] -> List.rev (List.rev cur :: acc)
    | [], x::tl -> f ~eq acc [x] tl
    | (y :: _), x :: tl when eq x y -> f ~eq acc (x::cur) tl
    | _, x :: tl -> f ~eq (List.rev cur :: acc) [x] tl
  in
  f ~eq [] [] l

(*$T
  group_succ [1;2;3;1;1;2;4] = [[1]; [2]; [3]; [1;1]; [2]; [4]]
  group_succ [] = []
  group_succ [1;1;1] = [[1;1;1]]
  group_succ [1;2;2;2] = [[1]; [2;2;2]]
  group_succ ~eq:(fun (x,_)(y,_)-> x=y) [1, 1; 1, 2; 1, 3; 2, 0] \
    = [[1, 1; 1, 2; 1, 3]; [2, 0]]
*)

let sorted_merge_uniq ?(cmp=Pervasives.compare) l1 l2 =
  let push ~cmp acc x = match acc with
    | [] -> [x]
    | y :: _ when cmp x y > 0 -> x :: acc
    | _ -> acc (* duplicate, do not yield *)
  in
  let rec recurse ~cmp acc l1 l2 = match l1,l2 with
    | [], l
    | l, [] ->
      let acc = List.fold_left (push ~cmp) acc l in
      List.rev acc
    | x1::l1', x2::l2' ->
      let c = cmp x1 x2 in
      if c < 0 then recurse ~cmp (push ~cmp acc x1) l1' l2
      else if c > 0 then recurse ~cmp (push ~cmp acc x2) l1 l2'
      else recurse ~cmp acc l1 l2' (* drop one of the [x] *)
  in
  recurse ~cmp [] l1 l2

(*$T
  sorted_merge_uniq [1; 1; 2; 3; 5; 8] [1; 2; 3; 4; 6; 8; 9; 9] = [1;2;3;4;5;6;8;9]
*)

(*$Q
  Q.(list int) (fun l -> \
    let l = List.sort Pervasives.compare l in \
    sorted_merge_uniq l [] = uniq_succ l)
  Q.(list int) (fun l -> \
    let l = List.sort Pervasives.compare l in \
    sorted_merge_uniq [] l = uniq_succ l)
  Q.(pair (list int) (list int)) (fun (l1, l2) -> \
    let l1 = List.sort Pervasives.compare l1 \
    and l2 = List.sort Pervasives.compare l2 in \
    let l3 = sorted_merge_uniq l1 l2 in \
    uniq_succ l3 = l3)
*)

let take n l =
  let rec direct i n l = match l with
    | [] -> []
    | _ when i=0 -> safe n [] l
    | x::l' ->
        if n > 0
        then x :: direct (i-1) (n-1) l'
        else []
  and safe n acc l = match l with
    | [] -> List.rev acc
    | _ when n=0 -> List.rev acc
    | x::l' -> safe (n-1) (x::acc) l'
  in
  direct direct_depth_default_ n l

(*$T
  take 2 [1;2;3;4;5] = [1;2]
  take 10_000 (range 0 100_000) |> List.length = 10_000
  take 10_000 (range 0 2_000) = range 0 2_000
  take 300_000 (1 -- 400_000) = 1 -- 300_000
*)

(*$Q
  (Q.pair (Q.list Q.small_int) Q.int) (fun (l,i) -> \
    let i = abs i in \
    let l1 = take i l in \
    List.length l1 <= i && ((List.length l1 = i) = (List.length l >= i)))
*)

let rec drop n l = match l with
  | [] -> []
  | _ when n=0 -> l
  | _::l' -> drop (n-1) l'

let hd_tl = function
  | [] -> failwith "hd_tl"
  | x :: l -> x, l

(*$T
  try ignore (hd_tl []); false with Failure _ -> true
  hd_tl [1;2;3] = (1, [2;3])
*)

let take_drop n l = take n l, drop n l

let split = take_drop

(*$Q
  (Q.pair (Q.list Q.small_int) Q.int) (fun (l,i) -> \
    let i = abs i in \
    let l1, l2 = take_drop i l in \
    l1 @ l2 = l )
*)

let take_while p l =
  let rec direct i p l = match l with
    | [] -> []
    | _ when i=0 -> safe p [] l
    | x :: l' ->
        if p x then x :: direct (i-1) p l' else []
  and safe p acc l = match l with
    | [] -> List.rev acc
    | x :: l' ->
        if p x then safe p (x::acc) l' else List.rev acc
  in
  direct direct_depth_default_ p l

(*$T
  take_while (fun x->x<10) (1 -- 20) = (1--9)
  take_while (fun x->x <> 0) [0;1;2;3] = []
  take_while (fun _ -> true) [] = []
  take_while (fun _ -> true) (1--10) = (1--10)
*)

(*$Q
  Q.(pair (fun1 small_int bool) (list small_int)) (fun (f,l) -> \
    let l1 = take_while f l in \
    List.for_all f l1)
*)

let rec drop_while p l = match l with
  | [] -> []
  | x :: l' -> if p x then drop_while p l' else l

(*$Q
  Q.(pair (fun1 small_int bool) (list small_int)) (fun (f,l) -> \
    take_while f l @ drop_while f l = l)
*)

let last n l =
  let len = List.length l in
  if len < n then l else drop (len-n) l

let rec find_pred p l = match l with
  | [] -> None
  | x :: _ when p x -> Some x
  | _ :: tl -> find_pred p tl

let find_pred_exn p l = match find_pred p l with
  | None -> raise Not_found
  | Some x -> x

(*$T
  find_pred ((=) 4) [1;2;5;4;3;0] = Some 4
  find_pred (fun _ -> true) [] = None
  find_pred (fun _ -> false) (1 -- 10) = None
  find_pred (fun x -> x < 10) (1 -- 9) = Some 1
*)

let find_mapi f l =
  let rec aux f i = function
    | [] -> None
    | x::l' ->
        match f i x with
          | Some _ as res -> res
          | None -> aux f (i+1) l'
  in aux f 0 l

let find_map f l = find_mapi (fun _ -> f) l

let find = find_map
let findi = find_mapi

let find_idx p l = find_mapi (fun i x -> if p x then Some (i, x) else None) l

(*$T
  find (fun x -> if x=3 then Some "a" else None) [1;2;3;4] = Some "a"
  find (fun x -> if x=3 then Some "a" else None) [1;2;4;5] = None
*)

let remove ?(eq=(=)) ~x l =
  let rec remove' eq x acc l = match l with
    | [] -> List.rev acc
    | y :: tail when eq x y -> remove' eq x acc tail
    | y :: tail -> remove' eq x (y::acc) tail
  in
  remove' eq x [] l

(*$T
  remove ~x:1 [2;1;3;3;2;1] = [2;3;3;2]
  remove ~x:10 [1;2;3] = [1;2;3]
*)

let filter_map f l =
  let rec recurse acc l = match l with
  | [] -> List.rev acc
  | x::l' ->
    let acc' = match f x with | None -> acc | Some y -> y::acc in
    recurse acc' l'
  in recurse [] l

(*$=
  ["2"; "4"] \
    (filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
      [1;2;3;4;5])
  [ "2"; "4"; "6" ] \
    (filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
      [ 1; 2; 3; 4; 5; 6 ])
*)

module Set = struct
  let mem ?(eq=(=)) x l =
    let rec search eq x l = match l with
      | [] -> false
      | y::l' -> eq x y || search eq x l'
    in search eq x l

  let add ?(eq=(=)) x l =
    if mem ~eq x l then l else x::l

  let remove ?(eq=(=)) x l =
    let rec remove_one ~eq x acc l = match l with
      | [] -> assert false
      | y :: tl when eq x y -> List.rev_append acc tl
      | y :: tl -> remove_one ~eq x (y::acc) tl
    in
    if mem ~eq x l then remove_one ~eq x [] l else l

  (*$Q
    Q.(pair int (list int)) (fun (x,l) -> \
      Set.remove x (Set.add x l) = l)
    Q.(pair int (list int)) (fun (x,l) -> \
      Set.mem x l || List.length (Set.add x l) = List.length l + 1)
    Q.(pair int (list int)) (fun (x,l) -> \
      not (Set.mem x l) || List.length (Set.remove x l) = List.length l - 1)
  *)

  let subset ?(eq=(=)) l1 l2 =
    List.for_all
      (fun t -> mem ~eq t l2)
      l1

  let uniq ?(eq=(=)) l =
    let rec uniq eq acc l = match l with
      | [] -> List.rev acc
      | x::xs when List.exists (eq x) xs -> uniq eq acc xs
      | x::xs -> uniq eq (x::acc) xs
    in uniq eq [] l

  (*$T
    Set.uniq [1;1;2;2;3;4;4;2;4;1;5] |> List.sort Pervasives.compare = [1;2;3;4;5]
  *)

  let union ?(eq=(=)) l1 l2 =
    let rec union eq acc l1 l2 = match l1 with
      | [] -> List.rev_append acc l2
      | x::xs when mem ~eq x l2 -> union eq acc xs l2
      | x::xs -> union eq (x::acc) xs l2
    in union eq [] l1 l2

  (*$T
    Set.union [1;2;4] [2;3;4;5] = [1;2;3;4;5]
  *)

  let inter ?(eq=(=)) l1 l2 =
    let rec inter eq acc l1 l2 = match l1 with
      | [] -> List.rev acc
      | x::xs when mem ~eq x l2 -> inter eq (x::acc) xs l2
      | _::xs -> inter eq acc xs l2
    in inter eq [] l1 l2

  (*$T
    Set.inter [1;2;4] [2;3;4;5] = [2;4]
  *)
end

module Idx = struct
  let mapi f l =
    let r = ref 0 in
    map
      (fun x ->
        let y = f !r x in
        incr r; y
      ) l

  (*$T
    Idx.mapi (fun i x -> i*x) [10;10;10] = [0;10;20]
  *)

  let iteri f l =
    let rec aux f i l = match l with
      | [] -> ()
      | x::l' -> f i x; aux f (i+1) l'
    in aux f 0 l

  let foldi f acc l =
    let rec foldi f acc i l = match l with
    | [] -> acc
    | x::l' ->
      let acc = f acc i x in
      foldi f acc (i+1) l'
    in
    foldi f acc 0 l

  let rec get_exn l i = match l with
    | [] -> raise Not_found
    | x::_ when i=0 -> x
    | _::l' -> get_exn l' (i-1)

  let get l i =
    try Some (get_exn l i)
    with Not_found -> None

  (*$T
    Idx.get (range 0 10) 0 = Some 0
    Idx.get (range 0 10) 5 = Some 5
    Idx.get (range 0 10) 11 = None
    Idx.get [] 0 = None
  *)

  let set l0 i x =
    let rec aux l acc i = match l with
      | [] -> l0
      | _::l' when i=0 -> List.rev_append acc (x::l')
      | y::l' ->
          aux l' (y::acc) (i-1)
    in
    aux l0 [] i

  (*$T
    Idx.set [1;2;3] 0 10 = [10;2;3]
    Idx.set [1;2;3] 4 10 = [1;2;3]
    Idx.set [1;2;3] 1 10 = [1;10;3]
   *)

  let insert l i x =
    let rec aux l acc i x = match l with
      | [] -> List.rev_append acc [x]
      | y::l' when i=0 -> List.rev_append acc (x::y::l')
      | y::l' ->
          aux l' (y::acc) (i-1) x
    in
    aux l [] i x

  (*$T
    Idx.insert [1;2;3] 0 10 = [10;1;2;3]
    Idx.insert [1;2;3] 4 10 = [1;2;3;10]
    Idx.insert [1;2;3] 1 10 = [1;10;2;3]
   *)

  let remove l0 i =
    let rec aux l acc i = match l with
      | [] -> l0
      | _::l' when i=0 -> List.rev_append acc l'
      | y::l' ->
          aux l' (y::acc) (i-1)
    in
    aux l0 [] i

  (*$T
    Idx.remove [1;2;3;4] 0 = [2;3;4]
    Idx.remove [1;2;3;4] 3 = [1;2;3]
    Idx.remove [1;2;3;4] 5 = [1;2;3;4]
  *)
end

let range_by ~step i j =
  let rec range i j acc =
    if i=j then i::acc else range i (j-step) (j::acc)
  in
  if step = 0 then
    raise (Invalid_argument "CCList.range_by")
  else if (if step > 0 then i>j else i<j) then
    []
  else
    range i ((j-i)/step*step + i)  []

(* note: the last test checks that no error occurs due to overflows. *)
(*$T
  range_by ~step:1   0 0 = [0]
  range_by ~step:1   5 0 = []
  range_by ~step:2   1 0 = []
  range_by ~step:2   0 4 = [0;2;4]
  range_by ~step:2   0 5 = [0;2;4]
  range_by ~step:~-1 0 0 = [0]
  range_by ~step:~-1 0 5 = []
  range_by ~step:~-2 0 1 = []
  range_by ~step:~-2 5 1 = [5;3;1]
  range_by ~step:~-2 5 0 = [5;3;1]
  range_by ~step:max_int 0 2 = [0]
*)

(*$Q
  Q.(pair small_int small_int) (fun (i,j) -> \
    let i = min i j and j = max i j in \
    range_by ~step:1 i j = range i j)
*)

let range i j =
  let rec up i j acc =
    if i=j then i::acc else up i (j-1) (j::acc)
  and down i j acc =
    if i=j then i::acc else down i (j+1) (j::acc)
  in
  if i<=j then up i j [] else down i j []

(*$T
  range 0 5 = [0;1;2;3;4;5]
  range 0 0 = [0]
  range 5 2 = [5;4;3;2]
*)

let range' i j =
  if i<j then range i (j-1)
  else if i=j then []
  else range i (j+1)

(*$T
  range' 0 0 = []
  range' 0 5 = [0;1;2;3;4]
  range' 5 2 = [5;4;3]
*)

let (--) = range

let (--^) = range'

(*$T
  append (range 0 100) (range 101 1000) = range 0 1000
  append (range 1000 501) (range 500 0) = range 1000 0
*)

(*$Q
  Q.(pair small_int small_int) (fun (a,b) -> \
    let l = (a--^b) in not (List.mem b l))
*)

let replicate i x =
  let rec aux acc i =
    if i = 0 then acc
    else aux (x::acc) (i-1)
  in aux [] i

let repeat i l =
  let l' = List.rev l in
  let rec aux acc i =
    if i = 0 then List.rev acc
    else aux (List.rev_append l' acc) (i-1)
  in aux [] i

module Assoc = struct
  type ('a, 'b) t = ('a*'b) list

  let rec search_exn eq l x = match l with
    | [] -> raise Not_found
    | (y,z)::l' ->
        if eq x y then z else search_exn eq l' x

  let get_exn ?(eq=(=)) l x = search_exn eq l x

  let get ?(eq=(=)) l x =
    try Some (search_exn eq l x)
    with Not_found -> None

  (*$T
    Assoc.get [1, "1"; 2, "2"] 1 = Some "1"
    Assoc.get [1, "1"; 2, "2"] 2 = Some "2"
    Assoc.get [1, "1"; 2, "2"] 3 = None
    Assoc.get [] 42 = None
  *)

  (* search for a binding for [x] in [l], and calls [f x (Some v) rest]
     or [f x None rest] depending on whether it finds the binding.
     [rest] is the list of the other bindings *)
  let rec search_set eq acc l x ~f = match l with
    | [] -> f x None acc
    | (x',y')::l' ->
        if eq x x'
          then f x (Some y') (List.rev_append acc l')
          else search_set eq ((x',y')::acc) l' x ~f

  let set ?(eq=(=)) l x y =
    search_set eq [] l x
      ~f:(fun x _ l -> (x,y)::l)

  (*$T
    Assoc.set [1,"1"; 2, "2"] 2 "two" |> List.sort Pervasives.compare \
      = [1, "1"; 2, "two"]
    Assoc.set [1,"1"; 2, "2"] 3 "3" |> List.sort Pervasives.compare \
      = [1, "1"; 2, "2"; 3, "3"]
  *)

  let mem ?(eq=(=)) l x =
    try ignore (search_exn eq l x); true
    with Not_found -> false

  (*$T
    Assoc.mem [1,"1"; 2,"2"; 3, "3"] 1
    not (Assoc.mem [1,"1"; 2,"2"; 3, "3"] 4)
  *)

  let update ?(eq=(=)) l x ~f =
    search_set eq [] l x
      ~f:(fun x opt_y rest ->
        match f opt_y with
        | None -> rest (* drop *)
        | Some y' -> (x,y') :: rest)
  (*$=
    [1,"1"; 2,"22"] \
      (Assoc.update [1,"1"; 2,"2"] 2 \
        ~f:(function Some "2" -> Some "22" | _ -> assert false) |> lsort)
    [1,"1"; 3,"3"] \
      (Assoc.update [1,"1"; 2,"2"; 3,"3"] 2 \
        ~f:(function Some "2" -> None | _ -> assert false) |> lsort)
    [1,"1"; 2,"2"; 3,"3"] \
      (Assoc.update [1,"1"; 2,"2"] 3 \
        ~f:(function None -> Some "3" | _ -> assert false) |> lsort)
  *)

  let remove ?(eq=(=)) l x =
    search_set eq [] l x
      ~f:(fun _ opt_y rest -> match opt_y with
          | None -> l  (* keep as is *)
          | Some _ -> rest)

  (*$=
    [1,"1"] \
      (Assoc.remove [1,"1"; 2,"2"] 2 |> lsort)
    [1,"1"; 3,"3"] \
      (Assoc.remove [1,"1"; 2,"2"; 3,"3"] 2  |> lsort)
    [1,"1"; 2,"2"] \
      (Assoc.remove [1,"1"; 2,"2"] 3 |> lsort)
  *)
end

(** {2 Zipper} *)

module Zipper = struct
  type 'a t = 'a list * 'a list

  let empty = [], []

  let is_empty = function
    | [], [] -> true
    | _ -> false

  let to_list (l,r) = List.rev_append l r

  let to_rev_list (l,r) = List.rev_append r l

  (*$Q
    Q.(pair (list small_int)(list small_int)) (fun z -> \
      Zipper.to_list z = List.rev (Zipper.to_rev_list z))
  *)

  let make l = [], l

  let left = function
    | x::l, r -> l, x::r
    | [], r -> [], r

  let left_exn = function
    | x::l, r -> l, x::r
    | [], _ -> invalid_arg "zipper.left_exn"

  let right = function
    | l, x::r -> x::l, r
    | l, [] -> l, []

  let right_exn = function
    | l, x::r -> x::l, r
    | _, [] -> invalid_arg "zipper.right_exn"

  let modify f z = match z with
    | l, [] ->
        begin match f None with
        | None -> z
        | Some x -> l, [x]
        end
    | l, x::r ->
        begin match f (Some x) with
        | None -> l,r
        | Some _ -> l, x::r
        end

  let is_focused = function
    | _, [] -> true
    | _ -> false

  let focused = function
    | _, x::_ -> Some x
    | _, [] -> None

  let focused_exn = function
    | _, x::_ -> x
    | _, [] -> raise Not_found

  let insert x (l,r) = l, x::r

  let remove (l,r) = match r with
    | [] -> l, []
    | _ :: r' -> l, r'

  (*$Q
    Q.(triple int (list small_int)(list small_int)) (fun (x,l,r) -> \
      Zipper.insert x (l,r) |> Zipper.remove = (l,r))
  *)

  let drop_before (_, r) = [], r

  let drop_after (l, r) = match r with
    | [] -> l, []
    | x :: _ -> l, [x]

  let drop_after_and_focused (l, _) = l, []
end

(** {2 References on Lists} *)

module Ref = struct
  type 'a t = 'a list ref

  let push l x = l := x :: !l

  let pop l = match !l with
    | [] -> None
    | x::tail ->
        l := tail;
        Some x

  let pop_exn l = match !l with
    | [] -> failwith "CCList.Ref.pop_exn"
    | x::tail ->
        l := tail;
        x

  let create() = ref []

  let clear l = l := []

  let lift f l = f !l

  let push_list r l =
    r := List.rev_append l !r

  (*$T
    let l = Ref.create() in Ref.push l 1; Ref.push_list l [2;3]; !l = [3;2;1]
  *)
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) = struct
  open M

  let map_m f l =
    let rec aux f acc l = match l with
      | [] -> return (List.rev acc)
      | x::tail ->
          f x >>= fun x' ->
          aux f (x' :: acc) tail
    in aux f [] l

  let rec map_m_par f l = match l with
    | [] -> M.return []
    | x::tl ->
        let x' = f x in
        let tl' = map_m_par f tl in
        x' >>= fun x' ->
        tl' >>= fun tl' ->
        M.return (x'::tl')

  let sequence_m l = map_m (fun x->x) l

  let rec fold_m f acc l = match l with
    | [] -> return acc
    | x :: l' ->
        f acc x
        >>= fun acc' ->
        fold_m f acc' l'
end

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let random_len len g st =
  init len (fun _ -> g st)

(*$T
  random_len 10 CCInt.random_small (Random.State.make [||]) |> List.length = 10
*)

let random g st =
  let len = Random.State.int st 1_000 in
  random_len len g st

let random_non_empty g st =
  let len = 1 + Random.State.int st 1_000 in
  random_len len g st

let random_choose l = match l with
  | [] -> raise Not_found
  | _::_ ->
      let len = List.length l in
      fun st ->
        let i = Random.State.int st len in
        List.nth l i

let random_sequence l st = map (fun g -> g st) l

let to_seq l k = List.iter k l
let of_seq seq =
  let l = ref [] in
  seq (fun x -> l := x :: !l);
  List.rev !l

let to_gen l =
  let l = ref l in
  fun () ->
    match !l with
    | [] -> None
    | x::l' ->
        l := l'; Some x

let of_gen g =
  let rec direct i g =
    if i = 0 then safe [] g
    else match g () with
      | None -> []
      | Some x -> x :: direct (i-1) g
  and safe acc g = match g () with
    | None -> List.rev acc
    | Some x -> safe (x::acc) g
  in
  direct direct_depth_default_ g

let to_klist l =
  let rec make l () = match l with
    | [] -> `Nil
    | x::l' -> `Cons (x, make l')
  in make l

let of_klist l =
  let rec direct i g =
    if i = 0 then safe [] g
    else match l () with
      | `Nil -> []
      | `Cons (x,l') -> x :: direct (i-1) l'
  and safe acc l = match l () with
    | `Nil -> List.rev acc
    | `Cons (x,l') -> safe (x::acc) l'
  in
  direct direct_depth_default_ l

module Infix = struct
  let (>|=) = (>|=)
  let (@) = (@)
  let (<*>) = (<*>)
  let (<$>) = (<$>)
  let (>>=) = (>>=)
  let (--) = (--)
  let (--^) = (--^)
end

(** {2 IO} *)

let pp ?(start="[") ?(stop="]") ?(sep=", ") pp_item buf l =
  let rec print l = match l with
    | x::((_::_) as l) ->
      pp_item buf x;
      Buffer.add_string buf sep;
      print l
    | x::[] -> pp_item buf x
    | [] -> ()
  in Buffer.add_string buf start; print l; Buffer.add_string buf stop

(*$T
  CCPrint.to_string (pp CCPrint.int) [1;2;3] = "[1, 2, 3]"
  *)

let print ?(start="[") ?(stop="]") ?(sep=", ") pp_item fmt l =
  let rec print fmt l = match l with
    | x::((_::_) as l) ->
      pp_item fmt x;
      Format.pp_print_string fmt sep;
      Format.pp_print_cut fmt ();
      print fmt l
    | x::[] -> pp_item fmt x
    | [] -> ()
  in
  Format.pp_print_string fmt start;
  print fmt l;
  Format.pp_print_string fmt stop
