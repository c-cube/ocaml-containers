module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCIntMap

let highest2 x : int =
  let rec aux i =
    if i = 0 then
      i
    else if 1 = x lsr i then
      1 lsl i
    else
      aux (i - 1)
  in
  if x < 0 then
    min_int
  else
    aux (Sys.word_size - 2)
;;

q ~count:1_000 Q.int (fun x ->
    if Bit.equal_int (highest2 x) (Bit.highest x) then
      true
    else
      QCheck.Test.fail_reportf "x=%d, highest=%d, highest2=%d@." x
        (Bit.highest x :> int)
        (highest2 x))

let _list_uniq l =
  CCList.sort_uniq ~cmp:(fun a b -> Stdlib.compare (fst a) (fst b)) l
;;

q
  Q.(list_small (pair int int))
  (fun l ->
    let m = of_list l in
    is_empty m = (cardinal m = 0))
;;

q Q.int (fun i ->
    let b = Bit.highest i in
    (b :> int) land i = (b :> int)
    && (i < 0 || ((b :> int) <= i && i - (b :> int) < (b :> int))))
;;

q Q.int (fun i -> Bit.highest i = Bit.min_int = (i < 0));;
q Q.int (fun i -> (Bit.highest i :> int) < 0 = (Bit.highest i = Bit.min_int));;

q Q.int (fun i ->
    let j = (Bit.highest i :> int) in
    j land (j - 1) = 0)
;;

t @@ fun () -> (Bit.highest min_int :> int) = min_int;;
t @@ fun () -> (Bit.highest 2 :> int) = 2;;
t @@ fun () -> (Bit.highest 17 :> int) = 16;;
t @@ fun () -> (Bit.highest 300 :> int) = 256;;
q Q.(list (pair int bool)) (fun l -> check_invariants (of_list l));;

q
  Q.(list (pair int int))
  (fun l ->
    let l = _list_uniq l in
    let m = of_list l in
    List.for_all (fun (k, v) -> find k m = Some v) l)
;;

q
  Q.(list (pair int int))
  (fun l ->
    let m = of_list l in
    List.for_all (fun (k, _) -> mem k m) l)
;;

q ~count:20
  Q.(list (pair int int))
  (fun l ->
    let l = _list_uniq l in
    let m = of_list l in
    List.for_all (fun (k, v) -> find_exn k m = v) l)
;;

q ~count:20
  Q.(list (pair int int))
  (fun l ->
    let l = _list_uniq l in
    let m = of_list l in
    List.for_all (fun (k, _) -> mem k m && not (mem k (remove k m))) l)
;;

eq
  ~printer:Q.Print.(list (pair int int))
  [ 1, 1; 2, 22; 3, 3 ]
  (of_list [ 1, 1; 2, 2; 3, 3 ]
  |> update 2 (function
       | None -> assert false
       | Some _ -> Some 22)
  |> to_list |> List.sort Stdlib.compare)
;;

q
  Q.(list (pair int bool))
  (fun l ->
    let open Q in
    CCList.sort_uniq ~cmp:CCOrd.poly l
    = CCList.sort CCOrd.poly l
    ==> equal ~eq:( = ) (of_list l) (of_list (List.rev l)))
;;

(* regression for #329 *)
t @@ fun () ->
let minus m1 m2 = union (fun _key v1 v2 -> v1 - v2) m1 m2 in

let key = 0 in
let m0 = singleton key 1 in
(* a map of [key] to the value 1 *)
let m1 = minus m0 m0 in
(* a map of [key] to the value 0 *)
let m2 = minus m0 m1 in
(* a map of [key] to the value 1 *)
let observed = equal ~eq:( = ) m2 m0 in
(* [m0] and [m2] should be equal *)
assert_equal true observed;
true
;;

q
  Q.(pair (list (pair int bool)) (list (pair int bool)))
  (fun (l1, l2) ->
    check_invariants (union (fun _ _ x -> x) (of_list l1) (of_list l2)))
;;

q
  Q.(pair (list (pair int bool)) (list (pair int bool)))
  (fun (l1, l2) ->
    check_invariants (inter (fun _ _ x -> x) (of_list l1) (of_list l2)))
;;

(* associativity of union *)
q
  Q.(
    let p = list (pair int int) in
    triple p p p)
  (fun (l1, l2, l3) ->
    let m1 = of_list l1 and m2 = of_list l2 and m3 = of_list l3 in
    let f _ x y = max x y in
    equal ~eq:( = ) (union f (union f m1 m2) m3) (union f m1 (union f m2 m3)))
;;

t @@ fun () ->
assert_equal ~cmp:(equal ~eq:( = ))
  ~printer:(CCFormat.to_string (pp CCString.pp))
  (of_list [ 1, "1"; 2, "2"; 3, "3"; 4, "4" ])
  (union
     (fun _ a _ -> a)
     (of_list [ 1, "1"; 3, "3" ])
     (of_list [ 2, "2"; 4, "4" ]));
true
;;

t @@ fun () ->
assert_equal ~cmp:(equal ~eq:( = ))
  ~printer:(CCFormat.to_string (pp CCString.pp))
  (of_list [ 1, "1"; 2, "2"; 3, "3"; 4, "4" ])
  (union
     (fun _ a _ -> a)
     (of_list [ 1, "1"; 2, "2"; 3, "3" ])
     (of_list [ 2, "2"; 4, "4" ]));
true
;;

q
  Q.(list (pair int bool))
  (fun l ->
    equal ~eq:( = ) (of_list l) (union (fun _ a _ -> a) (of_list l) (of_list l)))

let union_l l1 l2 =
  let l2' = List.filter (fun (x, _) -> not @@ List.mem_assoc x l1) l2 in
  _list_uniq (l1 @ l2')

let inter_l l1 l2 =
  let l2' = List.filter (fun (x, _) -> List.mem_assoc x l1) l2 in
  _list_uniq l2'
;;

q
  Q.(pair (list_small (pair nat_small unit)) (list_small (pair nat_small unit)))
  (fun (l1, l2) ->
    union_l l1 l2
    = _list_uniq @@ to_list (union (fun _ _ _ -> ()) (of_list l1) (of_list l2)))
;;

q
  Q.(pair (list_small (pair nat_small unit)) (list_small (pair nat_small unit)))
  (fun (l1, l2) ->
    inter_l l1 l2
    = _list_uniq @@ to_list (inter (fun _ _ _ -> ()) (of_list l1) (of_list l2)))
;;

t @@ fun () ->
assert_equal ~cmp:(equal ~eq:( = ))
  ~printer:(CCFormat.to_string (pp CCString.pp))
  (singleton 2 "2")
  (inter
     (fun _ a _ -> a)
     (of_list [ 1, "1"; 2, "2"; 3, "3" ])
     (of_list [ 2, "2"; 4, "4" ]));
true
;;

q
  Q.(list (pair int bool))
  (fun l ->
    equal ~eq:( = ) (of_list l) (inter (fun _ a _ -> a) (of_list l) (of_list l)))
;;

(* associativity of inter *)
q
  Q.(
    let p = list (pair int int) in
    triple p p p)
  (fun (l1, l2, l3) ->
    let m1 = of_list l1 and m2 = of_list l2 and m3 = of_list l3 in
    let f _ x y = max x y in
    equal ~eq:( = ) (inter f (inter f m1 m2) m3) (inter f m1 (inter f m2 m3)))
;;

q
  Q.(pair (fun2 Observable.int Observable.int bool) (list_small (pair int int)))
  (fun (f, l) ->
    let (QCheck.Fun (_, f)) = f in
    _list_uniq (List.filter (fun (x, y) -> f x y) l)
    = _list_uniq @@ to_list @@ filter f @@ of_list l)
;;

q
  Q.(
    pair
      (fun2 Observable.int Observable.int @@ option bool)
      (list_small (pair int int)))
  (fun (f, l) ->
    let (QCheck.Fun (_, f)) = f in
    _list_uniq
      (CCList.filter_map
         (fun (x, y) -> CCOption.map (CCPair.make x) @@ f x y)
         l)
    = _list_uniq @@ to_list @@ filter_map f @@ of_list l)

let merge_union _x o =
  match o with
  | `Left v | `Right v | `Both (v, _) -> Some v

let merge_inter _x o =
  match o with
  | `Left _ | `Right _ -> None
  | `Both (v, _) -> Some v
;;

q
  Q.(
    let p = list_small (pair nat_small nat_small) in
    pair p p)
  (fun (l1, l2) ->
    check_invariants (merge ~f:merge_union (of_list l1) (of_list l2)))
;;

q
  Q.(
    let p = list_small (pair nat_small nat_small) in
    pair p p)
  (fun (l1, l2) ->
    check_invariants (merge ~f:merge_inter (of_list l1) (of_list l2)))
;;

q
  Q.(
    let p = list_small (pair nat_small unit) in
    pair p p)
  (fun (l1, l2) ->
    let l1 = _list_uniq l1 and l2 = _list_uniq l2 in
    equal ~eq:Stdlib.( = )
      (union (fun _ v1 _ -> v1) (of_list l1) (of_list l2))
      (merge ~f:merge_union (of_list l1) (of_list l2)))
;;

q
  Q.(
    let p = list_small (pair nat_small unit) in
    pair p p)
  (fun (l1, l2) ->
    let l1 = _list_uniq l1 and l2 = _list_uniq l2 in
    equal ~eq:Stdlib.( = )
      (inter (fun _ v1 _ -> v1) (of_list l1) (of_list l2))
      (merge ~f:merge_inter (of_list l1) (of_list l2)))
;;

q
  Q.(list (pair int int))
  (fun l ->
    let l = List.map (fun (k, v) -> abs k, v) l in
    let rec is_sorted = function
      | [] | [ _ ] -> true
      | x :: y :: tail -> x <= y && is_sorted (y :: tail)
    in
    of_list l |> to_list |> List.rev_map fst |> is_sorted)
;;

q
  Q.(list (pair int int))
  (fun l ->
    of_list l |> cardinal
    = List.length (l |> List.map fst |> CCList.sort_uniq ~cmp:CCInt.compare))
;;

q
  Q.(list (pair nat_small int))
  (fun l ->
    of_list l |> cardinal
    = List.length (l |> List.map fst |> CCList.sort_uniq ~cmp:CCInt.compare))
;;

eq ~printer:Q.Print.int 1
  (let t = of_list [ 197151390, 0; 197151390, 0 ] in
   cardinal t)
;;

t @@ fun () ->
doubleton 1 "a" 2 "b" |> to_gen |> of_gen |> to_list |> List.sort Stdlib.compare
= [ 1, "a"; 2, "b" ]
;;

q
  Q.(list (pair int bool))
  (fun l ->
    let m = of_list l in
    equal ~eq:( = ) m (m |> to_gen |> of_gen))
;;

q
  Q.(list (pair int bool))
  (fun l ->
    let m1 = of_list l and m2 = of_list (List.rev l) in
    compare ~cmp:Stdlib.compare m1 m2 = 0)
;;

q
  Q.(pair (list (pair int bool)) (list (pair int bool)))
  (fun (l1, l2) ->
    let l1 = List.map (fun (k, v) -> abs k, v) l1 in
    let l2 = List.map (fun (k, v) -> abs k, v) l2 in
    let m1 = of_list l1 and m2 = of_list l2 in
    let c = compare ~cmp:Stdlib.compare m1 m2
    and c' = compare ~cmp:Stdlib.compare m2 m1 in
    c = 0 = (c' = 0) && c < 0 = (c' > 0) && c > 0 = (c' < 0))
;;

q
  Q.(pair (list (pair int bool)) (list (pair int bool)))
  (fun (l1, l2) ->
    let l1 = List.map (fun (k, v) -> abs k, v) l1 in
    let l2 = List.map (fun (k, v) -> abs k, v) l2 in
    let m1 = of_list l1 and m2 = of_list l2 in
    compare ~cmp:Stdlib.compare m1 m2 = 0 = equal ~eq:( = ) m1 m2)
;;

q
  Q.(list (pair int bool))
  (fun l ->
    let m = of_list l in
    equal ~eq:( = ) m (m |> to_seq |> of_seq))

let test_count = 2_500

open QCheck

type instr_tree =
  | Empty
  | Singleton of int * int
  | Add of int * int * instr_tree
  | Remove of int * instr_tree
  | Union of instr_tree * instr_tree
  | Inter of instr_tree * instr_tree

let rec to_string (a : instr_tree) : string =
  let int_to_string = string_of_int in
  match a with
  | Empty -> "Empty"
  | Singleton (k, v) -> Printf.sprintf "Singleton(%d,%d)" k v
  | Add (k, v, t) -> Printf.sprintf "Add(%d,%d," k v ^ to_string t ^ ")"
  | Remove (n, t) -> "Remove (" ^ int_to_string n ^ ", " ^ to_string t ^ ")"
  | Union (t, t') -> "Union (" ^ to_string t ^ ", " ^ to_string t' ^ ")"
  | Inter (t, t') -> "Inter (" ^ to_string t ^ ", " ^ to_string t' ^ ")"

let merge_f _ x y = min x y

let rec interpret t : _ t =
  match t with
  | Empty -> empty
  | Singleton (k, v) -> singleton k v
  | Add (k, v, t) -> add k v (interpret t)
  | Remove (n, t) -> remove n (interpret t)
  | Union (t, t') ->
    let s = interpret t in
    let s' = interpret t' in
    union merge_f s s'
  | Inter (t, t') ->
    let s = interpret t in
    let s' = interpret t' in
    inter merge_f s s'

let tree_gen int_gen : instr_tree Q.Gen.t =
  let open Gen in
  sized
    (fix (fun recgen n ->
         match n with
         | 0 ->
           oneof
             [
               return Empty;
               Gen.map2 (fun i j -> Singleton (i, j)) int_gen int_gen;
             ]
         | _ ->
           oneof_weighted
             [
               1, return Empty;
               1, map2 (fun k v -> Singleton (k, v)) int_gen int_gen;
               ( 2,
                 map3
                   (fun i j t -> Add (i, j, t))
                   int_gen int_gen
                   (recgen (n - 1)) );
               2, map2 (fun i t -> Remove (i, t)) int_gen (recgen (n - 1));
               ( 2,
                 map2
                   (fun l r -> Union (l, r))
                   (recgen (n / 2))
                   (recgen (n / 2)) );
               ( 2,
                 map2
                   (fun l r -> Inter (l, r))
                   (recgen (n / 2))
                   (recgen (n / 2)) );
             ]))

let ( <+> ) = Q.Iter.( <+> )

let rec tshrink t : instr_tree Q.Iter.t =
  match t with
  | Empty -> Iter.empty
  | Singleton (k, v) ->
    Iter.return Empty
    <+> Iter.map (fun k' -> Singleton (k', v)) (Shrink.int k)
    <+> Iter.map (fun v' -> Singleton (k, v')) (Shrink.int v)
  | Add (k, v, t) ->
    Iter.of_list [ Empty; t; Singleton (k, v) ]
    <+> Iter.map (fun t' -> Add (k, v, t')) (tshrink t)
    <+> Iter.map (fun k' -> Add (k', v, t)) (Shrink.int k)
    <+> Iter.map (fun v' -> Add (k, v', t)) (Shrink.int v)
  | Remove (i, t) ->
    Iter.of_list [ Empty; t ]
    <+> Iter.map (fun t' -> Remove (i, t')) (tshrink t)
    <+> Iter.map (fun i' -> Remove (i', t)) (Shrink.int i)
  | Union (t0, t1) ->
    Iter.of_list [ Empty; t0; t1 ]
    <+> Iter.map (fun t0' -> Union (t0', t1)) (tshrink t0)
    <+> Iter.map (fun t1' -> Union (t0, t1')) (tshrink t1)
  | Inter (t0, t1) ->
    Iter.of_list [ Empty; t0; t1 ]
    <+> Iter.map (fun t0' -> Inter (t0', t1)) (tshrink t0)
    <+> Iter.map (fun t1' -> Inter (t0, t1')) (tshrink t1)

let arb_int =
  oneof_weighted [ 5, int_small; 3, int; 1, oneof_list [ min_int; max_int ] ]

let arb_tree = make ~print:to_string ~shrink:tshrink (tree_gen arb_int.gen)
let empty_m = []
let singleton_m k v = [ k, v ]
let mem_m i s = List.mem_assoc i s

let rec remove_m i s =
  match s with
  | [] -> []
  | (j, v) :: s' ->
    if i = j then
      s'
    else
      (j, v) :: remove_m i s'

let add_m k v s = List.sort Stdlib.compare ((k, v) :: remove_m k s)

let rec union_m s s' =
  match s, s' with
  | [], _ -> s'
  | _, [] -> s
  | (k1, v1) :: is, (k2, v2) :: js ->
    if k1 < k2 then
      (k1, v1) :: union_m is s'
    else if k1 > k2 then
      (k2, v2) :: union_m s js
    else
      (k1, min v1 v2) :: union_m is js

let rec inter_m s s' =
  match s with
  | [] -> []
  | (k, v) :: s ->
    if List.mem_assoc k s' then
      (k, min v (List.assoc k s')) :: inter_m s s'
    else
      inter_m s s'

let abstract s =
  List.sort Stdlib.compare (fold (fun k v acc -> (k, v) :: acc) s [])
;;

(* A bunch of agreement properties *)

eq empty_m
  (let s = empty in
   abstract s)
;;

q ~count:test_count (Q.pair arb_int arb_int) (fun (k, v) ->
    abstract (singleton k v) = singleton_m k v)
;;

q ~count:test_count
  Q.(pair arb_tree arb_int)
  (fun (t, n) ->
    let s = interpret t in
    mem n s = mem_m n (abstract s))
;;

q ~count:test_count (triple arb_tree arb_int arb_int) (fun (t, k, v) ->
    let s = interpret t in
    abstract (add k v s) = add_m k v (abstract s))
;;

q ~count:test_count (pair arb_tree arb_int) (fun (t, n) ->
    let s = interpret t in
    abstract (remove n s) = remove_m n (abstract s))
;;

q ~count:test_count (pair arb_tree arb_tree) (fun (t, t') ->
    let s = interpret t in
    let s' = interpret t' in
    abstract (union merge_f s s') = union_m (abstract s) (abstract s'))
;;

q ~count:test_count
  Q.(pair arb_tree arb_tree)
  (fun (t, t') ->
    let s = interpret t in
    let s' = interpret t' in
    abstract (inter merge_f s s') = inter_m (abstract s) (abstract s'))
