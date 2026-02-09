module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCWBTree
module M = Make (CCInt)

type op =
  | Add of int * int
  | Remove of int
  | Remove_min

let gen_op =
  CCRandom.(
    choose_exn
      [
        return Remove_min;
        map (fun x -> Remove x) small_int;
        pure (fun x y -> Add (x, y)) <*> small_int <*> small_int;
      ])

and pp_op =
  let open Printf in
  function
  | Add (x, y) -> sprintf "Add %d %d" x y
  | Remove x -> sprintf "Remove %d" x
  | Remove_min -> "Remove_min"

let apply_ops l m =
  List.fold_left
    (fun m -> function
      | Add (i, b) -> M.add i b m
      | Remove i -> M.remove i m
      | Remove_min ->
        (try
           let _, _, m' = M.extract_min m in
           m'
         with Not_found -> m))
    m l

let op = Q.make ~print:pp_op gen_op
let _list_uniq = CCList.sort_uniq ~cmp:(CCFun.compose_binop fst Stdlib.compare)
;;

q ~count:200
  Q.(list op)
  (fun l ->
    let m = apply_ops l M.empty in
    M.balanced m)
;;

q
  Q.(list (pair nat_small bool))
  (fun l ->
    let m = M.of_list l in
    M.balanced m)
;;

q
  Q.(list (pair nat_small nat_small))
  (fun l ->
    let l = _list_uniq l in
    let m = M.of_list l in
    List.for_all (fun (k, v) -> M.get_exn k m = v) l)
;;

q
  Q.(list (pair nat_small nat_small))
  (fun l ->
    let l = _list_uniq l in
    let m = M.of_list l in
    M.cardinal m = List.length l)
;;

q
  Q.(list_size Gen.(0 -- 30) (pair nat_small nat_small))
  (fun l ->
    let m = M.of_list l in
    List.for_all
      (fun (k, _) ->
        M.mem k m
        &&
        let m' = M.remove k m in
        not (M.mem k m'))
      l)
;;

q
  Q.(list_size Gen.(0 -- 30) (pair nat_small nat_small))
  (fun l ->
    let m = M.of_list l in
    List.for_all
      (fun (k, _) ->
        let m' = M.remove k m in
        M.balanced m')
      l)
;;

t @@ fun () ->
let m = CCList.(0 -- 1000 |> map (fun i -> i, i) |> M.of_list) in
List.for_all (fun i -> M.nth_exn i m = (i, i)) CCList.(0 -- 1000)
;;

q ~count:1_000
  Q.(list_size Gen.(0 -- 30) (pair nat_small nat_small))
  (fun l ->
    let l = CCList.sort_uniq ~cmp:(CCFun.compose_binop fst compare) l in
    let m = M.of_list l in
    List.for_all
      (fun (k, v) ->
        match M.get_rank k m with
        | `First | `After _ -> true
        | `At n -> (k, v) = M.nth_exn n m)
      l)
;;

q ~count:20
  Q.(list_size Gen.(1 -- 100) (pair nat_small nat_small))
  (fun lst ->
    let lst = _list_uniq lst in
    let m = M.of_list lst in
    List.for_all
      (fun (k, v) ->
        let l, v', r = M.split k m in
        v' = Some v
        && M.to_iter l |> Iter.for_all (fun (k', _) -> k' < k)
        && M.to_iter r |> Iter.for_all (fun (k', _) -> k' > k)
        && M.balanced m
        && M.cardinal l + M.cardinal r + 1 = List.length lst)
      lst)
;;

t @@ fun () ->
let m1 = M.of_list [ 1, 1; 2, 2; 4, 4 ] in
let m2 = M.of_list [ 1, 1; 3, 3; 4, 4; 7, 7 ] in
let m = M.merge ~f:(fun _ -> CCOption.map2 ( + )) m1 m2 in
assert_bool "balanced" (M.balanced m);
assert_equal
  ~cmp:(CCList.equal (CCPair.equal CCInt.equal CCInt.equal))
  ~printer:CCFormat.(to_string (list (pair int int)))
  [ 1, 2; 4, 8 ]
  (M.to_list m |> List.sort Stdlib.compare);
true
;;

q
  Q.(
    let p = list (pair nat_small nat_small) in
    pair p p)
  (fun (l1, l2) ->
    let l1 = _list_uniq l1 and l2 = _list_uniq l2 in
    let m1 = M.of_list l1 and m2 = M.of_list l2 in
    let m =
      M.merge
        ~f:(fun _ v1 v2 ->
          match v1 with
          | None -> v2
          | Some _ as r -> r)
        m1 m2
    in
    List.for_all (fun (k, v) -> M.get_exn k m = v) l1
    && List.for_all (fun (k, v) -> M.mem k m1 || M.get_exn k m = v) l2)
