

module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCPersistentHashtbl;;

module H = Make(CCInt)

let my_list =
  [ 1, "a";
    2, "b";
    3, "c";
    4, "d";
  ]

let my_iter = Iter.of_list my_list

let _list_uniq = CCList.sort_uniq
  ~cmp:(fun a b -> Stdlib.compare (fst a) (fst b))

let _list_int_int = Q.(
  map_same_type _list_uniq
    (list_of_size Gen.(0 -- 40) (pair small_int small_int))
);;

t @@ fun () ->
  let h = H.of_iter my_iter in
  assert_equal "a" (H.find h 1);
  assert_raises ((=)Not_found) (fun () -> H.find h 5);
  let h' = H.replace h 5 "e" in
  assert_equal "a" (H.find h' 1);
  assert_equal "e" (H.find h' 5);
  assert_equal "a" (H.find h 1);
  assert_raises ((=)Not_found) (fun () -> H.find h 5);
  true;;

t @@ fun () ->
  let n = 10000 in
  let seq = Iter.map (fun i -> i, string_of_int i) Iter.(0--n) in
  let h = H.of_iter seq in
  Iter.iter
    (fun (k,v) ->
      assert_equal ~printer:(fun x -> x) v (H.find h k))
    seq;
  assert_raises ((=)Not_found) (fun () -> H.find h (n+1));
  true;;

q _list_int_int
    (fun l ->
      let h = H.of_list l in
      List.for_all
        (fun (k,v) ->
          try
            H.find h k = v
          with Not_found -> false)
        l
    );;

t @@ fun () ->
  let h = H.of_iter
    Iter.(map (fun i -> i, string_of_int i)
      (0 -- 200)) in
  assert_equal 201 (H.length h);
  true;;

q _list_int_int (fun l ->
    let h = H.of_list l in
    H.length h = List.length l
);;

t @@ fun () ->
  let h = H.of_iter my_iter in
  assert_equal "a" (H.find h 1);
  assert_raises ((=)Not_found) (fun () -> H.find h 5);
  let h1 = H.add h 5 "e" in
  assert_equal "a" (H.find h1 1);
  assert_equal "e" (H.find h1 5);
  assert_equal "a" (H.find h 1);
  let h2 = H.add h1 5 "ee" in
  assert_equal "ee" (H.find h2 5);
  assert_raises ((=)Not_found) (fun () -> H.find h 5);
  let h3 = H.remove h2 1 in
  assert_equal "ee" (H.find h3 5);
  assert_raises ((=)Not_found) (fun () -> H.find h3 1);
  let h4 = H.remove h3 5 in
  assert_equal "e" (H.find h4 5);
  assert_equal "ee" (H.find h3 5);
  true;;

t @@ fun () ->
  let h = H.of_iter my_iter in
  assert_equal (H.find h 2) "b";
  assert_equal (H.find h 3) "c";
  assert_equal (H.find h 4) "d";
  assert_equal (H.length h) 4;
  let h = H.remove h 2 in
  assert_equal (H.find h 3) "c";
  assert_equal (H.length h) 3;
  assert_raises ((=)Not_found) (fun () -> H.find h 2);
  true;;

t @@ fun () ->
  let open Iter.Infix in
  let n = 10000 in
  let seq = Iter.map (fun i -> i, string_of_int i) (0 -- n) in
  let h = H.of_iter seq in
  assert_equal (n+1) (H.length h);
  let h = Iter.fold (fun h i -> H.remove h i) h (0 -- 500) in
  assert_equal (n-500) (H.length h);
  assert_bool "is_empty" (H.is_empty (H.create 16));
  true;;

q _list_int_int (fun l ->
    let h = H.of_list l in
    let h = List.fold_left (fun h (k,_) -> H.remove h k) h l in
    H.is_empty h);;

t @@ fun () ->
  let t1 = H.of_list [1, "a"; 2, "b1"] in
  let t2 = H.of_list [2, "b2"; 3, "c"] in
  let t = H.merge
    ~f:(fun _ -> function
      | `Right v2 -> Some v2
      | `Left v1 -> Some v1
      | `Both (s1,s2) -> if s1 < s2 then Some s1 else Some s2)
    t1 t2
  in
  assert_equal ~printer:string_of_int 3 (H.length t);
  assert_equal "a" (H.find t 1);
  assert_equal "b1" (H.find t 2);
  assert_equal "c" (H.find t 3);
  true;;

q _list_int_int (fun l ->
    let l1, l2 = List.partition (fun (x,_) -> x mod 2 = 0) l in
    let h1 = H.of_list l1 in
    let h2 = H.add_list h1 l2 in
    List.for_all
      (fun (k,v) -> H.find h2 k = v)
      l
    &&
    List.for_all
      (fun (k,v) -> H.find h1 k = v)
      l1
    &&
    List.length l1 = H.length h1
    &&
    List.length l = H.length h2
);;

t @@ fun () ->
  let h = H.of_iter my_iter in
  let l = Iter.to_list (H.to_iter h) in
  assert_equal my_list (List.sort compare l);
  true;;

t @@ fun () ->
  let h = H.of_iter my_iter in
  assert_equal "b" (H.find h 2);
  assert_equal "a" (H.find h 1);
  assert_raises ((=)Not_found) (fun () -> H.find h 42);
  true;;
