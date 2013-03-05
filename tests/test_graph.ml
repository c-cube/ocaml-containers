
(** Tests on graphs *)

open OUnit
open Helpers

(* build a graph from a list of pairs of ints *)
let mk_graph l =
  let g = Graph.empty 5 in
  Graph.add_seq g
    (Sequence.map (fun (x,y) -> x,1,y)
      (Sequence.of_list l));
  g

let test_copy () =
  let g = mk_graph [0,1; 1,2; 2,3; 3,0] in
  let g' = Graph.copy g in
  Graph.add g 1 1 3;
  Graph.add g 1 2 3;
  OUnit.assert_equal ~printer:print_int_list
    [1;2] (List.sort compare (Sequence.to_list (Graph.between g 1 3)));
  OUnit.assert_bool "copy" (Sequence.is_empty (Graph.between g' 1 3));
  ()

let test_roots () =
  let g = mk_graph [0,1; 1,2; 2,3; 4,1; 5,1; 6,5; 3,5] in
  let roots = Sequence.to_list (Graph.roots g) in
  OUnit.assert_equal (List.sort compare roots) [0;4;6]

let test_leaves () =
  let g = mk_graph [0,1; 1,2; 2,3; 4,1; 6,5; 3,5; 3,7] in
  let leaves = Sequence.to_list (Graph.leaves g) in
  OUnit.assert_equal (List.sort compare leaves) [5;7]

let test_dfs () =
  let g = mk_graph [0,1; 1,2; 2,3; 3,0; 1,4; 1,5; 5,6; 4,6; 6,0] in
  let l = ref [] in
  Graph.dfs g 0 (fun (v,i) -> l := (v,i) :: !l);
  (* get index of vertex [v] in DFS traversal *)
  let get_idx v = List.assoc v !l in
  OUnit.assert_bool "order" (get_idx 0 < get_idx 1);
  OUnit.assert_bool "order" (get_idx 1 < get_idx 2);
  OUnit.assert_bool "order" (get_idx 2 < get_idx 3);
  OUnit.assert_bool "order" (get_idx 1 < get_idx 4);
  OUnit.assert_bool "order" (get_idx 1 < get_idx 5);
  OUnit.assert_bool "order" (get_idx 4 < get_idx 6 || get_idx 5 < get_idx 6);
  ()

let test_bfs () =
  let g = mk_graph [0,1; 1,2; 2,3; 2,4; 3,0; 1,4; 1,5; 5,6; 4,6; 6,0] in
  let l = Sequence.to_list
    (Sequence.mapi (fun i v -> (v,i)) (Graph.bfs_seq g 0)) in
  (* get index of vertex [v] in DFS traversal *)
  let get_idx v = List.assoc v l in
  OUnit.assert_bool "order" (get_idx 0 < get_idx 1);
  OUnit.assert_bool "order" (get_idx 0 < get_idx 2);
  OUnit.assert_bool "order" (get_idx 0 < get_idx 4);
  OUnit.assert_bool "order" (get_idx 1 < get_idx 3);
  OUnit.assert_bool "order" (get_idx 2 < get_idx 3);
  OUnit.assert_bool "order" (get_idx 4 < get_idx 6);
  OUnit.assert_bool "order" (get_idx 5 < get_idx 6);
  ()

let rec pp_path p =
  let buf = Buffer.create 10 in
  Format.bprintf buf "%a" (Sequence.pp_seq ~sep:"; " pp_edge)
    (Sequence.of_list p);
  Buffer.contents buf
and pp_edge formatter (v1,e,v2) =
  Format.fprintf formatter "%d -> %d" v1 v2

let test_dijkstra () =
  let g = mk_graph [0,1; 1,2; 2,3; 3,4; 3,0; 4,5; 1,5; 5,6; 4,6; 6,0] in
  let path = Graph.min_path g ~cost:(fun x -> x) 0 6 in
  let path = Graph.rev_path path in
  OUnit.assert_equal ~printer:pp_path [0,1,1; 1,1,5; 5,1,6] path;
  ()

let suite =
  "test_graph" >:::
    [ "test_copy" >:: test_copy;
      "test_leaves" >:: test_leaves;
      "test_roots" >:: test_roots;
      "test_dfs" >:: test_dfs;
      "test_bfs" >:: test_bfs;
      "test_dijkstra" >:: test_dijkstra;
    ]
