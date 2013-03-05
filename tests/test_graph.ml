
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
  let g = mk_graph [0,1; 1,2; 2,3; 3,0] in

  ()

let test_dijkstra () =
  ()

let suite =
  "test_graph" >:::
    [ "test_copy" >:: test_copy;
      "test_leaves" >:: test_leaves;
      "test_roots" >:: test_roots;
      "test_dfs" >:: test_dfs;
      "test_dijkstra" >:: test_dijkstra;
    ]
