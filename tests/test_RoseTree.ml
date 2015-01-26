open OUnit
open CCFun

module RoseTree = Containers_misc.RoseTree

let format_node = Format.pp_print_int

let string_of_tree tree =
  CCFormat.sprintf "%a" (RoseTree.print format_node) tree

let assert_equal_tree expected_tree_rep tree =
  let expected_tree_rep_string =
    (String.concat "\n" expected_tree_rep) ^ "\n"
  in
  let tree_as_string = string_of_tree tree in
  assert_equal ~printer:(fun x -> x) expected_tree_rep_string tree_as_string

let assert_equal_zipper expected_tree_rep zipper =
  assert_equal_tree expected_tree_rep (RoseTree.Zipper.tree zipper)

let single_node_tree = `Node (10, [])

let single_tree_strings = ["10"]

let normal_tree =
  `Node (0, [
      `Node (1, [
          `Node (10, []) ;
        ]) ;
      `Node (2, [
          `Node (20, []) ;
          `Node (21, []) ;
        ]) ;
      `Node (3, [
          `Node (30, []) ;
          `Node (31, []) ;
          `Node (32, []) ;
        ]) ;
    ])

let normal_tree_strings = [
  "0" ;
  "|- 1" ;
  "|  '- 10" ;
  "|- 2" ;
  "|  |- 20" ;
  "|  '- 21" ;
  "'- 3" ;
  "   |- 30" ;
  "   |- 31" ;
  "   '- 32" ;
]

let new_tree =
  `Node (100, [
      `Node (1000, [
          `Node (10000, []) ;
        ]) ;
      `Node (1001, [
          `Node (10010, []) ;
          `Node (10012, []) ;
        ]) ;
    ])

let new_tree_strings = [
  "100" ;
  "|- 1000" ;
  "|  '- 10000" ;
  "'- 1001" ;
  "   |- 10010" ;
  "   '- 10012" ;
]

let test_print_single_node_tree () =
  let expected = single_tree_strings in
  assert_equal_tree expected single_node_tree

let test_print_normal_tree () =
  let expected = normal_tree_strings in
  assert_equal_tree expected normal_tree

let test_fold_single_node_tree () =
  let tree_double_sum = RoseTree.fold ~f:(fun value acc -> acc + value * 2) 0 single_node_tree
  in
  assert_equal 20 tree_double_sum

let test_fold_normal_tree () =
  let tree_sum = RoseTree.fold ~f:(fun value acc -> acc + value) 0 normal_tree
  in
  assert_equal 150 tree_sum

let test_base_zipper_single_node_tree () =
  let expected = single_tree_strings in
  assert_equal_zipper expected (RoseTree.Zipper.zipper single_node_tree)

let test_base_zipper_normal_tree () =
  let expected = normal_tree_strings in
  assert_equal_zipper expected (RoseTree.Zipper.zipper normal_tree)

let test_zipper_nth_child_0 () =
  let zipper = RoseTree.Zipper.zipper normal_tree
               |> RoseTree.Zipper.nth_child 0
               |> CCOpt.get_exn
  in
  let expected = [
    "1" ;
    "'- 10" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_zipper_nth_child_1 () =
  let zipper = RoseTree.Zipper.zipper normal_tree
               |> RoseTree.Zipper.nth_child 1
               |> CCOpt.get_exn
  in
  let expected = [
    "2" ;
    "|- 20" ;
    "'- 21" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_zipper_nth_child_2 () =
  let zipper = RoseTree.Zipper.zipper normal_tree
               |> RoseTree.Zipper.nth_child 2
               |> CCOpt.get_exn
  in
  let expected = [
    "3" ;
    "|- 30" ;
    "|- 31" ;
    "'- 32" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_zipper_nth_child_does_not_exist () =
  let maybe_zipper = RoseTree.Zipper.zipper normal_tree
                     |> RoseTree.Zipper.nth_child 3
  in
  assert_equal false (CCOpt.is_some maybe_zipper)

let test_zipper_nth_child_negative_index () =
  let maybe_zipper = RoseTree.Zipper.zipper normal_tree
                     |> RoseTree.Zipper.nth_child (-2)
  in
  assert_equal false (CCOpt.is_some maybe_zipper)

let test_zipper_nth_child_plus_parent_is_noop () =
  let zipper = RoseTree.Zipper.zipper normal_tree
               |> RoseTree.Zipper.nth_child 2
               |> CCOpt.get_exn
               |> RoseTree.Zipper.parent
               |> CCOpt.get_exn
  in
  let expected = normal_tree_strings in
  assert_equal_zipper expected zipper

let test_zipper_left_sibling () =
  let zipper = RoseTree.Zipper.zipper normal_tree
               |> RoseTree.Zipper.nth_child 2
               |> CCOpt.get_exn
               |> RoseTree.Zipper.left_sibling
               |> CCOpt.get_exn
  in
  let expected = [
    "2" ;
    "|- 20" ;
    "'- 21" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_zipper_left_sibling_twice () =
  let zipper = RoseTree.Zipper.zipper normal_tree
               |> RoseTree.Zipper.nth_child 2
               |> CCOpt.get_exn
               |> RoseTree.Zipper.left_sibling
               |> CCOpt.get_exn
               |> RoseTree.Zipper.left_sibling
               |> CCOpt.get_exn
  in
  let expected = [
  "1" ;
  "'- 10" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_zipper_left_sibling_does_not_exist () =
  let maybe_zipper = RoseTree.Zipper.zipper normal_tree
                     |> RoseTree.Zipper.nth_child 2
                     |> CCOpt.get_exn
                     |> RoseTree.Zipper.left_sibling
                     |> CCOpt.get_exn
                     |> RoseTree.Zipper.left_sibling
                     |> CCOpt.get_exn
                     |> RoseTree.Zipper.left_sibling
  in
  assert_equal false (CCOpt.is_some maybe_zipper)

let test_zipper_nth_child_plus_left_sibling_plus_parent_is_noop () =
  let zipper = RoseTree.Zipper.zipper normal_tree
               |> RoseTree.Zipper.nth_child 2
               |> CCOpt.get_exn
               |> RoseTree.Zipper.left_sibling
               |> CCOpt.get_exn
               |> RoseTree.Zipper.parent
               |> CCOpt.get_exn
  in
  let expected = normal_tree_strings in
  assert_equal_zipper expected zipper

let test_zipper_right_sibling () =
  let zipper = RoseTree.Zipper.zipper normal_tree
               |> RoseTree.Zipper.nth_child 0
               |> CCOpt.get_exn
               |> RoseTree.Zipper.right_sibling
               |> CCOpt.get_exn
  in
  let expected = [
    "2" ;
    "|- 20" ;
    "'- 21" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_zipper_right_sibling_twice () =
  let zipper = RoseTree.Zipper.zipper normal_tree
               |> RoseTree.Zipper.nth_child 0
               |> CCOpt.get_exn
               |> RoseTree.Zipper.right_sibling
               |> CCOpt.get_exn
               |> RoseTree.Zipper.right_sibling
               |> CCOpt.get_exn
  in
  let expected = [
  "3" ;
  "|- 30" ;
  "|- 31" ;
  "'- 32" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_zipper_right_sibling_does_not_exist () =
  let maybe_zipper = RoseTree.Zipper.zipper normal_tree
                     |> RoseTree.Zipper.nth_child 0
                     |> CCOpt.get_exn
                     |> RoseTree.Zipper.right_sibling
                     |> CCOpt.get_exn
                     |> RoseTree.Zipper.right_sibling
                     |> CCOpt.get_exn
                     |> RoseTree.Zipper.right_sibling
  in
  assert_equal false (CCOpt.is_some maybe_zipper)

let test_zipper_nth_child_plus_right_sibling_plus_parent_is_noop () =
  let zipper = RoseTree.Zipper.zipper normal_tree
               |> RoseTree.Zipper.nth_child 0
               |> CCOpt.get_exn
               |> RoseTree.Zipper.right_sibling
               |> CCOpt.get_exn
               |> RoseTree.Zipper.parent
               |> CCOpt.get_exn
  in
  let expected = normal_tree_strings in
  assert_equal_zipper expected zipper

let test_parent () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.parent
             |> CCOpt.get_exn
  in
  let expected = [
    "1" ;
    "'- 10" ;
  ] in
  assert_equal_zipper expected zipper

let test_parent_on_root () =
  let maybe_zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.parent
  in
  assert_equal false (CCOpt.is_some maybe_zipper)

let test_root () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.root
  in
  let expected = normal_tree_strings in
  assert_equal_zipper expected zipper

let test_root_on_root () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.root
  in
  let expected = normal_tree_strings in
  assert_equal_zipper expected zipper

let test_insert_left_sibling () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.insert_left_sibling new_tree
             |> CCOpt.get_exn
             |> RoseTree.Zipper.root
  in
  let expected = [
    "0" ;
    "|- 1" ;
    "|  |- 100" ;
    "|  |  |- 1000" ;
    "|  |  |  '- 10000" ;
    "|  |  '- 1001" ;
    "|  |     |- 10010" ;
    "|  |     '- 10012" ;
    "|  '- 10" ;
    "|- 2" ;
    "|  |- 20" ;
    "|  '- 21" ;
    "'- 3" ;
    "   |- 30" ;
    "   |- 31" ;
    "   '- 32" ;
  ] in
  assert_equal_zipper expected zipper

let test_insert_left_sibling_focuses_on_new_tree () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.insert_left_sibling new_tree
             |> CCOpt.get_exn
  in
  let expected = new_tree_strings
  in
  assert_equal_zipper expected zipper

let test_insert_left_sibling_on_root () =
  let maybe_zipper = RoseTree.Zipper.zipper normal_tree
                     |> RoseTree.Zipper.insert_left_sibling new_tree
  in
  assert_equal false (CCOpt.is_some maybe_zipper)

let test_insert_right_sibling () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.insert_right_sibling new_tree
             |> CCOpt.get_exn
             |> RoseTree.Zipper.root
  in
  let expected = [
    "0" ;
    "|- 1" ;
    "|  |- 10" ;
    "|  '- 100" ;
    "|     |- 1000" ;
    "|     |  '- 10000" ;
    "|     '- 1001" ;
    "|        |- 10010" ;
    "|        '- 10012" ;
    "|- 2" ;
    "|  |- 20" ;
    "|  '- 21" ;
    "'- 3" ;
    "   |- 30" ;
    "   |- 31" ;
    "   '- 32" ;
  ] in
  assert_equal_zipper expected zipper

let test_insert_right_sibling_focuses_on_new_tree () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.insert_right_sibling new_tree
             |> CCOpt.get_exn
  in
  let expected = new_tree_strings
  in
  assert_equal_zipper expected zipper

let test_insert_right_sibling_on_root () =
  let maybe_zipper = RoseTree.Zipper.zipper normal_tree
                     |> RoseTree.Zipper.insert_right_sibling new_tree
  in
  assert_equal false (CCOpt.is_some maybe_zipper)

let test_append_child () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 2
             |> CCOpt.get_exn
             |> RoseTree.Zipper.append_child new_tree
             |> RoseTree.Zipper.root
  in
  let expected = [
    "0" ;
    "|- 1" ;
    "|  '- 10" ;
    "|- 2" ;
    "|  |- 20" ;
    "|  '- 21" ;
    "'- 3" ;
    "   |- 30" ;
    "   |- 31" ;
    "   |- 32" ;
    "   '- 100" ;
    "      |- 1000" ;
    "      |  '- 10000" ;
    "      '- 1001" ;
    "         |- 10010" ;
    "         '- 10012" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_append_child_focuses_on_new_tree () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 2
             |> CCOpt.get_exn
             |> RoseTree.Zipper.append_child new_tree
  in
  let expected = new_tree_strings
  in
  assert_equal_zipper expected zipper

let test_replace () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 1
             |> CCOpt.get_exn
             |> RoseTree.Zipper.replace new_tree
             |> RoseTree.Zipper.root
  in
  let expected = [
  "0" ;
  "|- 1" ;
  "|  '- 10" ;
  "|- 100" ;
  "|  |- 1000" ;
  "|  |  '- 10000" ;
  "|  '- 1001" ;
  "|     |- 10010" ;
  "|     '- 10012" ;
  "'- 3" ;
  "   |- 30" ;
  "   |- 31" ;
  "   '- 32" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_replace_focuses_on_new_tree () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 1
             |> CCOpt.get_exn
             |> RoseTree.Zipper.replace new_tree
  in
  let expected = new_tree_strings in
  assert_equal_zipper expected zipper

let test_replace_root () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.replace new_tree
  in
  let expected = new_tree_strings in
  assert_equal_zipper expected zipper

let test_delete () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 1
             |> CCOpt.get_exn
             |> RoseTree.Zipper.delete
             |> CCOpt.get_exn
             |> RoseTree.Zipper.root
  in
  let expected = [
  "0" ;
  "|- 1" ;
  "|  '- 10" ;
  "'- 3" ;
  "   |- 30" ;
  "   |- 31" ;
  "   '- 32" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_delete_focuses_on_leftmost_sibling_if_possible () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 1
             |> CCOpt.get_exn
             |> RoseTree.Zipper.delete
             |> CCOpt.get_exn
  in
  let expected = [
  "1" ;
  "'- 10" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_delete_focuses_on_rightmost_sibling_if_no_left_sibling () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.delete
             |> CCOpt.get_exn
  in
  let expected = [
  "2" ;
  "|- 20" ;
  "'- 21" ;
  ]
  in
  assert_equal_zipper expected zipper

let test_delete_focuses_on_parent_if_no_more_siblings () =
  let zipper = RoseTree.Zipper.zipper normal_tree
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.nth_child 0
             |> CCOpt.get_exn
             |> RoseTree.Zipper.delete
             |> CCOpt.get_exn
  in
  let expected = ["1"] in
  assert_equal_zipper expected zipper

let test_delete_root () =
  let maybe_zipper = RoseTree.Zipper.zipper normal_tree
                     |> RoseTree.Zipper.delete
  in
  assert_equal false (CCOpt.is_some maybe_zipper)

let suite =
  "test_RoseTree" >:::
  [
    "test_print_single_node_tree" >:: test_print_single_node_tree ;
    "test_print_normal_tree" >:: test_print_normal_tree ;
    "test_fold_single_node_tree" >:: test_fold_single_node_tree ;
    "test_fold_normal_tree" >:: test_fold_normal_tree ;
    "test_base_zipper_single_node_tree" >:: test_base_zipper_single_node_tree ;
    "test_base_zipper_normal_tree" >:: test_base_zipper_normal_tree ;
    "test_zipper_nth_child_0" >:: test_zipper_nth_child_0 ;
    "test_zipper_nth_child_1" >:: test_zipper_nth_child_1 ;
    "test_zipper_nth_child_2" >:: test_zipper_nth_child_2 ;
    "test_zipper_nth_child_does_not_exist" >:: test_zipper_nth_child_does_not_exist ;
    "test_zipper_nth_child_negative_index" >:: test_zipper_nth_child_negative_index ;
    "test_zipper_nth_child_plus_parent_is_noop" >:: test_zipper_nth_child_plus_parent_is_noop ;
    "test_zipper_left_sibling" >:: test_zipper_left_sibling ;
    "test_zipper_left_sibling_twice" >:: test_zipper_left_sibling_twice ;
    "test_zipper_left_sibling_does_not_exist" >:: test_zipper_left_sibling_does_not_exist ;
    "test_zipper_nth_child_plus_left_sibling_plus_parent_is_noop" >:: test_zipper_nth_child_plus_left_sibling_plus_parent_is_noop ;
    "test_zipper_right_sibling" >:: test_zipper_right_sibling ;
    "test_zipper_right_sibling_twice" >:: test_zipper_right_sibling_twice ;
    "test_zipper_right_sibling_does_not_exist" >:: test_zipper_right_sibling_does_not_exist ;
    "test_zipper_nth_child_plus_right_sibling_plus_parent_is_noop" >:: test_zipper_nth_child_plus_right_sibling_plus_parent_is_noop ;
    "test_parent" >:: test_parent ;
    "test_parent_on_root" >:: test_parent_on_root ;
    "test_root" >:: test_root ;
    "test_root_on_root" >:: test_root_on_root ;
    "test_insert_left_sibling" >:: test_insert_left_sibling ;
    "test_insert_left_sibling_focuses_on_new_tree" >:: test_insert_left_sibling_focuses_on_new_tree ;
    "test_insert_left_sibling_on_root" >:: test_insert_left_sibling_on_root ;
    "test_insert_right_sibling" >:: test_insert_right_sibling ;
    "test_insert_right_sibling_focuses_on_new_tree" >:: test_insert_right_sibling_focuses_on_new_tree ;
    "test_insert_right_sibling_on_root" >:: test_insert_right_sibling_on_root ;
    "test_append_child" >:: test_append_child ;
    "test_append_child_focuses_on_new_tree" >:: test_append_child_focuses_on_new_tree ;
    "test_replace" >:: test_replace ;
    "test_replace_focuses_on_new_tree" >:: test_replace_focuses_on_new_tree ;
    "test_replace_root" >:: test_replace_root ;
    "test_delete" >:: test_delete ;
    "test_delete_focuses_on_leftmost_sibling_if_possible" >:: test_delete_focuses_on_leftmost_sibling_if_possible ;
    "test_delete_focuses_on_rightmost_sibling_if_no_left_sibling" >:: test_delete_focuses_on_rightmost_sibling_if_no_left_sibling ;
    "test_delete_focuses_on_parent_if_no_more_siblings" >:: test_delete_focuses_on_parent_if_no_more_siblings ;
    "test_delete_root" >:: test_delete_root ;
  ]
