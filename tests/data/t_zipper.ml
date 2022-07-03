module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCZipper;;

t @@ fun () -> is_empty empty;;
t @@ fun () -> not ([ 42 ] |> make |> right |> is_empty)

let zip_gen = Q.(pair (small_list int) (small_list int));;

q zip_gen (fun z -> to_list z = List.rev (to_rev_list z));;
q zip_gen (fun g -> is_focused g = (focused g |> CCOption.is_some));;

q
  Q.(triple int (list small_int) (list small_int))
  (fun (x, l, r) -> insert x (l, r) |> remove = (l, r))
;;

eq ([ 1 ], [ 2 ]) (drop_after ([ 1 ], [ 2; 3 ]));;
eq ([ 1 ], []) (drop_after ([ 1 ], []));;
eq ([ 1 ], []) (drop_after_and_focused ([ 1 ], [ 2; 3 ]))
