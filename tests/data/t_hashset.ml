module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCHashSet;;

t @@ fun () ->
let module IS = Make (CCInt) in
IS.cardinal (IS.create 10) = 0
;;

t @@ fun () ->
let module IS = Make (CCInt) in
IS.find (IS.of_list [ 1; 2; 3 ]) 3 = Some 3
;;

t @@ fun () ->
let module IS = Make (CCInt) in
IS.find (IS.of_list [ 1; 2; 3 ]) 5 = None
;;

t @@ fun () ->
let module IS = Make (CCInt) in
IS.(equal (inter (of_list [ 1; 2; 3 ]) (of_list [ 2; 5; 4 ])) (of_list [ 2 ]))
;;

t @@ fun () ->
let module IS = Make (CCInt) in
IS.(
  equal
    (union (of_list [ 1; 2; 3 ]) (of_list [ 2; 5; 4 ]))
    (of_list [ 1; 2; 3; 4; 5 ]))
;;

t @@ fun () ->
let module IS = Make (CCInt) in
IS.(equal (diff (of_list [ 1; 2; 3 ]) (of_list [ 2; 4; 5 ])) (of_list [ 1; 3 ]))
