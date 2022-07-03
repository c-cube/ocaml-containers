open CCOption
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

eq None (filter (( = ) 0) (Some 1));;
eq (Some 0) (filter (( = ) 0) (Some 0));;
eq None (filter (fun _ -> true) None);;

eq
  (try
     get_exn_or "ohno" (None : unit option);
     false
   with Invalid_argument s -> s = "ohno")
;;

t @@ fun () -> 123 = get_exn_or "yes" (Some 123);;
t @@ fun () -> sequence_l [ None; Some 1; Some 2 ] = None;;
t @@ fun () -> sequence_l [ Some 1; Some 2; Some 3 ] = Some [ 1; 2; 3 ];;
t @@ fun () -> sequence_l [] = Some [];;
t @@ fun () -> choice_iter (Iter.of_list [ None; Some 1; Some 2 ]) = Some 1;;
t @@ fun () -> choice_iter Iter.empty = None;;
t @@ fun () -> choice_iter (Iter.repeat None |> Iter.take 100) = None;;
t @@ fun () -> choice_seq (CCSeq.of_list [ None; Some 1; Some 2 ]) = Some 1;;
t @@ fun () -> choice_seq CCSeq.empty = None;;
t @@ fun () -> choice_seq (CCSeq.repeat None |> CCSeq.take 100) = None;;
t @@ fun () -> flatten None = None;;
t @@ fun () -> flatten (Some None) = None;;
t @@ fun () -> flatten (Some (Some 1)) = Some 1;;
t @@ fun () -> return_if false 1 = None;;
t @@ fun () -> return_if true 1 = Some 1
