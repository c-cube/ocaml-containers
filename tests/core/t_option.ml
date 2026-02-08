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
t @@ fun () -> return_if true 1 = Some 1;;

(* Additional comprehensive tests for CCOption *)

(* Test map *)
eq (Some 2) (map (( + ) 1) (Some 1));;
eq None (map (( + ) 1) None);;
t @@ fun () -> map (fun x -> x * 2) (Some 5) = Some 10;;

(* Test map_or *)
eq 10 (map_or ~default:0 (fun x -> x * 2) (Some 5));;
eq 0 (map_or ~default:0 (fun x -> x * 2) None);;
t @@ fun () -> map_or ~default:"empty" String.uppercase_ascii (Some "hello") = "HELLO";;
t @@ fun () -> map_or ~default:"empty" String.uppercase_ascii None = "empty";;

(* Test map_lazy *)
t @@ fun () ->
  let called = ref false in
  let result = map_lazy (fun () -> called := true; 0) (fun x -> x * 2) (Some 5) in
  result = 10 && not !called
;;

t @@ fun () ->
  let called = ref false in
  let result = map_lazy (fun () -> called := true; 0) (fun x -> x * 2) None in
  result = 0 && !called
;;

(* Test is_some and is_none *)
t @@ fun () -> is_some (Some 1);;
t @@ fun () -> not (is_some None);;
t @@ fun () -> is_none None;;
t @@ fun () -> not (is_none (Some 1));;

(* Test compare *)
t @@ fun () -> compare Int.compare (Some 1) (Some 1) = 0;;
t @@ fun () -> compare Int.compare (Some 1) (Some 2) < 0;;
t @@ fun () -> compare Int.compare (Some 2) (Some 1) > 0;;
t @@ fun () -> compare Int.compare None None = 0;;
t @@ fun () -> compare Int.compare None (Some 1) < 0;;
t @@ fun () -> compare Int.compare (Some 1) None > 0;;

(* Test equal *)
t @@ fun () -> equal Int.equal (Some 1) (Some 1);;
t @@ fun () -> not (equal Int.equal (Some 1) (Some 2));;
t @@ fun () -> equal Int.equal None None;;
t @@ fun () -> not (equal Int.equal None (Some 1));;
t @@ fun () -> not (equal Int.equal (Some 1) None);;

(* Test return and some *)
eq (Some 42) (return 42);;
eq (Some "hello") (some "hello");;
t @@ fun () -> return 5 = Some 5;;

(* Test none *)
t @@ fun () -> (none : int option) = None;;

(* Test flat_map / bind *)
eq (Some 2) (flat_map (fun x -> Some (x + 1)) (Some 1));;
eq None (flat_map (fun x -> Some (x + 1)) None);;
eq None (flat_map (fun _ -> None) (Some 1));;

eq (Some 2) (bind (Some 1) (fun x -> Some (x + 1)));;
eq None (bind None (fun x -> Some (x + 1)));;

(* Test flat_map_l *)
eq [1; 2; 3] (flat_map_l (fun x -> [x; x+1; x+2]) (Some 1));;
eq [] (flat_map_l (fun x -> [x; x+1]) None);;

(* Test map2 *)
eq (Some 5) (map2 ( + ) (Some 2) (Some 3));;
eq None (map2 ( + ) None (Some 3));;
eq None (map2 ( + ) (Some 2) None);;
eq None (map2 ( + ) None None);;

(* Test iter *)
t @@ fun () ->
  let r = ref 0 in
  iter (fun x -> r := x) (Some 42);
  !r = 42
;;

t @@ fun () ->
  let r = ref 0 in
  iter (fun x -> r := x) None;
  !r = 0
;;

(* Test fold *)
eq 10 (fold (fun acc x -> acc + x) 5 (Some 5));;
eq 5 (fold (fun acc x -> acc + x) 5 None);;

(* Test if_ *)
eq (Some 5) (if_ (fun x -> x > 0) 5);;
eq None (if_ (fun x -> x > 0) (-5));;
eq (Some "hello") (if_ (fun s -> String.length s > 0) "hello");;
eq None (if_ (fun s -> String.length s > 0) "");;

(* Test exists *)
t @@ fun () -> exists (fun x -> x > 0) (Some 5);;
t @@ fun () -> not (exists (fun x -> x > 0) (Some (-5)));;
t @@ fun () -> not (exists (fun x -> x > 0) None);;

(* Test for_all *)
t @@ fun () -> for_all (fun x -> x > 0) (Some 5);;
t @@ fun () -> not (for_all (fun x -> x > 0) (Some (-5)));;
t @@ fun () -> for_all (fun x -> x > 0) None;;

(* Test get_or *)
eq 5 (get_or ~default:0 (Some 5));;
eq 0 (get_or ~default:0 None);;

(* Test value *)
eq 5 (value (Some 5) ~default:0);;
eq 0 (value None ~default:0);;

(* Test apply_or *)
eq 10 (apply_or (fun x -> Some (x * 2)) 5);;
t @@ fun () -> apply_or (fun x -> if x > 0 then Some (x * 2) else None) 5 = 10;;
t @@ fun () -> apply_or (fun x -> if x > 0 then Some (x * 2) else None) (-5) = -5;;

(* Test get_exn *)
eq 42 (get_exn (Some 42));;

t @@ fun () ->
  try
    ignore (get_exn None);
    false
  with Invalid_argument _ -> true
;;

(* Test get_lazy *)
eq 5 (get_lazy (fun () -> 0) (Some 5));;
eq 0 (get_lazy (fun () -> 0) None);;

t @@ fun () ->
  let called = ref false in
  let _ = get_lazy (fun () -> called := true; 0) (Some 5) in
  not !called
;;

t @@ fun () ->
  let called = ref false in
  let _ = get_lazy (fun () -> called := true; 0) None in
  !called
;;

(* Test wrap *)
t @@ fun () ->
  wrap (fun x -> x + 1) 5 = Some 6
;;

t @@ fun () ->
  wrap (fun _ -> failwith "error") () = None
;;

t @@ fun () ->
  wrap ~handler:(fun _ -> true) (fun x -> if x = 0 then failwith "div by zero" else 10 / x) 0 = None
;;

t @@ fun () ->
  wrap ~handler:(function Division_by_zero -> true | _ -> false)
    (fun x -> 10 / x) 2 = Some 5
;;

(* Test wrap2 *)
t @@ fun () ->
  wrap2 ( + ) 2 3 = Some 5
;;

t @@ fun () ->
  wrap2 (fun _ _ -> failwith "error") 1 2 = None
;;

(* Test pure *)
eq (Some 42) (pure 42);;

(* Test or_ *)
eq (Some 1) (or_ ~else_:(Some 2) (Some 1));;
eq (Some 2) (or_ ~else_:(Some 2) None);;
eq None (or_ ~else_:None None);;

(* Test or_lazy *)
t @@ fun () ->
  let called = ref false in
  let result = or_lazy ~else_:(fun () -> called := true; Some 2) (Some 1) in
  result = Some 1 && not !called
;;

t @@ fun () ->
  let called = ref false in
  let result = or_lazy ~else_:(fun () -> called := true; Some 2) None in
  result = Some 2 && !called
;;

(* Test choice *)
eq (Some 1) (choice [Some 1; Some 2; Some 3]);;
eq (Some 2) (choice [None; Some 2; Some 3]);;
eq (Some 3) (choice [None; None; Some 3]);;
eq None (choice [None; None; None]);;
eq None (choice []);;

(* Test to_list *)
eq [42] (to_list (Some 42));;
eq [] (to_list None);;

(* Test of_list *)
eq (Some 1) (of_list [1]);;
eq (Some 1) (of_list [1; 2; 3]);;
eq None (of_list []);;

(* Test to_result *)
eq (Ok 5) (to_result "error" (Some 5));;
eq (Error "error") (to_result "error" None);;

(* Test to_result_lazy *)
t @@ fun () ->
  let called = ref false in
  let result = to_result_lazy (fun () -> called := true; "error") (Some 5) in
  result = Ok 5 && not !called
;;

t @@ fun () ->
  let called = ref false in
  let result = to_result_lazy (fun () -> called := true; "error") None in
  result = Error "error" && !called
;;

(* Test of_result *)
eq (Some 5) (of_result (Ok 5));;
eq None (of_result (Error "error"));;

(* Property-based tests *)
q Q.int (fun x ->
  return x = Some x
);;

q Q.(option int) (fun o ->
  is_some o = not (is_none o)
);;

q Q.(option int) (fun o ->
  map CCFun.id o = o
);;

q Q.(option int) (fun o ->
  flat_map return o = o
);;

q Q.(option int) (fun o ->
  bind o return = o
);;

q Q.(option int) (fun o ->
  equal Int.equal o o
);;

q Q.(option int) (fun o ->
  compare Int.compare o o = 0
);;

q Q.(pair (option int) int) (fun (o, default) ->
  let v = get_or ~default o in
  match o with
  | Some x -> v = x
  | None -> v = default
);;

q Q.int (fun x ->
  to_list (Some x) = [x]
);;

q Q.(list int) (fun l ->
  match of_list l with
  | Some x -> List.mem x l
  | None -> l = []
);;

q Q.(option int) (fun o ->
  match o with
  | Some x -> of_list (to_list o) = Some x
  | None -> of_list (to_list o) = None
);;

q Q.(option int) (fun o ->
  of_result (to_result "err" o) = o
);;

q Q.int (fun x ->
  let o1 = Some x in
  let o2 = Some x in
  or_ ~else_:o2 o1 = o1
);;

q Q.int (fun x ->
  or_ ~else_:(Some x) None = Some x
);;

q Q.(pair (option int) (option int)) (fun (o1, o2) ->
  match choice [o1; o2] with
  | Some _ -> is_some o1 || is_some o2
  | None -> is_none o1 && is_none o2
);;
