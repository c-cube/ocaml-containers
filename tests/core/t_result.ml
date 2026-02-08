open CCResult
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> Error "ohno 42" = fail_printf "ohno %d" 42;;
t @@ fun () -> Error "ohno 42" = fail_fprintf "ohno %d" 42;;

eq (Error "error\ncontext:message(number 42, foo: true)")
  (add_ctxf "message(number %d, foo: %B)" 42 true (Error "error"))
;;

t @@ fun () ->
let called_with = ref None in
let f e = called_with := Some e in
iter_err f (Ok 1);
assert (!called_with = None);
iter_err f (Error 1);
assert (!called_with = Some 1);
true
;;

t @@ fun () -> get_or_failwith (Ok 1) = 1;;

t @@ fun () ->
try
  ignore @@ get_or_failwith (Error "e");
  false
with Failure msg -> msg = "e"
;;

eq (get_lazy (fun _ -> 2) (Ok 1)) 1;;
eq (get_lazy (fun _ -> 2) (Error "error")) 2;;
eq 42 (fold_ok ( + ) 2 (Ok 40));;
eq 40 (fold_ok ( + ) 40 (Error "foo"));;
eq (Ok []) (flatten_l []);;
eq (Ok [ 1; 2; 3 ]) (flatten_l [ Ok 1; Ok 2; Ok 3 ]);;
eq (Error "ohno") (flatten_l [ Ok 1; Error "ohno"; Ok 2; Ok 3; Error "wut" ])

(* Additional comprehensive tests for CCResult *)

(* Test return and fail *)
eq (Ok 42) (return 42);;
eq (Error "failed") (fail "failed");;

(* Test of_exn and of_exn_trace *)
t @@ fun () ->
  match of_exn (Failure "test") with
  | Error msg -> String.length msg > 0
  | Ok _ -> false
;;

t @@ fun () ->
  match of_exn_trace (Failure "test") with
  | Error msg -> String.length msg > 0
  | Ok _ -> false
;;

(* Test opt_map *)
eq (Ok (Some 6)) (opt_map (fun x -> Ok (x * 2)) (Some 3));;
eq (Ok None) (opt_map (fun x -> Ok (x * 2)) None);;
eq (Error "err") (opt_map (fun _ -> Error "err") (Some 3));;

(* Test map *)
eq (Ok 3) (map (( + ) 1) (Ok 2));;
eq (Error "e") (map (( + ) 1) (Error "e"));;
t @@ fun () -> map String.uppercase_ascii (Ok "hello") = Ok "HELLO";;

(* Test map_err *)
eq (Ok 5) (map_err String.uppercase_ascii (Ok 5));;
eq (Error "ERROR") (map_err String.uppercase_ascii (Error "error"));;

(* Test map2 *)
eq (Ok "HELLO") (map2 String.uppercase_ascii String.uppercase_ascii (Ok "hello"));;
eq (Error "ERROR") (map2 String.uppercase_ascii String.uppercase_ascii (Error "error"));;

(* Test iter *)
t @@ fun () ->
  let r = ref 0 in
  iter (fun x -> r := x) (Ok 42);
  !r = 42
;;

t @@ fun () ->
  let r = ref 0 in
  iter (fun x -> r := x) (Error "e");
  !r = 0
;;

(* Test get_exn *)
eq 42 (get_exn (Ok 42));;

t @@ fun () ->
  try
    ignore (get_exn (Error "error"));
    false
  with Invalid_argument _ -> true
;;

(* Test get_or *)
eq 5 (get_or (Ok 5) ~default:0);;
eq 0 (get_or (Error "e") ~default:0);;

(* Test apply_or *)
eq 10 (apply_or (fun x -> Ok (x * 2)) 5);;
t @@ fun () -> apply_or (fun x -> if x > 0 then Ok (x * 2) else Error "neg") 5 = 10;;
t @@ fun () -> apply_or (fun x -> if x > 0 then Ok (x * 2) else Error "neg") (-5) = -5;;

(* Test map_or *)
eq 10 (map_or (fun x -> x * 2) (Ok 5) ~default:0);;
eq 0 (map_or (fun x -> x * 2) (Error "e") ~default:0);;

(* Test catch *)
eq 5 (catch (Ok 5) ~ok:CCFun.id ~err:(fun _ -> 0));;
eq 0 (catch (Error "e") ~ok:CCFun.id ~err:(fun _ -> 0));;
eq "ERROR: e" (catch (Error "e") ~ok:Int.to_string ~err:(fun e -> "ERROR: " ^ e));;

(* Test flat_map *)
eq (Ok 3) (flat_map (fun x -> Ok (x + 1)) (Ok 2));;
eq (Error "e") (flat_map (fun x -> Ok (x + 1)) (Error "e"));;
eq (Error "e2") (flat_map (fun _ -> Error "e2") (Ok 2));;

(* Test fold *)
eq 10 (fold ~ok:CCFun.id ~error:(fun _ -> 0) (Ok 10));;
eq 0 (fold ~ok:CCFun.id ~error:(fun _ -> 0) (Error "e"));;

(* Test is_ok and is_error *)
t @@ fun () -> is_ok (Ok 1);;
t @@ fun () -> not (is_ok (Error "e"));;
t @@ fun () -> is_error (Error "e");;
t @@ fun () -> not (is_error (Ok 1));;

(* Test guard and guard_str *)
t @@ fun () ->
  match guard (fun () -> 42) with
  | Ok 42 -> true
  | _ -> false
;;

t @@ fun () ->
  match guard (fun () -> failwith "error") with
  | Error _ -> true
  | _ -> false
;;

t @@ fun () ->
  match guard_str (fun () -> 42) with
  | Ok 42 -> true
  | _ -> false
;;

t @@ fun () ->
  match guard_str (fun () -> failwith "test error") with
  | Error msg -> String.length msg > 0
  | _ -> false
;;

(* Test guard_str_trace *)
t @@ fun () ->
  match guard_str_trace (fun () -> 42) with
  | Ok 42 -> true
  | _ -> false
;;

t @@ fun () ->
  match guard_str_trace (fun () -> failwith "test error") with
  | Error msg -> String.length msg > 0
  | _ -> false
;;

(* Test wrap functions *)
eq (Ok 6) (wrap1 (( + ) 1) 5);;

t @@ fun () ->
  match wrap1 (fun _ -> failwith "error") () with
  | Error _ -> true
  | _ -> false
;;

eq (Ok 7) (wrap2 ( + ) 3 4);;

t @@ fun () ->
  match wrap2 (fun _ _ -> failwith "error") 1 2 with
  | Error _ -> true
  | _ -> false
;;

eq (Ok 10) (wrap3 (fun a b c -> a + b + c) 2 3 5);;

t @@ fun () ->
  match wrap3 (fun _ _ _ -> failwith "error") 1 2 3 with
  | Error _ -> true
  | _ -> false
;;

(* Test pure *)
eq (Ok 42) (pure 42);;

(* Test join *)
eq (Ok 5) (join (Ok (Ok 5)));;
eq (Error "e") (join (Ok (Error "e")));;
eq (Error "e") (join (Error "e"));;

(* Test both *)
eq (Ok (3, 5)) (both (Ok 3) (Ok 5));;
eq (Error "e1") (both (Error "e1") (Ok 5));;
eq (Error "e2") (both (Ok 3) (Error "e2"));;
eq (Error "e1") (both (Error "e1") (Error "e2"));;

(* Test map_l *)
eq (Ok [2; 3; 4]) (map_l (fun x -> Ok (x + 1)) [1; 2; 3]);;
eq (Error "e") (map_l (fun x -> if x > 0 then Ok x else Error "e") [1; -1; 2]);;
eq (Ok []) (map_l (fun x -> Ok x) []);;

(* Test fold_l *)
eq (Ok 6) (fold_l (fun acc x -> Ok (acc + x)) 0 [1; 2; 3]);;
eq (Error "e") (fold_l (fun _ x -> if x > 0 then Ok x else Error "e") 0 [1; -1; 2]);;

(* Test choose *)
eq (Ok 1) (choose [Ok 1; Ok 2; Ok 3]);;
eq (Ok 2) (choose [Error "e1"; Ok 2; Ok 3]);;
eq (Ok 3) (choose [Error "e1"; Error "e2"; Ok 3]);;
eq (Error ["e1"; "e2"; "e3"]) (choose [Error "e1"; Error "e2"; Error "e3"]);;
eq (Error []) (choose []);;

(* Test retry *)
t @@ fun () ->
  let attempts = ref 0 in
  let f () =
    incr attempts;
    if !attempts < 3 then Error "fail" else Ok "success"
  in
  match retry 5 f with
  | Ok "success" -> !attempts = 3
  | _ -> false
;;

t @@ fun () ->
  let attempts = ref 0 in
  let f () =
    incr attempts;
    Error "always fails"
  in
  match retry 3 f with
  | Error errs -> !attempts = 3 && List.length errs = 3
  | _ -> false
;;

(* Test to_opt *)
eq (Some 5) (to_opt (Ok 5));;
eq None (to_opt (Error "e"));;

(* Test of_opt *)
eq (Ok 5) (of_opt (Some 5));;
eq (Error "option is None") (of_opt None);;

(* Test equal *)
t @@ fun () -> equal ~err:String.equal Int.equal (Ok 5) (Ok 5);;
t @@ fun () -> not (equal ~err:String.equal Int.equal (Ok 5) (Ok 6));;
t @@ fun () -> equal ~err:String.equal Int.equal (Error "e") (Error "e");;
t @@ fun () -> not (equal ~err:String.equal Int.equal (Error "e1") (Error "e2"));;
t @@ fun () -> not (equal ~err:String.equal Int.equal (Ok 5) (Error "e"));;

(* Test compare *)
t @@ fun () -> compare ~err:String.compare Int.compare (Ok 5) (Ok 5) = 0;;
t @@ fun () -> compare ~err:String.compare Int.compare (Ok 5) (Ok 6) < 0;;
t @@ fun () -> compare ~err:String.compare Int.compare (Ok 6) (Ok 5) > 0;;
t @@ fun () -> compare ~err:String.compare Int.compare (Error "a") (Error "a") = 0;;
t @@ fun () -> compare ~err:String.compare Int.compare (Error "a") (Error "b") < 0;;
t @@ fun () -> compare ~err:String.compare Int.compare (Error "a") (Ok 5) < 0;;
t @@ fun () -> compare ~err:String.compare Int.compare (Ok 5) (Error "a") > 0;;

(* Property-based tests *)
q Q.int (fun x ->
  return x = Ok x
);;

q Q.(result int string) (fun r ->
  is_ok r = not (is_error r)
);;

q Q.(result int string) (fun r ->
  map CCFun.id r = r
);;

q Q.(result int string) (fun r ->
  map_err CCFun.id r = r
);;

q Q.(result int string) (fun r ->
  flat_map return r = r
);;

q Q.(result int string) (fun r ->
  equal ~err:String.equal Int.equal r r
);;

q Q.(result int string) (fun r ->
  compare ~err:String.compare Int.compare r r = 0
);;

q Q.(result int string) (fun r ->
  of_opt (to_opt r) = (match r with Ok x -> Ok x | Error _ -> Error "option is None")
);;

q Q.int (fun x ->
  to_opt (Ok x) = Some x
);;

q Q.string (fun e ->
  to_opt (Error e) = None
);;

q Q.(pair (result int string) int) (fun (r, default) ->
  let v = get_or r ~default in
  match r with
  | Ok x -> v = x
  | Error _ -> v = default
);;

q Q.(list (result int string)) (fun l ->
  match flatten_l l with
  | Ok values -> List.for_all (function Ok _ -> true | Error _ -> false) l && List.length values <= List.length l
  | Error _ -> List.exists (function Error _ -> true | Ok _ -> false) l
);;

(* Additional focused tests for high-value functions *)
t @@ fun () -> map (( + ) 1) (Ok 2) = Ok 3;;
t @@ fun () -> is_ok (Ok 1) && not (is_ok (Error "e"));;
t @@ fun () -> to_opt (Ok 5) = Some 5 && to_opt (Error "e") = None;;
t @@ fun () -> both (Ok 3) (Ok 5) = Ok (3, 5);;
q Q.int (fun x -> return x = Ok x);;
q Q.int (fun x -> to_opt (Ok x) = Some x);;
