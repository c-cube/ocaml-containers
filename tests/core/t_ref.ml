open CCRef
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () ->
  let r = create 5 in
  !r = 5
;;

t @@ fun () ->
  let r = create "hello" in
  !r = "hello"
;;

(* Test map *)
t @@ fun () ->
  let r = ref 5 in
  let r2 = map (( + ) 1) r in
  !r2 = 6 && !r = 5
;;

t @@ fun () ->
  let r = ref "hello" in
  let r2 = map String.uppercase_ascii r in
  !r2 = "HELLO" && !r = "hello"
;;

(* Test iter *)
t @@ fun () ->
  let r = ref 5 in
  let acc = ref 0 in
  iter (fun x -> acc := !acc + x) r;
  !acc = 5
;;

(* Test update *)
t @@ fun () ->
  let r = ref 5 in
  update (( + ) 3) r;
  !r = 8
;;

t @@ fun () ->
  let r = ref "hello" in
  update String.uppercase_ascii r;
  !r = "HELLO"
;;

t @@ fun () ->
  let r = ref 10 in
  update (fun x -> x * 2) r;
  update (fun x -> x - 1) r;
  !r = 19
;;

(* Test incr_then_get *)
t @@ fun () ->
  let r = ref 5 in
  let v = incr_then_get r in
  v = 6 && !r = 6
;;

t @@ fun () ->
  let r = ref 0 in
  let v1 = incr_then_get r in
  let v2 = incr_then_get r in
  v1 = 1 && v2 = 2 && !r = 2
;;

(* Test get_then_incr *)
t @@ fun () ->
  let r = ref 5 in
  let v = get_then_incr r in
  v = 5 && !r = 6
;;

t @@ fun () ->
  let r = ref 0 in
  let v1 = get_then_incr r in
  let v2 = get_then_incr r in
  v1 = 0 && v2 = 1 && !r = 2
;;

(* Test difference between incr_then_get and get_then_incr *)
t @@ fun () ->
  let r1 = ref 5 in
  let r2 = ref 5 in
  let v1 = incr_then_get r1 in
  let v2 = get_then_incr r2 in
  v1 = 6 && v2 = 5 && !r1 = !r2
;;

(* Test swap *)
t @@ fun () ->
  let r1 = ref 5 in
  let r2 = ref 10 in
  swap r1 r2;
  !r1 = 10 && !r2 = 5
;;

t @@ fun () ->
  let r1 = ref "hello" in
  let r2 = ref "world" in
  swap r1 r2;
  !r1 = "world" && !r2 = "hello"
;;

t @@ fun () ->
  let r1 = ref 1 in
  let r2 = ref 2 in
  swap r1 r2;
  swap r1 r2;
  !r1 = 1 && !r2 = 2
;;

(* Test protect *)
t @@ fun () ->
  let r = ref 5 in
  let result = protect r 10 (fun () -> !r) in
  result = 10 && !r = 5
;;

t @@ fun () ->
  let r = ref "original" in
  let result = protect r "temp" (fun () ->
    assert (!r = "temp");
    "result"
  ) in
  result = "result" && !r = "original"
;;

t @@ fun () ->
  let r = ref 0 in
  try
    ignore (protect r 5 (fun () ->
      assert (!r = 5);
      failwith "error"
    ));
    false
  with Failure _ ->
    !r = 0
;;

t @@ fun () ->
  let r1 = ref 1 in
  let r2 = ref 2 in
  let result = protect r1 10 (fun () ->
    protect r2 20 (fun () ->
      !r1 + !r2
    )
  ) in
  result = 30 && !r1 = 1 && !r2 = 2
;;

(* Test compare *)
t @@ fun () ->
  let r1 = ref 5 in
  let r2 = ref 5 in
  compare Int.compare r1 r2 = 0
;;

t @@ fun () ->
  let r1 = ref 3 in
  let r2 = ref 5 in
  compare Int.compare r1 r2 < 0
;;

t @@ fun () ->
  let r1 = ref 7 in
  let r2 = ref 5 in
  compare Int.compare r1 r2 > 0
;;

(* Test equal *)
t @@ fun () ->
  let r1 = ref 5 in
  let r2 = ref 5 in
  equal Int.equal r1 r2
;;

t @@ fun () ->
  let r1 = ref 5 in
  let r2 = ref 6 in
  not (equal Int.equal r1 r2)
;;

t @@ fun () ->
  let r1 = ref "hello" in
  let r2 = ref "hello" in
  equal String.equal r1 r2
;;

(* Test to_list *)
eq [5] (to_list (ref 5));;
eq ["hello"] (to_list (ref "hello"));;

t @@ fun () ->
  let r = ref 42 in
  let l = to_list r in
  List.length l = 1 && List.hd l = 42
;;

(* Test to_iter *)
t @@ fun () ->
  let r = ref 5 in
  let acc = ref 0 in
  to_iter r (fun x -> acc := !acc + x);
  !acc = 5
;;

t @@ fun () ->
  let r = ref 10 in
  let count = ref 0 in
  to_iter r (fun _ -> incr count);
  !count = 1
;;

(* Property-based tests *)
q Q.int (fun x ->
  let r = create x in
  !r = x
);;

q Q.int (fun x ->
  let r = ref x in
  let r2 = map CCFun.id r in
  !r2 = !r
);;

q Q.int (fun x ->
  let r = ref x in
  update CCFun.id r;
  !r = x
);;

q Q.int (fun x ->
  let r = ref x in
  incr_then_get r = x + 1 && !r = x + 1
);;

q Q.int (fun x ->
  let r = ref x in
  get_then_incr r = x && !r = x + 1
);;

q Q.(pair int int) (fun (x, y) ->
  let r1 = ref x in
  let r2 = ref y in
  swap r1 r2;
  !r1 = y && !r2 = x
);;

q Q.int (fun x ->
  let r = ref 0 in
  let result = protect r x (fun () -> !r) in
  result = x && !r = 0
);;

q Q.int (fun x ->
  let r = ref x in
  equal Int.equal r r
);;

q Q.int (fun x ->
  let r = ref x in
  compare Int.compare r r = 0
);;
