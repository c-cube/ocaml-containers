include (val Containers_testlib.make ~__FILE__ ())
module H = Containers_xxhash

(* Gold tests: hash_string *)
;;

t @@ fun () ->
assert_equal ~printer:Int64.to_string (-1205034819632174695L) (H.hash_string "");
assert_equal ~printer:Int64.to_string (-7444071767201028348L)
  (H.hash_string_seed "" 42L);
assert_equal ~printer:Int64.to_string 2794345569481354659L
  (H.hash_string "hello");
assert_equal ~printer:Int64.to_string (-4367754540140381902L)
  (H.hash_string_seed "hello" 42L);
assert_equal ~printer:Int64.to_string 1513236774081638803L
  (H.hash_string "the quick brown fox");
assert_equal ~printer:Int64.to_string 6882318601984224800L
  (H.hash_string_seed "the quick brown fox" 42L);
true
;;

(* Gold tests: hash_int64 *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string 3803688792395291579L (H.hash_int64 0L);
assert_equal ~printer:Int64.to_string (-6977822845260490347L) (H.hash_int64 1L);
assert_equal ~printer:Int64.to_string (-8804195676797548855L)
  (H.hash_int64 (-1L));
assert_equal ~printer:Int64.to_string (-7296932117151183542L)
  (H.hash_int64 1234567890123456789L);
true
;;

(* Gold tests: hash_int32 *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string 4246796580750024372L (H.hash_int32 0l);
assert_equal ~printer:Int64.to_string (-851299076295404719L) (H.hash_int32 1l);
assert_equal ~printer:Int64.to_string 9185342943168159635L (H.hash_int32 (-1l));
assert_equal ~printer:Int64.to_string (-2929917330072466447L) (H.hash_int32 42l);
true
;;

(* Gold tests: hash_int *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string 3803688792395291579L (H.hash_int 0);
assert_equal ~printer:Int64.to_string (-6977822845260490347L) (H.hash_int 1);
assert_equal ~printer:Int64.to_string (-8804195676797548855L) (H.hash_int (-1));
assert_equal ~printer:Int64.to_string (-5379971487550586029L) (H.hash_int 42);
true
;;

(* Gold tests: hash_bool *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string 3803688792395291579L (H.hash_bool false);
assert_equal ~printer:Int64.to_string (-6977822845260490347L) (H.hash_bool true);
true
;;

(* Gold tests: hash_char *)
t @@ fun () ->
(* 'a' = 97, '0' = 48 *)
assert_equal ~printer:Int64.to_string (H.hash_int 97) (H.hash_char 'a');
assert_equal ~printer:Int64.to_string (H.hash_int 48) (H.hash_char '0');
true
;;

(* Property tests: determinism *)
q ~count:10_000 Q.string @@ fun s ->
Int64.equal (H.hash_string s) (H.hash_string s)
;;

q ~count:10_000 Q.int64 @@ fun v ->
Int64.equal (H.hash_int64 v) (H.hash_int64 v)
;;

q ~count:10_000 Q.int @@ fun v -> Int64.equal (H.hash_int v) (H.hash_int v);;
q ~count:10_000 Q.bool @@ fun b -> Int64.equal (H.hash_bool b) (H.hash_bool b);;
q ~count:10_000 Q.char @@ fun c -> Int64.equal (H.hash_char c) (H.hash_char c);;

(* Different seeds give different results for the same input *)
q ~count:10_000 (Q.pair Q.string Q.int64) @@ fun (s, seed) ->
Q.assume (not (Int64.equal seed 0L));
not (Int64.equal (H.hash_string s) (H.hash_string_seed s seed))
;;

q ~count:10_000 (Q.pair Q.int64 Q.int64) @@ fun (v, seed) ->
Q.assume (not (Int64.equal seed 0L));
not (Int64.equal (H.hash_int64 v) (H.hash_int64_seed v seed))
;;

q ~count:10_000 (Q.pair Q.int Q.int64) @@ fun (v, seed) ->
Q.assume (not (Int64.equal seed 0L));
not (Int64.equal (H.hash_int v) (H.hash_int_seed v seed))
;;

(* Different inputs give different results for the same seed *)
q ~count:10_000 (Q.pair Q.string Q.string) @@ fun (s1, s2) ->
Q.assume (not (String.equal s1 s2));
not (Int64.equal (H.hash_string s1) (H.hash_string s2))
;;

q ~count:10_000 (Q.pair Q.int64 Q.int64) @@ fun (a, b) ->
Q.assume (not (Int64.equal a b));
not (Int64.equal (H.hash_int64 a) (H.hash_int64 b))
;;

q ~count:10_000 (Q.pair Q.int Q.int) @@ fun (a, b) ->
Q.assume (a <> b);
not (Int64.equal (H.hash_int a) (H.hash_int b))
;;

(* Stress test: hash 100k strings of varying lengths, non-empty => non-zero *)
t @@ fun () ->
for len = 0 to 99 do
  for _ = 1 to 1000 do
    let s = String.make len 'x' in
    let h = H.hash_string s in
    if len > 0 then
      if Int64.equal h 0L then
        failwith
          (Printf.sprintf "unexpected zero hash for string of len %d" len)
  done
done;
true

let () = Containers_testlib.run_all ~descr:"test xxhash" [ get () ]
