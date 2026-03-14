include (val Containers_testlib.make ~__FILE__ ())
module H = Containers_xxhash

(* Gold tests: hash_string with XXH64 *)
;;

t @@ fun () ->
assert_equal ~printer:Int64.to_string (-1205034819632174695L)
  (H.hash_string ~seed:0L "");
assert_equal ~printer:Int64.to_string (-3049013831022690741L)
  (H.hash_string ~seed:1L "");
assert_equal ~printer:Int64.to_string (-7444071767201028348L)
  (H.hash_string ~seed:42L "");
assert_equal ~printer:Int64.to_string 2994696410035606400L
  (H.hash_string ~seed:(-1L) "");
assert_equal ~printer:Int64.to_string (-3292477735350538661L)
  (H.hash_string ~seed:0L "a");
assert_equal ~printer:Int64.to_string (-2395144786285869370L)
  (H.hash_string ~seed:1L "a");
assert_equal ~printer:Int64.to_string (-8582455328737087284L)
  (H.hash_string ~seed:42L "a");
assert_equal ~printer:Int64.to_string 6972758980737027682L
  (H.hash_string ~seed:(-1L) "a");
assert_equal ~printer:Int64.to_string 2794345569481354659L
  (H.hash_string ~seed:0L "hello");
assert_equal ~printer:Int64.to_string 2584346877953614258L
  (H.hash_string ~seed:1L "hello");
assert_equal ~printer:Int64.to_string (-4367754540140381902L)
  (H.hash_string ~seed:42L "hello");
assert_equal ~printer:Int64.to_string 125878816811915416L
  (H.hash_string ~seed:(-1L) "hello");
assert_equal ~printer:Int64.to_string (-4510281645523327586L)
  (H.hash_string ~seed:0L "hello, world!");
assert_equal ~printer:Int64.to_string (-504920995464555508L)
  (H.hash_string ~seed:1L "hello, world!");
assert_equal ~printer:Int64.to_string 6256815367265528243L
  (H.hash_string ~seed:42L "hello, world!");
assert_equal ~printer:Int64.to_string 3946915247760971950L
  (H.hash_string ~seed:(-1L) "hello, world!");
assert_equal ~printer:Int64.to_string 1513236774081638803L
  (H.hash_string ~seed:0L "the quick brown fox");
assert_equal ~printer:Int64.to_string 4806561598883688116L
  (H.hash_string ~seed:1L "the quick brown fox");
assert_equal ~printer:Int64.to_string 6882318601984224800L
  (H.hash_string ~seed:42L "the quick brown fox");
assert_equal ~printer:Int64.to_string 3091774062649670660L
  (H.hash_string ~seed:(-1L) "the quick brown fox");
true
;;

(* Gold tests: hash_string default seed=0 *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string (-1205034819632174695L) (H.hash_string "");
assert_equal ~printer:Int64.to_string (-3292477735350538661L)
  (H.hash_string "a");
assert_equal ~printer:Int64.to_string 2794345569481354659L
  (H.hash_string "hello");
true
;;

(* Gold tests: hash_int64 *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string 3803688792395291579L (H.hash_int64 0L 0L);
assert_equal ~printer:Int64.to_string (-6977822845260490347L)
  (H.hash_int64 1L 0L);
assert_equal ~printer:Int64.to_string (-5379971487550586029L)
  (H.hash_int64 42L 0L);
assert_equal ~printer:Int64.to_string (-8804195676797548855L)
  (H.hash_int64 (-1L) 0L);
assert_equal ~printer:Int64.to_string (-7296932117151183542L)
  (H.hash_int64 1234567890123456789L 0L);
true
;;

(* Gold tests: mix64 *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string 2506089352882295055L (H.mix64 0L 1L);
assert_equal ~printer:Int64.to_string (-7001672635703045582L) (H.mix64 1L 42L);
assert_equal ~printer:Int64.to_string (-6036538516425062073L)
  (H.mix64 42L (-1L));
assert_equal ~printer:Int64.to_string 3785813419010030675L
  (H.mix64 (-1L) 1234567890123456789L);
assert_equal ~printer:Int64.to_string (-7296932117151183542L)
  (H.mix64 1234567890123456789L 0L);
true
;;

(* Gold tests: finalize64 *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string 3803688792395291579L (H.finalize64 0L);
assert_equal ~printer:Int64.to_string (-6977822845260490347L) (H.finalize64 1L);
assert_equal ~printer:Int64.to_string (-5379971487550586029L) (H.finalize64 42L);
assert_equal ~printer:Int64.to_string (-8804195676797548855L)
  (H.finalize64 (-1L));
assert_equal ~printer:Int64.to_string (-7296932117151183542L)
  (H.finalize64 1234567890123456789L);
true
;;

(* Property tests: determinism *)
q ~count:10_000 Q.string @@ fun s ->
Int64.equal (H.hash_string s) (H.hash_string s)
;;

q ~count:10_000 Q.int64 @@ fun v ->
Int64.equal (H.hash_int64 v 0L) (H.hash_int64 v 0L)
;;

q ~count:10_000 Q.int @@ fun v -> Int.equal (H.hash_int v 0) (H.hash_int v 0);;

q ~count:10_000 Q.int64 @@ fun h ->
Int64.equal (H.finalize64 h) (H.finalize64 h)
;;

(* mix64 is not commutative for most pairs *)
q ~count:10_000 (Q.pair Q.int64 Q.int64) @@ fun (a, b) ->
Q.assume (not (Int64.equal a b));
let ab = H.mix64 a b in
let ba = H.mix64 b a in
not (Int64.equal ab ba)
;;

(* Stress test: hash many strings, non-empty => non-zero *)
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
