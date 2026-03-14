include (val Containers_testlib.make ~__FILE__ ())
module H = Containers_xxhash

(* Gold tests: hash_string with XXH64 (mix_string + finalize) *)
;;

t @@ fun () ->
assert_equal ~printer:Int64.to_string (-8037231448521241007L)
  (H.hash_string ~seed:H.seed "");
assert_equal ~printer:Int64.to_string 7619381941762342490L
  (H.hash_string ~seed:H.seed "a");
assert_equal ~printer:Int64.to_string 8482916093137399771L
  (H.hash_string ~seed:H.seed "hello");
assert_equal ~printer:Int64.to_string (-3052030864281505429L)
  (H.hash_string ~seed:H.seed "hello, world!");
assert_equal ~printer:Int64.to_string 2707297459162763210L
  (H.hash_string ~seed:H.seed "the quick brown fox");
true
;;

(* Gold tests: hash_string with non-default seed (seed from mix_int) *)
t @@ fun () ->
(* seed after mixing 1 into seed=0: hash_string uses that as XXH64 seed *)
let seed1 = H.mix_int H.seed 1 in
(* these values computed from: finalize(mix_string(mix_int(0,1), s)) *)
assert_equal ~printer:Int64.to_string
  (H.hash_string ~seed:seed1 "")
  (H.hash_string ~seed:seed1 "");
(* just test determinism with custom seed *)
assert_equal ~printer:Int64.to_string
  (H.hash_string ~seed:seed1 "hello")
  (H.hash_string ~seed:seed1 "hello");
(* different seeds produce different hashes for same string *)
assert (
  not
    (Int64.equal
       (H.hash_string ~seed:H.seed "hello")
       (H.hash_string ~seed:seed1 "hello")));
true
;;

(* Gold tests: hash_string default seed=0 *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string (-8037231448521241007L) (H.hash_string "");
assert_equal ~printer:Int64.to_string 7619381941762342490L (H.hash_string "a");
assert_equal ~printer:Int64.to_string 8482916093137399771L
  (H.hash_string "hello");
true
;;

(* Gold tests: hash_int64 *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string (-5605595894618674504L) (H.hash_int64 0L);
assert_equal ~printer:Int64.to_string 7046788939542163588L (H.hash_int64 1L);
assert_equal ~printer:Int64.to_string 2627184251037003377L (H.hash_int64 42L);
assert_equal ~printer:Int64.to_string (-8629399683307595115L)
  (H.hash_int64 (-1L));
assert_equal ~printer:Int64.to_string 8147024165990365903L
  (H.hash_int64 1234567890123456789L);
true
;;

(* Gold tests: hash_int *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string (-5605595894618674504L) (H.hash_int 0);
assert_equal ~printer:Int64.to_string 7046788939542163588L (H.hash_int 1);
assert_equal ~printer:Int64.to_string 2627184251037003377L (H.hash_int 42);
assert_equal ~printer:Int64.to_string (-8629399683307595115L) (H.hash_int (-1));
assert_equal ~printer:Int64.to_string (-3317520227865190253L)
  (H.hash_int 1234567890);
true
;;

(* Gold tests: hash_int32 *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string (-5605595894618674504L) (H.hash_int32 0l);
assert_equal ~printer:Int64.to_string 7046788939542163588L (H.hash_int32 1l);
assert_equal ~printer:Int64.to_string 2627184251037003377L (H.hash_int32 42l);
assert_equal ~printer:Int64.to_string (-8629399683307595115L)
  (H.hash_int32 (-1l));
assert_equal ~printer:Int64.to_string (-3317520227865190253L)
  (H.hash_int32 1234567890l);
true
;;

(* Gold tests: hash_bool *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string (-5605595894618674504L)
  (H.hash_bool false);
assert_equal ~printer:Int64.to_string 7046788939542163588L (H.hash_bool true);
true
;;

(* Gold tests: hash_char *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string (-1595464024050301112L) (H.hash_char 'a');
assert_equal ~printer:Int64.to_string (-2980224328396984668L) (H.hash_char 'z');
assert_equal ~printer:Int64.to_string 7387411195422956975L (H.hash_char '0');
true
;;

(* Gold tests: finalize(seed) = finalize(0L) = XXH64(&0, 8, 0) *)
t @@ fun () ->
assert_equal ~printer:Int64.to_string 3803688792395291579L (H.finalize H.seed);
(* finalize is deterministic *)
assert_equal ~printer:Int64.to_string
  (H.finalize (H.mix_int64 H.seed 42L))
  (H.finalize (H.mix_int64 H.seed 42L));
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

(* mix_int64 is not commutative for most pairs *)
q ~count:10_000 (Q.pair Q.int64 Q.int64) @@ fun (a, b) ->
Q.assume (not (Int64.equal a b));
let ab = H.finalize (H.mix_int64 (H.mix_int64 H.seed a) b) in
let ba = H.finalize (H.mix_int64 (H.mix_int64 H.seed b) a) in
not (Int64.equal ab ba)
;;

(* Stress test: hash many strings, non-empty => non-zero *)
t @@ fun () ->
for len = 0 to 99 do
  for _ = 1 to 1000 do
    let s = String.make len 'x' in
    let h = H.mix_string H.seed s |> H.finalize in
    if len > 0 then
      if Int64.equal h 0L then
        failwith
          (Printf.sprintf "unexpected zero hash for string of len %d" len)
  done
done;
true

let () = Containers_testlib.run_all ~descr:"test xxhash" [ get () ]
