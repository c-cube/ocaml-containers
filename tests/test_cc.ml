(** Tests for congruence closure *)

open OUnit

let parse = CC.parse
let pp = CC.pp

module CT = CC.StrTerm
module CC = CC.StrCC

let test_add () =
  let cc = CC.create 5 in
  let t = parse "((a (b c)) d)" in
  OUnit.assert_equal ~cmp:CT.eq t t;
  let t2 = parse "(f (g (h x)))" in
  OUnit.assert_bool "not eq" (not (CC.eq cc t t2));
  ()

let test_merge () =
  let t1 = parse "((f (a b)) c)" in
  let t2 = parse "((f (a b2)) c2)" in
  (* Format.printf "t1=%a, t2=%a@." pp t1 pp t2; *)
  let cc = CC.create 5 in
  (* merge b and b2 *)
  let cc = CC.merge cc (parse "b") (parse "b2") in
  OUnit.assert_bool "not eq" (not (CC.eq cc t1 t2));
  OUnit.assert_bool "eq_sub" (CC.eq cc (parse "b") (parse "b2"));
  (* merge c and c2 *)
  let cc = CC.merge cc (parse "c") (parse "c2") in
  OUnit.assert_bool "eq_sub" (CC.eq cc (parse "c") (parse "c2"));
  (* Format.printf "t1=%a, t2=%a@." pp (CC.normalize cc t1) pp (CC.normalize cc t2); *)
  OUnit.assert_bool "eq" (CC.eq cc t1 t2);
  ()

let test_merge2 () =
  let cc = CC.create 5 in
  let cc = CC.distinct cc (parse "a") (parse "b") in
  let cc = CC.merge cc (parse "(f c)") (parse "a") in
  let cc = CC.merge cc (parse "(f d)") (parse "b") in
  OUnit.assert_bool "not_eq" (not (CC.can_eq cc (parse "a") (parse "b")));
  OUnit.assert_bool "inconsistent"
    (try ignore (CC.merge cc (parse "c") (parse "d")); false
     with CC.Inconsistent _ -> true);
  ()

let test_merge3 () =
  let cc = CC.create 5 in
  (* f^3(a) = a *)
  let cc = CC.merge cc (parse "a") (parse "(f (f (f a)))") in
  OUnit.assert_equal ~cmp:CT.eq (parse "(f (f a))") (parse "(f (f a))");
  (* f^4(a) = a *)
  let cc = CC.merge cc (parse "(f (f (f (f (f a)))))") (parse "a") in
  (* CC.iter_equiv_class cc (parse "a") (fun t -> Format.printf "a = %a@." pp t); *)
  (* hence, f^5(a) = f^2(f^3(a)) = f^2(a), and f^3(a) = f(f^2(a)) = f(a) = a *)
  OUnit.assert_bool "eq" (CC.eq cc (parse "a") (parse "(f a)"));
  ()

let test_merge4 () =
  let cc = CC.create 5 in
  let cc = CC.merge cc (parse "true") (parse "(p (f (f (f (f (f (f a)))))))") in
  let cc = CC.merge cc (parse "a") (parse "(f b)") in
  let cc = CC.merge cc (parse "(f a)") (parse "b") in
  OUnit.assert_bool "eq" (CC.eq cc (parse "a") (parse "(f (f (f (f (f (f a))))))"));
  ()

let test_explain () =
  let cc = CC.create 5 in
  (* f^3(a) = a *)
  let cc = CC.merge cc (parse "a") (parse "(f (f (f a)))") in
  (* f^4(a) = a *)
  let cc = CC.merge cc (parse "(f (f (f (f (f a)))))") (parse "a") in
  (* Format.printf "t: %a@." pp (parse "(f (f (f (f (f a)))))"); *)
  (* hence, f^5(a) = f^2(f^3(a)) = f^2(a), and f^3(a) = f(f^2(a)) = f(a) = a *)
  let l = CC.explain cc (parse "a") (parse "(f (f a))") in
  (*
  List.iter
    (function
    | CC.ByMerge (a,b) -> Format.printf "merge %a %a@." pp a pp b
    | CC.ByCongruence (a,b) -> Format.printf "congruence %a %a@." pp a pp b)
    l;
    *)
  OUnit.assert_equal 4 (List.length l);
  ()

let suite =
  "test_cc" >:::
    [ "test_add" >:: test_add;
      "test_merge" >:: test_merge;
      "test_merge2" >:: test_merge2;
      "test_merge3" >:: test_merge3;
      "test_merge4" >:: test_merge4;
      "test_explain" >:: test_explain;
    ]
