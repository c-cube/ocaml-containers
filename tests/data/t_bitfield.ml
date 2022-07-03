module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCBitField;;

t @@ fun () ->
let module B = CCBitField.Make () in
let x = B.mk_field () in
let y = B.mk_field () in
let z = B.mk_field () in

let f = B.empty |> B.set x true |> B.set y true in

assert_bool "z_false" (not (B.get z f));

assert_bool "z_true" (f |> B.set z true |> B.get z);
true
;;

t @@ fun () ->
let module B = CCBitField.Make () in
let _ = B.mk_field () in
B.freeze ();
assert_bool "must raise"
  (try
     ignore (B.mk_field ());
     false
   with Frozen -> true);
true
;;

t @@ fun () ->
let module B = CCBitField.Make () in
let x = B.mk_field () in
let y = B.mk_field () in
let z = B.mk_field () in
let u = B.mk_field () in
B.freeze ();

let f = B.empty |> B.set y true |> B.set z true in

assert_equal ~printer:CCInt.to_string 6 (f :> int);

assert_equal false (B.get x f);
assert_equal true (B.get y f);
assert_equal true (B.get z f);

let f' = B.set u true f in

assert_equal false (B.get x f');
assert_equal true (B.get y f');
assert_equal true (B.get z f');
assert_equal true (B.get u f');
true
;;

t @@ fun () -> all_bits_ 0 1 = 1;;
t @@ fun () -> all_bits_ 0 2 = 3;;
t @@ fun () -> all_bits_ 0 3 = 7;;
t @@ fun () -> all_bits_ 0 4 = 15
