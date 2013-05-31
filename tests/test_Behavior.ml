
open OUnit

module B = Behavior

let test_do () =
  let r = ref false in
  let t = B.mk_do_ok (fun () -> r := true) in
  let res = B.run t in
  OUnit.assert_equal true !r;
  OUnit.assert_equal (Some true) (React.S.value (B.Fut.wait res));
  ()

let test_seq () =
  let l = ref [] in
  (* add int to [l] *)
  let add x = l := x :: !l in
  let t = B.mk_sequence
    [ B.mk_do (fun () -> add 3; true);
      B.mk_do (fun () -> add 2; true);
      B.mk_test_fun (fun () -> List.length !l = 2);
      B.mk_do (fun () -> add 1; true);
    ] in
  let res = B.run t in
  OUnit.assert_equal [1;2;3] !l;
  OUnit.assert_equal (Some true) (React.S.value (B.Fut.wait res));
  ()

let test_wait () =
  let e, send_e = React.E.create () in
  let t = B.mk_sequence [B.mk_wait e; B.mk_succeed] in
  let signal = B.Fut.wait (B.run t) in
  OUnit.assert_equal None (React.S.value signal);
  send_e ();
  OUnit.assert_equal (Some true) (React.S.value signal);
  ()

let test_parallel () =
  let e, send_e = React.E.create () in
  (* forall fails *)
  let t =
    B.mk_parallel ~strat:B.PSForall
      [ B.mk_sequence [B.mk_wait e; B.mk_succeed];
        B.mk_fail
      ] in
  let signal = B.Fut.wait (B.run t) in
  OUnit.assert_equal (Some false) (React.S.value signal);
  send_e ();
  OUnit.assert_equal (Some false) (React.S.value signal);
  (* exists succeeds *)
  let t =
    B.mk_parallel ~strat:B.PSExists
      [ B.mk_sequence [B.mk_wait e; B.mk_succeed];
        B.mk_fail
      ] in
  let signal = B.Fut.wait (B.run t) in
  OUnit.assert_equal None (React.S.value signal);
  send_e ();
  OUnit.assert_equal (Some true) (React.S.value signal);
  ()


let suite =
  "test_behavior" >:::
    [ "test_do" >:: test_do;
      "test_seq" >:: test_seq;
      "test_wait" >:: test_wait;
      "test_parallel" >:: test_parallel;
    ]
