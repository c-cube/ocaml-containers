
open OUnit

module B = Behavior

let lwt_get fut = match Lwt.state fut with
  | Lwt.Sleep
  | Lwt.Fail _ -> None
  | Lwt.Return x -> Some x

let test_do () =
  let r = ref false in
  let t = B.do_succeed (fun () -> r := true) in
  let res = B.run t in
  OUnit.assert_equal true !r;
  OUnit.assert_equal (Some true) (lwt_get res);
  ()

let test_seq () =
  let l = ref [] in
  (* add int to [l] *)
  let add x = l := x :: !l in
  let t = B.sequence
    [ B.do_ (fun () -> add 3; true);
      B.do_ (fun () -> add 2; true);
      B.test (fun () -> List.length !l = 2);
      B.do_ (fun () -> add 1; true);
    ] in
  let res = B.run t in
  OUnit.assert_equal [1;2;3] !l;
  OUnit.assert_equal (Some true) (lwt_get res);
  ()

let test_wait () =
  let e, send_e = Lwt.wait () in
  let t = B.run (B.sequence [B.wait_ e; B.succeed]) in
  OUnit.assert_equal None (lwt_get t);
  Lwt.wakeup send_e ();
  OUnit.assert_equal (Some true) (lwt_get t);
  ()

let test_parallel () =
  (* forall fails *)
  let e, send_e = Lwt.wait () in
  let t =
    B.parallel ~strat:B.PSForall
      [ B.sequence [B.wait_ e; B.succeed];
        B.fail
      ] in
  let t = B.run t in
  let res = Lwt_main.run
    (let open Lwt in
      choose [t; Lwt_unix.sleep 0.1 >>= fun () -> (Lwt.wakeup send_e (); return true)])
  in
  OUnit.assert_equal false res;
  (* exists succeeds *)
  let e, send_e = Lwt.wait () in
  let t =
    B.parallel ~strat:B.PSExists
      [ B.sequence [B.wait_ e; B.succeed];
        B.fail
      ] in
  let t = B.run t in
  let res = Lwt_main.run
    (let open Lwt in
      choose [t; Lwt_unix.sleep 0.1 >>= fun () -> (Lwt.wakeup send_e ();
        Lwt_unix.sleep 0.1 >>= (fun () -> return true))])
  in
  OUnit.assert_equal true res;
  ()


let suite =
  "test_behavior" >:::
    [ "test_do" >:: test_do;
      "test_seq" >:: test_seq;
      "test_wait" >:: test_wait;
      "test_parallel" >:: test_parallel;
    ]
