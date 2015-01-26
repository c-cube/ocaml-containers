
(** Test Future *)

open OUnit
open CCFun

module Future = CCFuture
open Future.Infix

let test_parallel n () =
  let l = Sequence.(1 -- n) |> Sequence.to_list in
  let l = List.map (fun i ->
    Future.make
      (fun () ->
        Thread.delay 0.1;
        1
    )) l in
  let l' = List.map Future.get l in
  OUnit.assert_equal n (List.fold_left (+) 0 l');
  ()

let test_map () =
  let a = Future.make (fun () -> 1) in
  let b = Future.map (fun x -> x+1) a in
  let c = Future.map (fun x -> x-1) b in
  OUnit.assert_equal 1 (Future.get c)

let test_sequence_ok () =
  let l = CCList.(1 -- 10) in
  let l' = l
    |> List.map
      (fun x -> Future.make (fun () -> Thread.delay 0.2; x*10))
    |> Future.sequence
    |> Future.map (List.fold_left (+) 0)
  in
  let expected = List.fold_left (fun acc x -> acc + 10 * x) 0 l in
  OUnit.assert_equal expected (Future.get l')

let test_sequence_fail () =
  let l = CCList.(1 -- 10) in
  let l' = l
    |> List.map
      (fun x -> Future.make (fun () -> Thread.delay 0.2; if x = 5 then raise Exit; x))
    |> Future.sequence
    |> Future.map (List.fold_left (+) 0)
  in
  OUnit.assert_raises Exit (fun () -> Future.get l')

let test_time () =
  let start = Unix.gettimeofday () in
  let l = CCList.(1 -- 10)
    |> List.map (fun _ -> Future.make (fun () -> Thread.delay 0.5))
  in
  List.iter Future.get l;
  let stop = Unix.gettimeofday () in
  OUnit.assert_bool "some_parallelism" (stop -. start < 10. *. 0.5);
  ()

let test_timer () =
  let timer = Future.Timer.create () in
  let n = CCLock.create 1 in
  let get = Future.make (fun () -> Thread.delay 0.8; CCLock.get n) in
  let _ =
    Future.Timer.after timer 0.6
    >>= fun () -> CCLock.update n (fun x -> x+2); Future.return()
  in
  let _ =
    Future.Timer.after timer 0.4
    >>= fun () -> CCLock.update n (fun x -> x * 4); Future.return()
  in
  OUnit.assert_equal 6 (Future.get get);
  ()

let suite =
  "test_future" >:::
    [ 
      "test_parallel_10" >:: test_parallel 10;
      "test_parallel_300" >:: test_parallel 300;
      "test_time" >:: test_time;
      "test_map" >:: test_map;
      "test_sequence_ok" >:: test_sequence_ok;
      "test_sequence_fail" >:: test_sequence_fail;
      "test_timer" >:: test_timer;
    ]

let () =
  let _ = OUnit.run_test_tt_main suite in
  ()
