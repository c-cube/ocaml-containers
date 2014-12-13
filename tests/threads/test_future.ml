
(** Test Future *)

open OUnit

module Future = CCFuture

let test_mvar () =
  let box = Future.MVar.empty () in
  let f = Future.spawn (fun () -> Future.MVar.take box + 1) in
  Thread.delay 0.1;
  OUnit.assert_bool "still waiting" (not (Future.is_done f));
  Future.MVar.put box 1;
  OUnit.assert_equal 2 (Future.get f);
  ()

let test_parallel () =
  let l = Sequence.(1 -- 300) in
  let l = Sequence.map (fun _ -> Future.spawn (fun () -> Thread.delay 0.1; 1)) l in
  let l = Sequence.to_list l in
  let l' = List.map Future.get l in
  OUnit.assert_equal 300 (List.fold_left (+) 0 l');
  ()

let test_time () =
  let start = Unix.gettimeofday () in
  let f1 = Future.spawn (fun () -> Thread.delay 0.5) in
  let f2 = Future.spawn (fun () -> Thread.delay 0.5) in
  Future.get f1;
  Future.get f2;
  let stop = Unix.gettimeofday () in
  OUnit.assert_bool "parallelism" (stop -. start < 0.75);
  ()

let test_timer () =
  let timer = Future.Timer.create () in
  let mvar = Future.MVar.full 1 in
  Future.Timer.schedule_in timer 0.5
    (fun () -> ignore (Future.MVar.update mvar (fun x -> x + 2)));
  Future.Timer.schedule_in timer 0.2
    (fun () -> ignore (Future.MVar.update mvar (fun x -> x * 4)));
  Thread.delay 0.7;
  OUnit.assert_equal 6 (Future.MVar.peek mvar);
  ()

let suite =
  "test_future" >:::
    [ "test_mvar" >:: test_mvar;
      "test_parallel" >:: test_parallel;
      "test_time" >:: test_time;
      "test_timer" >:: test_timer;
    ]
