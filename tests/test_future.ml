
(** Test Future *)

open OUnit

let test_mvar () =
  let box = Future.MVar.empty () in
  let f = Future.spawn (fun () -> Future.MVar.take box + 1) in
  Thread.delay 0.1;
  OUnit.assert_bool "still waiting" (not (Future.is_done f));
  Future.MVar.put box 1;
  OUnit.assert_equal 2 (Future.get f);
  ()

let test_parallel () =
  let open Enum.Infix in
  let l = 1 -- 300
    |> Enum.map (fun _ -> Future.spawn (fun () -> Thread.delay 0.1; 1))
    |> Enum.to_list in
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

let suite =
  "test_future" >:::
    [ "test_mvar" >:: test_mvar;
      "test_parallel" >:: test_parallel;
      "test_time" >:: test_time;
    ]
