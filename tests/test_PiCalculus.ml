
open OUnit

open Containers_misc
open PiCalculus
module Pi = PiCalculus

let test_message () =
  let r = ref 0 in
  let p1 = new_ 
    (fun c ->
      send_one c 1 stop |||
      receive_one c (fun x -> r := x; stop))
  in
  Pi.run p1;
  OUnit.assert_equal ~printer:string_of_int 1 !r;
  ()

let test_replicate () =
  let a = ref 0 in
  let b = ref 0 in
  let p1 = new_
    (fun c ->
      replicate (escape (fun () -> incr a; send_one c !a stop)) |||
      receive_one c (fun _ -> receive_one c (fun x -> b := x; stop)))
  in
  run p1;
  OUnit.assert_equal ~printer:string_of_int 2 !b;
  ()

let suite =
  "test_PiCalculus" >:::
    [ "test_message" >:: test_message;
      "test_replicate" >:: test_replicate;
    ]
