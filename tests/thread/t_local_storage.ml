module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
module L = CCLocal_storage;;

t @@ fun () ->
let var = L.create () in

let sum_of_res = CCAtomic.make 0 in
let n = 1_000 in

let run1 () =
  L.with_ var 0 @@ fun () ->
  for _i = 1 to n do
    let x = L.get_exn var in
    Thread.yield ();
    L.set var (x + 1)
  done;
  ignore (CCAtomic.fetch_and_add sum_of_res (L.get_exn var) : int)
in

let threads = Array.init 16 (fun _ -> Thread.create run1 ()) in
Array.iter Thread.join threads;

assert_equal ~printer:string_of_int (n * 16) (CCAtomic.get sum_of_res);

(* cleanup *)
assert_equal ~printer:string_of_int 0 (L.n_entries var);

true
