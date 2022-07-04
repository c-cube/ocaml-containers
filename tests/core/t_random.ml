open CCRandom
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

q Q.(list small_int) (fun l -> l = [] || List.mem (run (pick_list l)) l);;

q
  Q.(pair small_int small_int)
  (fun (i, j) ->
    let len, n = 2 + min i j, max i j in
    let l = QCheck.Gen.generate1 (split_list n ~len) in
    match l with
    | None -> true
    | Some l -> l <> [] && List.for_all (fun x -> x > 0) l)
;;

t @@ fun () ->
let open Containers in
ignore (List.random_choose [ 1; 2; 3 ] (Random.get_state ()) : int);
true
