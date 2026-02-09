open CCEqual
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

q
  Q.(
    let p = list_small (pair nat_small bool) in
    pair p p)
  (fun (l1, l2) -> (list (pair int bool)) l1 l2 = (l1 = l2))
