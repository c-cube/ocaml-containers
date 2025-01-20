open CCFloat
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> is_nan (max nan 1.);;
t @@ fun () -> is_nan (min nan 1.);;
t @@ fun () -> is_nan (max 1. nan);;
t @@ fun () -> is_nan (min 1. nan);;

q
  Q.(pair float float)
  (fun (x, y) -> is_nan x || is_nan y || (min x y <= x && min x y <= y))
;;

q
  Q.(pair float float)
  (fun (x, y) -> is_nan x || is_nan y || (max x y >= x && max x y >= y))
;;

eq 2. (round 1.6);;
eq 1. (round 1.4);;
eq 0. (round 0.)
