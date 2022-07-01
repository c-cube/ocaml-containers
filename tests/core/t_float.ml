
open CCFloat

module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> max nan 1. = 1.;;
t @@ fun () -> min nan 1. = 1.;;
t @@ fun () -> max 1. nan = 1.;;
t @@ fun () -> min 1. nan = 1.;;

q   Q.(pair float float) (fun (x,y) ->
    is_nan x || is_nan y || (min x y <= x && min x y <= y));;
q   Q.(pair float float) (fun (x,y) ->
    is_nan x || is_nan y || (max x y >= x && max x y >= y));;

eq  2. (round 1.6);;
eq  1. (round 1.4);;
eq  0. (round 0.);;
