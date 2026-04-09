module T = (val Containers_testlib.make ~__FILE__ ())
include T
open CCByte_slice;;

(* --- of_string --- *)

t @@ fun () ->
let sl = of_string "hello" in
len sl = 5
;;

t @@ fun () ->
let sl = of_string "hello" in
contents sl = "hello"
;;

t @@ fun () ->
let sl = of_string "" in
len sl = 0
;;

(* --- clear --- *)

t @@ fun () ->
let sl = of_string "hello" in
clear sl;
len sl = 0
;;

t @@ fun () ->
(* after clear, get raises *)
let sl = of_string "hello" in
clear sl;
(try
   let _ = get sl 0 in
   false
 with Invalid_argument _ -> true)
