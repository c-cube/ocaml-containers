open CCResult
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> Error "ohno 42" = fail_printf "ohno %d" 42;;
t @@ fun () -> Error "ohno 42" = fail_fprintf "ohno %d" 42;;

eq (Error "error\ncontext:message(number 42, foo: true)")
  (add_ctxf "message(number %d, foo: %B)" 42 true (Error "error"))
;;

t @@ fun () ->
let called_with = ref None in
let f e = called_with := Some e in
iter_err f (Ok 1);
assert (!called_with = None);
iter_err f (Error 1);
assert (!called_with = Some 1);
true
;;

t @@ fun () -> get_or_failwith (Ok 1) = 1;;

t @@ fun () ->
try
  ignore @@ get_or_failwith (Error "e");
  false
with Failure msg -> msg = "e"
;;

eq (get_lazy (fun _ -> 2) (Ok 1)) 1;;
eq (get_lazy (fun _ -> 2) (Error "error")) 2;;
eq 42 (fold_ok ( + ) 2 (Ok 40));;
eq 40 (fold_ok ( + ) 40 (Error "foo"));;
eq (Ok []) (flatten_l []);;
eq (Ok [ 1; 2; 3 ]) (flatten_l [ Ok 1; Ok 2; Ok 3 ]);;
eq (Error "ohno") (flatten_l [ Ok 1; Error "ohno"; Ok 2; Ok 3; Error "wut" ])
