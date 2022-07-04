open CCCanonical_sexp
module T = (val Containers_testlib.make ~__FILE__ ())
include T

let csexp_bijective s = to_string s |> parse_string = Ok s;;

eq
  ~printer:CCFormat.(to_string (Dump.result pp))
  (Ok (`List [ `Atom "" ]))
  (parse_string {|(0:)|})
;;

eq
  ~printer:CCFormat.(to_string (Dump.result pp))
  (Ok (`List [ `Atom "a"; `Atom "b " ]))
  (parse_string {|(1:a2:b )|})
;;

t @@ fun () -> csexp_bijective (`List [ `Atom "" ])

let sexp_gen =
  let mkatom a = `Atom a and mklist l = `List l in
  let atom = Q.Gen.(map mkatom (string_size ~gen:char (1 -- 30))) in
  let gen =
    Q.Gen.(
      sized
        (fix (fun self n st ->
             match n with
             | 0 -> atom st
             | _ ->
               frequency
                 [
                   1, atom; 2, map mklist (list_size (0 -- 10) (self (n / 10)));
                 ]
                 st)))
  in
  let rec small = function
    | `Atom s -> String.length s
    | `List l -> List.fold_left (fun n x -> n + small x) 0 l
  and print = function
    | `Atom s -> Printf.sprintf "`Atom \"%s\"" s
    | `List l -> "`List " ^ Q.Print.list print l
  and shrink = function
    | `Atom s -> Q.Iter.map mkatom (Q.Shrink.string s)
    | `List l -> Q.Iter.map mklist (Q.Shrink.list ~shrink l)
  in
  Q.make ~print ~small ~shrink gen
;;

q ~count:100 sexp_gen csexp_bijective;;

t @@ fun () ->
let s1 =
  `List
    (CCList.init 100_000 (fun i ->
         `List [ `Atom "-"; `Atom (string_of_int i); `Atom ")(\n]" ]))
in
let str = to_string s1 in
(match parse_string str with
| Ok s2 -> assert_equal s1 s2
| Error e -> assert_failure e);
true
