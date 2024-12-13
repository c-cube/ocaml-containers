open CCSexp
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () ->
CCResult.to_opt (parse_string "(abc d/e/f \"hello \\\" () world\" )") <> None
;;

t @@ fun () ->
CCResult.to_opt (parse_string "(abc ( d e ffff   ) \"hello/world\")") <> None
;;

t @@ fun () -> CCResult.to_opt (parse_string "\"\123\bcoucou\"") <> None

let eq' =
  eq ~printer:(function
    | Ok x -> to_string x
    | Error e -> "error " ^ e)
;;

eq' (parse_string "(a b)") (Ok (`List [ `Atom "a"; `Atom "b" ]));;
eq' (parse_string "(a\n ;coucou\n b)") (Ok (`List [ `Atom "a"; `Atom "b" ]));;

eq'
  (parse_string "(a #; (foo bar\n (1 2 3)) b)")
  (Ok (`List [ `Atom "a"; `Atom "b" ]))
;;

eq' (parse_string "#; (a b) (c d)") (Ok (`List [ `Atom "c"; `Atom "d" ]));;
eq' (parse_string "#; (a b) 1") (Ok (`Atom "1"))

let eq' =
  eq ~printer:(function
    | Ok x -> String.concat ";" @@ List.map to_string x
    | Error e -> "error " ^ e)
;;

eq'
  (parse_string_list "(a b)(c)")
  (Ok [ `List [ `Atom "a"; `Atom "b" ]; `List [ `Atom "c" ] ])
;;

eq' (parse_string_list "  ") (Ok []);;

eq'
  (parse_string_list "(a\n ;coucou\n b)")
  (Ok [ `List [ `Atom "a"; `Atom "b" ] ])
;;

eq'
  (parse_string_list "#; (a b) (c d) e ")
  (Ok [ `List [ `Atom "c"; `Atom "d" ]; `Atom "e" ])
;;

eq' (parse_string_list "#; (a b) 1") (Ok [ `Atom "1" ])

let sexp_bijective s = to_string s |> parse_string = Ok s;;

eq
  ~printer:CCFormat.(to_string (Dump.result pp))
  (Ok (`List [ `Atom "" ]))
  (parse_string "(\"\")")
;;

t @@ fun () -> sexp_bijective (`List [ `Atom "" ])

let sexp_gen =
  let mkatom a = `Atom a and mklist l = `List l in
  let atom = Q.Gen.(map mkatom (string_size ~gen:printable (1 -- 30))) in
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

q ~count:100 sexp_gen sexp_bijective;;

(* regression for #338 *)
t @@ fun () ->
Printexc.record_backtrace true;
let cases =
  [
    "\"\\256\"";
    "\"\\722\02622222\\\\\n\r<\\\\\\\\\"\\222222222\\\\\"\"\2032!2222\\\\\"\"";
    "\"\n\r<\\t\023\n\203\\622222222\\\\\"\"\2032!2222\\\\\"\"";
    "\"\n\
     \r<@t\023\n\
     \203\\2222D2\n\
     \r22222\01622222222222222222222222\203\\292242\222 2\\\\\">K2";
    "\"\n\r<\\t\023\n\203\\272222222\\\\\"\"\2032\0042222\\\\\"\"";
    "\"\023\n\
     \203\\5222\n\
     \r<\\t\023\n\
     \203\\52222222\\\\\"2\\\216\216\216\216\216\\\\\"\216\216\216\216\216\216\216\216\216222222222222222\147";
    "\"\\722\02622222\\\\\n\r<\\\\\\\\\"\\222222222\\\\\"\"\2032!2222\\\\\"\"";
  ]
in
cases
|> List.iter (fun s ->
       try ignore (parse_string s)
       with e ->
         let st = Printexc.get_backtrace () in
         print_endline @@ Printexc.to_string e ^ "\n" ^ st;
         assert false);
true

module CS = CCCanonical_sexp

module Csexp_arg = struct
  open Csexp

  type t = Csexp.t

  let atom s = Atom s
  let list l = List l

  let match_ s ~atom ~list =
    match s with
    | Atom s -> atom s
    | List l -> list l

  type loc = unit

  let make_loc = None
  let atom_with_loc ~loc:() = atom
  let list_with_loc ~loc:() = list
end

module CS0 = CS.Make (Csexp_arg)
module Sexp0 = CCSexp.Make (Csexp_arg)

let gen_csexp (str : string Q.Gen.t) : CS0.t Q.Gen.t =
  let open Q.Gen in
  let open Csexp in
  ( fix @@ fun self depth ->
    let mklist n = list_size (0 -- n) (self (depth + 1)) >|= fun l -> List l in
    frequency
    @@ List.flatten
         [
           [ (3, str >|= fun s -> Atom s) ];
           (match depth with
           | 0 -> [ 4, mklist 25 ]
           | 1 -> [ 2, mklist 7 ]
           | 2 -> [ 1, mklist 2 ]
           | _ -> []);
         ] )
    0

let rec shrink_csexp (s : Csexp.t) : Csexp.t Q.Iter.t =
  let open Csexp in
  let open Q.Iter in
  match s with
  | Atom s -> Q.Shrink.string s >|= fun s -> Atom s
  | List l -> Q.Shrink.list ~shrink:shrink_csexp l >|= fun l -> List l

let arb_csexp_pp =
  let genstr = Q.Gen.(string_size ~gen:Q.Gen.printable (0 -- 15)) in
  Q.make ~print:Sexp0.to_string ~shrink:shrink_csexp (gen_csexp genstr)

let arb_csexp_arb =
  (* binary-ready *)
  let genchar = Q.Gen.(0 -- 255 >|= Char.chr) in
  let genstr = Q.Gen.(string_size ~gen:genchar (0 -- 15)) in
  Q.make ~print:Sexp0.to_string ~shrink:shrink_csexp (gen_csexp genstr)

module Make
    (X : sig
      val arb : Csexp.t Q.arbitrary
    end)
    () =
struct
  open X;;

  q ~count:2_000 arb @@ fun sexp ->
  let s = CS0.to_string sexp in
  match Csexp.parse_string s with
  | Ok sexp' -> sexp = sexp'
  | Error (_, msg) -> Q.Test.fail_report msg
  ;;

  q ~count:2_000 arb @@ fun sexp ->
  let s = Csexp.to_string sexp in
  match CS0.parse_string s with
  | Ok sexp' -> sexp = sexp'
  | Error msg -> Q.Test.fail_report msg

  let init () = ()
end

let () =
  let module M1 =
    Make
      (struct
        let arb = arb_csexp_pp
      end)
      ()
  in
  let module M2 =
    Make
      (struct
        let arb = arb_csexp_arb
      end)
      ()
  in
  M1.init ();
  M2.init ();
  ()
