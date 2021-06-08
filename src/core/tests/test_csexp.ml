
module Q = QCheck
module CS = CCCanonical_sexp


module Csexp_arg = struct
  open Csexp
  type t = Csexp.t
  let atom s = Atom s
  let list l = List l
  let match_ s ~atom ~list = match s with
    | Atom s -> atom s
    | List l -> list l
  type loc = unit
  let make_loc = None
  let atom_with_loc ~loc:() = atom
  let list_with_loc ~loc:() = list
end

module CS0 = CS.Make(Csexp_arg)
module Sexp0 = CCSexp.Make(Csexp_arg)

let gen_csexp (str:string Q.Gen.t) : CS0.t Q.Gen.t =
  let open Q.Gen in
  let open Csexp in
  begin fix @@ fun self depth ->
    let mklist n =
      list_size (0 -- n) (self (depth+1)) >|= fun l -> List l
    in
    frequency @@ List.flatten [
      [(3, str
        >|= fun s -> Atom s)];
      (match depth with
        | 0 -> [4,mklist 25]
        | 1 -> [2,mklist 7]
        | 2 -> [1,mklist 2]
        | _ -> []);
    ]
  end 0

let rec shrink_csexp (s:Csexp.t) : Csexp.t Q.Iter.t =
  let open Csexp in
  let open Q.Iter in
  match s with
    | Atom s -> Q.Shrink.string s >|= fun s -> Atom s
    | List l -> Q.Shrink.list ~shrink:shrink_csexp l >|= fun l -> List l

let arb_csexp_pp =
  let genstr = Q.Gen.(string_size ~gen:Q.Gen.printable (0--15)) in
  Q.make ~print:Sexp0.to_string
    ~shrink:shrink_csexp (gen_csexp genstr)

let arb_csexp_arb =
  (* binary-ready *)
  let genchar = Q.Gen.(0 -- 255 >|=Char.chr) in
  let genstr = Q.Gen.(string_size ~gen:genchar (0--15)) in
  Q.make
    ~print:Sexp0.to_string
    ~shrink:shrink_csexp (gen_csexp genstr)

module Make(X : sig val arb : Csexp.t Q.arbitrary end) = struct
  open X
  let test_print_cc_parse_csexp =
    Q.Test.make ~count:2_000 ~name:"cc-print-csexp-parse" arb @@ fun sexp ->
    let s = CS0.to_string sexp in
    match Csexp.parse_string s with
      | Ok sexp' -> sexp = sexp'
      | Error (_,msg) -> Q.Test.fail_report msg

  let test_print_csexp_parse_cc =
    Q.Test.make ~count:2_000 ~name:"cc-parse-csexp-print" arb @@ fun sexp ->
    let s = Csexp.to_string sexp in
    match CS0.parse_string s with
      | Ok sexp' -> sexp = sexp'
      | Error msg -> Q.Test.fail_report msg

  let suite = [test_print_cc_parse_csexp; test_print_csexp_parse_cc ]
end

let suite =
  let module M1 = Make(struct let arb=arb_csexp_pp end) in
  let module M2 = Make(struct let arb=arb_csexp_arb end) in
  List.flatten [M1.suite; M2.suite]

let () =
  QCheck_base_runner.run_tests_main suite

