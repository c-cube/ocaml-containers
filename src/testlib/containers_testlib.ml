
module Q = QCheck
let spf = Printf.sprintf

type 'a eq = 'a -> 'a -> bool
type 'a print = 'a -> string

module Test = struct
  type run =
    | T of (unit -> bool)
    | Eq : {
      eq: 'a eq option;
      print: 'a print option;
      lhs: 'a;
      rhs: 'a;
    } -> run
    | Q : {
      count: int option;
      arb: 'a Q.arbitrary;
      prop: 'a -> bool;
      long_factor: int option;
    } -> run

  type t = {
    run: run;
    __FILE__: string;
    n: int;
  }

  (** Location for this test *)
  let str_loc (self:t) : string =
    Printf.sprintf "(test :file '%s' :n %d)" self.__FILE__ self.n


[@@@ifge 4.08]

  let get_state (r:_ QCheck.TestResult.t) : _ QCheck.TestResult.state =
    QCheck.TestResult.get_state r

[@@@else_]

  (* must have qcheck < 0.17 *)
  let get_state (r:_ QCheck.TestResult.t) : _ QCheck.TestResult.state =
    r.state

[@@@endif]

  let run ~seed (self:t) : _ result =
    match
      match self.run with
      | T f ->
        if f() then Ok() else Error "failed: returns false"
      | Eq {eq; print; lhs; rhs} ->
        let eq = match eq with Some f->f | None -> (=) in
        if eq lhs rhs then Ok ()
        else (
          let msg = match print with
            | None -> "failed: not equal"
            | Some p -> spf "failed: not equal:\nlhs=%s\nrhs=%s" (p lhs) (p rhs)
          in
          Error msg
        )
      | Q {count; arb; prop; long_factor} ->

        (* create a random state from the seed *)
        let rand =
          let bits = CCString.to_list seed |> List.map Char.code |> Array.of_list in
          Random.State.make bits
        in

        let module Fmt = CCFormat in
        let cell = Q.Test.make_cell ?count ?long_factor ~name:(str_loc self) arb prop in

        let pp_cex out (cx: _ Q.TestResult.counter_ex) =
          let {Q.TestResult.instance; shrink_steps=n; msg_l} = cx in
          let msg_l = if msg_l =[] then "" else "\n" ^ String.concat "\n" msg_l in
          match arb.print with
          | None -> Fmt.fprintf out "<instance> (after %d shrink steps)%s" n msg_l
          | Some p ->
            Fmt.fprintf out "`%s` (after %d shrink steps)%s" (p instance) n msg_l
        in

        (* TODO: if verbose, print stats, etc. *)

        let res = Q.Test.check_cell ~rand cell in

        begin match get_state res with
        | QCheck.TestResult.Success -> Ok ()
        | QCheck.TestResult.Failed { instances } ->
          let msg = Format.asprintf "@[<v2>failed on instances:@ %a@]"
            (Fmt.list ~sep:(Fmt.return ";@ ") pp_cex) instances
          in
          Error msg
        | QCheck.TestResult.Failed_other {msg} ->
          let msg = spf "failed: %s" msg in
          Error msg
        | QCheck.TestResult.Error {instance; exn; backtrace} ->
          let msg = Format.asprintf "@[<v2>raised %s@ on instance %a@ :backtrace %s@]"
            (Printexc.to_string exn) pp_cex instance backtrace
          in
          Error msg
        end
    with
    | res -> res
    | exception e ->
        Error (spf "failed: raised %s" (Printexc.to_string e))
end

module type S = sig
  module Q = QCheck

  val t : (unit -> bool) -> unit

  val eq : ?cmp:'a eq -> ?printer:'a print -> 'a -> 'a -> unit

  val q : ?count:int -> ?long_factor:int -> 'a Q.arbitrary -> ('a -> bool) -> unit

  val assert_equal :
    ?printer:('a -> string) -> ?cmp:('a -> 'a -> bool) ->
    'a -> 'a -> unit

  val assert_bool : string -> bool -> unit

  val assert_failure : string -> 'a

  val assert_raises : (exn -> bool) -> (unit -> 'b) -> unit

  val get : unit -> Test.t list
end

module Make_test(X:sig val file: string end) = struct
  module Q = QCheck

  let all_ : Test.t list ref = ref []
  let add_ t = all_ := t :: !all_

  let n_ = ref 0

  let mk run : Test.t =
    let n = !n_ in
    incr n_;
    { __FILE__=X.file; n; run }

  let t f : unit =
    add_ @@ mk @@ Test.T f

  let eq ?cmp ?printer lhs rhs : unit =
    add_ @@ mk @@ Test.Eq {eq=cmp; print=printer; lhs; rhs}

  let q ?count ?long_factor arb prop : unit =
    add_ @@ mk @@ Test.Q {arb; prop; count; long_factor}

  let assert_equal ?printer ?(cmp=(=)) x y : unit =
    if not @@ cmp x y then (
      match printer with
      | None -> failwith "not equal"
      | Some p ->
        failwith @@ spf "not equal: lhs=%s, rhs=%s" (p x) (p y)
    )

  let assert_bool what b =
    if not b then (
      failwith what
    )

  let assert_failure s = failwith s

  let assert_raises check f =
    try ignore (f()); failwith "did not raise"
    with e ->
      if check e then ()
      else failwith ("raised unexpected exception " ^ Printexc.to_string e)

  let get () = !all_
end

let make ~__FILE__ () : (module S) =
  let module M = Make_test(struct
    let file = __FILE__
  end) in
  (module M)

let getenv_opt s = try Some (Sys.getenv s) with _ -> None

let run_all ?seed:seed_hex ~descr (l:Test.t list list) : unit =
  let start = Unix.gettimeofday() in

  (* generate or parse seed *)

  let seed_hex = match seed_hex, getenv_opt "SEED" with
    | Some s, _ -> s
    | None, Some s -> s
    | None, None ->
      Random.self_init();
      let a = CCList.init 8 (fun _ -> Random.int 256 |> Char.chr) in
      CCString.to_hex @@ CCString.of_list a
  in

  let seed = match CCString.of_hex seed_hex with
    | Some s->s
    | None ->
      Format.printf "error: seed must be a hex string: %S@." seed_hex; exit 1
  in

  Format.printf "seed: %s@." seed_hex;

  (* now run the suite *)

  let suite = List.flatten l in
  Format.printf "testing %s: running %d tests…@." descr (List.length suite);
  let failed = ref [] in

  List.iter (fun t ->
    (*
      NOTE: we probably want this to be silent?
      Format.printf "> run %s@." (Test.str_loc t);
      *)
    match Test.run ~seed t with
    | Ok () -> ()
    | Error msg ->
      Format.printf "FAILED: %s@." (Test.str_loc t);
      failed := (Test.str_loc t, msg ) :: !failed;
  ) suite;

  Format.printf "%d tests done in %.3fs@." (List.length suite)
    (Unix.gettimeofday() -. start);

  match !failed with
  | [] ->
    Format.printf "OK@."
  | _f ->
    Format.printf "ERROR (%d failures)@." (List.length _f);
    List.iter (fun (w,msg) ->
      Format.printf "@.========@.failed %s:@.%s@." w msg)
    _f;
    exit 1


