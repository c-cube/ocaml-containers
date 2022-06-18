
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
    } -> run

  type t = {
    __FILE__: string;
    __LINE__: int;
    run: run;
  }

  (** Location for this test *)
  let str_loc (self:t) : string =
    Printf.sprintf "(test :file '%s' :line %d)" self.__FILE__ self.__LINE__

  let mk ~__FILE__ ~__LINE__ run : t =
    { __FILE__; __LINE__; run }

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
      | Q {count; arb; prop} ->

        (* create a random state from the seed *)
        let rand =
          let bits = String.to_seq seed |> Seq.map Char.code |> CCArray.of_seq in
          Random.State.make bits
        in

        let module Fmt = CCFormat in
        let cell = Q.Test.make_cell ?count ~name:(str_loc self) arb prop in

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

        begin match Q.TestResult.get_state res with
        | QCheck2.TestResult.Success -> Ok ()
        | QCheck2.TestResult.Failed { instances } ->
          let msg = Format.asprintf "@[<v2>failed on instances:@ %a@]"
            (Fmt.list ~sep:(Fmt.return ";@ ") pp_cex) instances
          in
          Error msg
        | QCheck2.TestResult.Failed_other {msg} ->
          let msg = spf "failed: %s" msg in
          Error msg
        | QCheck2.TestResult.Error {instance; exn; backtrace} ->
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

let all_ : Test.t list ref = ref []
let add_ t = all_ := t :: !all_

module Prelude = struct
  module Q = QCheck

  let t ~__FILE__ ~__LINE__ f : unit =
    add_ @@ Test.mk ~__FILE__ ~__LINE__ @@ Test.T f

  let eq ~__FILE__ ~__LINE__ ?cmp ?printer lhs rhs : unit =
    add_ @@ Test.mk ~__FILE__ ~__LINE__ @@ Test.Eq {eq=cmp; print=printer; lhs; rhs}

  let q ~__FILE__ ~__LINE__ ?count arb prop : unit =
    add_ @@ Test.mk ~__FILE__ ~__LINE__ @@ Test.Q {arb; prop; count}

  let assert_equal ?printer ?(cmp=(=)) x y : unit =
    if not @@ cmp x y then (
      match printer with
      | None -> failwith "not equal"
      | Some p ->
        failwith @@ spf "not equal: lhs=%s, rhs=%s" (p x) (p y)
    )
end

let run_all ?seed:seed_hex ~descr () : unit =
  let start = Unix.gettimeofday() in

  (* generate or parse seed *)

  let seed_hex = match seed_hex, Sys.getenv_opt "SEED" with
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

  let suite = List.rev !all_ in
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


