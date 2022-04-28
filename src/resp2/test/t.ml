open Containers_resp2
module Q = QCheck

let gen_data =
  Q.Gen.(
    let rec gen i =
      let alphanum =
        oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; char_range '0' '9' ]
      in
      let basic_str = string_size ~gen:alphanum (0 -- 20) in
      frequency
      @@ List.flatten
           [
             [
               (4, basic_str >|= fun s -> Data.Simple_string s);
               ( 3,
                 string_size ~gen:char (0 -- 30) >|= fun s -> Data.Bulk_string s
               );
               (4, 0 -- 100_000_000 >|= fun i -> Data.Int i);
               (1, basic_str >|= fun s -> Data.Error s);
             ];
             (if i > 2 then
               []
             else
               [
                 (1, list_size (0 -- 2) (gen (i + 1)) >|= fun l -> Data.Array l);
               ]);
           ]
    in
    gen 0)

let rec shrink_data d =
  let open Q.Iter in
  match d with
  | Data.Array l ->
    Q.Shrink.list ~shrink:shrink_data l >|= fun l -> Data.Array l
  | Data.Int i -> Q.Shrink.int i >|= fun i -> Data.Int i
  | Data.Bulk_string s -> Q.Shrink.string s >|= fun s -> Data.Bulk_string s
  | _ -> empty

let arb_data = Q.make ~print:Data.show ~shrink:shrink_data gen_data

let t_ser_deser_str =
  Q.Test.make ~name:"ser->deser str" arb_data @@ fun d ->
  let s = Print.to_string d in
  let d' = Parse.parse_string s in
  if d <> d' then
    Q.Test.fail_reportf "expected %a,@ got %a" Data.pp d Data.pp d';
  true

let t_ser_deser_chan =
  Q.Test.make ~name:"ser->deser chan" arb_data @@ fun d ->
  let ic, oc = Unix.pipe () in
  let ic = Unix.in_channel_of_descr ic in
  let oc = Unix.out_channel_of_descr oc in
  let out = ref (Error "no res") in
  let _th =
    Thread.create
      (fun () ->
        try
          let r = Parse.parse_chan ic in
          out := Ok r
        with e -> out := Error ("thread failed: " ^ Printexc.to_string e))
      ()
  in
  Print.to_chan oc d;
  flush oc;
  close_out oc;
  Thread.join _th;

  match !out with
  | Error err -> Q.Test.fail_reportf "thread did not parse value: %s" err
  | Ok d' ->
    if d <> d' then
      Q.Test.fail_reportf "expected %a,@ got %a" Data.pp d Data.pp d';
    true

let tests = [ t_ser_deser_str; t_ser_deser_chan ]
let () = QCheck_runner.run_tests_main tests
