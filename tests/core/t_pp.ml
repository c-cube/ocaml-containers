include (val Containers_testlib.make ~__FILE__ ())
open Containers_pp

let spf = Printf.sprintf

let () =
  eq "hello world" (Flatten.to_string @@ text "hello" ^ newline ^ text "world")

let () =
  eq ~name:"split text" ~printer:(spf "%S") "let rec f x =\n x+2\n"
    (let d = text "let rec f x =\n x+2\n" in
     Pretty.to_string ~width:15 d)

let () =
  eq ~name:"l1" ~printer:(spf "%S") "[0; 1; 2; 3;\n 4; 5; 6; 7;\n 8; 9]"
    (let d = Dump.list (CCList.init 10 int) in
     Pretty.to_string ~width:10 d)

let () =
  eq ~name:"l2" ~printer:(spf "%S")
    "[[0; 1; 2; 3;\n\
    \  4; 5];\n\
    \ [1; 2; 3; 4;\n\
    \  5; 6];\n\
    \ [2; 3; 4; 5;\n\
    \  6; 7];\n\
    \ [3; 4; 5; 6;\n\
    \  7; 8];\n\
    \ [4; 5; 6; 7;\n\
    \  8; 9];\n\
    \ [5; 6; 7; 8;\n\
    \  9; 10]]"
    (let d =
       Dump.list
         (CCList.init 6 (fun i ->
              Dump.list (CCList.init 6 (fun j -> int @@ (i + j)))))
     in
     Pretty.to_string ~width:10 d)

let () =
  eq ~name:"s1" ~printer:(spf "%S") "(foo\n bar\n baaz\n (g 42 10))"
    (let d =
       sexp_apply "foo"
         [ text "bar"; text "baaz"; sexp_apply "g" [ int 42; int 10 ] ]
     in
     Pretty.to_string ~width:10 d)

let ext_coucou =
  Ext.make ~name:"coucou"
    ~pre:(fun out ~inside:_ () -> out.string "<coucou>")
    ~post:(fun out ~inside:_ () -> out.string "</coucou>")
    ()

let () =
  eq ~name:"wrap1" ~printer:(spf "%S")
    "(foo\n bar\n <coucou>(g 42 10)</coucou>)"
    (let d =
       sexp_apply "foo"
         [ text "bar"; ext ext_coucou () (sexp_apply "g" [ int 42; int 10 ]) ]
     in
     Pretty.to_string ~width:10 d)

let () =
  eq ~name:"nested color" ~printer:(spf "%S")
    "hello \027[32mworld \027[31moh my!\027[32m!?\027[43moh well\027[32m\027[0m"
    (let d =
       text "hello"
       ^+ Term_color.color `Green
            (text "world"
            ^+ Term_color.color `Red (text "oh my!")
            ^ text "!?"
            ^ Term_color.style_l [ `BG `Yellow ] (text "oh well"))
     in
     Pretty.to_string ~width:1000 d)

let () =
  t ~name:"disable nested termcolor without changing the document" (fun () ->
      let plain =
        group (text "hello" ^ nest 2 (nl ^ text "world" ^ nl ^ text "again"))
      in
      let styled =
        group
          (text "hello"
          ^ nest 2
              (nl
              ^ Term_color.color `Green
                  (text "world" ^ nl
                  ^ Term_color.style_l [ `Bold ] (text "again"))))
      in
      List.iter
        (fun width ->
          let original = Pretty.to_string ~width styled in
          assert_equal ~printer:(spf "%S")
            (Pretty.to_string ~width plain)
            (Pretty.to_string_with ~disabled_ext:[ "termcolor" ] ~width styled);
          assert_equal original (Pretty.to_string ~width styled);
          assert_equal original
            (Pretty.to_string_with ~disabled_ext:[] ~width styled);
          assert_equal original
            (Pretty.to_string_with ~disabled_ext:[ "TERMCOLOR"; "unknown" ]
               ~width styled))
        [ 8; 80 ];
      true)

let () =
  t ~name:"disabled callbacks and enabled extension nesting" (fun () ->
      let calls = ref [] in
      let e =
        Ext.make ~name:"marker"
          ~pre:(fun out ~inside v ->
            calls := ("pre", inside, v) :: !calls;
            out.string (spf "<%d>" v))
          ~post:(fun out ~inside v ->
            calls := ("post", inside, v) :: !calls;
            out.string (spf "</%d>" v))
          ()
      in
      let plain = group (text "a" ^ nl ^ text "b" ^ nl ^ text "c") in
      let d =
        group (ext e 1 (text "a" ^ nl ^ ext e 2 (text "b") ^ nl ^ text "c"))
      in
      List.iter
        (fun width ->
          calls := [];
          let original = Pretty.to_string ~width d in
          assert_equal
            [
              "pre", None, 1;
              "pre", Some 1, 2;
              "post", Some 1, 2;
              "post", None, 1;
            ]
            (List.rev !calls);
          calls := [];
          assert_equal original
            (Pretty.to_string_with ~disabled_ext:[ "unknown" ] ~width d);
          assert_equal 4 (List.length !calls);
          calls := [];
          assert_equal
            (Pretty.to_string ~width plain)
            (Pretty.to_string_with ~disabled_ext:[ "marker"; "marker" ] ~width d);
          assert_equal [] !calls)
        [ 1; 80 ];
      true)

let () =
  t ~name:"rebuild group fill and nest widths when removing extensions"
    (fun () ->
      let width_calls = ref 0 in
      let callbacks = ref 0 in
      let weighted =
        Ext.make ~name:"weighted"
          ~width:(fun () ->
            incr width_calls;
            2)
          ~pre:(fun out ~inside:_ () ->
            incr callbacks;
            out.char '[')
          ~post:(fun out ~inside:_ () ->
            incr callbacks;
            out.char ']')
          ()
      in
      let wrap d = ext weighted () d in
      let wrapped = group (wrap (text "a") ^ nl ^ text "bbb") in
      let plain = group (text "a" ^ nl ^ text "bbb") in
      let cases =
        [
          wrapped, plain;
          text "p" ^ nest 2 (nl ^ wrapped), text "p" ^ nest 2 (nl ^ plain);
          ( group
              (fill (wrap nl)
                 [ wrap (text "aa"); text "bb"; wrap (text "ccc") ]),
            group (fill nl [ text "aa"; text "bb"; text "ccc" ]) );
          ( group (wrap nil ^ nl ^ wrap (text "a\nbb\n")),
            group (nil ^ nl ^ text "a\nbb\n") );
          ext ext_coucou () wrapped, ext ext_coucou () plain;
        ]
      in
      let original_width_calls = !width_calls in
      List.iter
        (fun width ->
          List.iter
            (fun (d, plain) ->
              assert_equal ~printer:(spf "%S")
                (Pretty.to_string ~width plain)
                (Pretty.to_string_with ~disabled_ext:[ "weighted" ] ~width d))
            cases)
        [ 1; 2; 3; 4; 5; 6; 10; 80 ];
      assert_equal 0 !callbacks;
      assert_equal original_width_calls !width_calls;
      true)

let () =
  t ~name:"suppress all keys with the same extension name" (fun () ->
      let fail out ~inside:_ () = Out.string out "unexpected" in
      let a = Ext.make ~name:"shared" ~pre:fail ~post:fail () in
      let b = Ext.make ~name:"shared" ~pre:fail ~post:fail () in
      assert_equal "inner"
        (Pretty.to_string_with ~disabled_ext:[ "shared" ] ~width:80
           (ext a () (ext b () (text "inner"))));
      true)

let () =
  t ~name:"controlled renderers and higher-order printer compatibility"
    (fun () ->
      let d = Term_color.color `Red (group (text "a" ^ nl ^ text "bbb")) in
      let disabled_ext = [ "termcolor" ] in
      let width = 3 in
      let expected =
        Pretty.to_string ~width (group (text "a" ^ nl ^ text "bbb"))
      in
      let buf = Buffer.create 32 in
      Pretty.to_out_with ~disabled_ext ~width (Out.of_buffer buf) d;
      assert_equal expected (Buffer.contents buf);
      Buffer.clear buf;
      Pretty.to_buffer_with ~disabled_ext ~width buf d;
      assert_equal expected (Buffer.contents buf);
      assert_equal expected
        (Format.asprintf "%a" (Pretty.to_format_with ~disabled_ext ~width) d);
      let pp_plain = pp_with ~disabled_ext in
      assert_equal "a bbb" (Format.asprintf "%a" pp_plain d);
      let legacy_out : width:int -> Out.t -> Containers_pp.t -> unit =
        Pretty.to_out
      in
      let legacy_buffer : width:int -> Buffer.t -> Containers_pp.t -> unit =
        Pretty.to_buffer
      in
      let legacy_string : width:int -> Containers_pp.t -> string =
        Pretty.to_string
      in
      let legacy_format :
          width:int -> Format.formatter -> Containers_pp.t -> unit =
        Pretty.to_format
      in
      let legacy_pp : Format.formatter -> Containers_pp.t -> unit = pp in
      assert_equal (legacy_string ~width d)
        (Pretty.to_string_with ~disabled_ext:[] ~width d);
      assert_equal
        (Format.asprintf "%a" legacy_pp d)
        (Format.asprintf "%a" (pp_with ~disabled_ext:[]) d);
      ignore (legacy_out, legacy_buffer, legacy_format);
      true)

(* Compare complete output without building large result strings. *)
let output_signature render d =
  let bytes = ref 0 in
  let lines = ref 0 in
  let checksum = ref 0 in
  let char c =
    incr bytes;
    if c = '\n' then incr lines;
    checksum := ((!checksum * 257) + Stdlib.Char.code c) mod 1_000_003
  in
  let out : Out.t =
    {
      char;
      string = Stdlib.String.iter char;
      sub_string =
        (fun s i len ->
          for j = i to i + len - 1 do
            char s.[j]
          done);
      newline = (fun () -> char '\n');
    }
  in
  render out d;
  !bytes, !lines, !checksum

let () =
  t ~name:"extension control preserves deep left-folded append output"
    (fun () ->
      let n = 300_000 in
      let width_calls = ref 0 in
      let callbacks = ref 0 in
      let style =
        Ext.make ~name:"deep"
          ~width:(fun () ->
            incr width_calls;
            0)
          ~pre:(fun out ~inside:_ () ->
            incr callbacks;
            out.string "\027[31m")
          ~post:(fun out ~inside:_ () ->
            incr callbacks;
            out.string "\027[0m")
          ()
      in
      let leaf = char 'x' in
      let styled_leaf = ext style () leaf in
      let repeat leaf =
        let d = ref nil in
        for _ = 1 to n do
          d := append !d leaf
        done;
        !d
      in
      let plain = repeat leaf in
      let styled = repeat styled_leaf in
      let original = output_signature (Pretty.to_out ~width:80) styled in
      let original_bytes, _, _ = original in
      assert_equal (10 * n) original_bytes;
      assert_equal (2 * n) !callbacks;
      callbacks := 0;
      assert_equal original
        (output_signature
           (Pretty.to_out_with ~disabled_ext:[ "unknown" ] ~width:80)
           styled);
      assert_equal (2 * n) !callbacks;
      callbacks := 0;
      assert_equal
        (output_signature (Pretty.to_out ~width:80) plain)
        (output_signature
           (Pretty.to_out_with ~disabled_ext:[ "deep" ] ~width:80)
           styled);
      assert_equal 0 !callbacks;
      assert_equal 1 !width_calls;
      true)

let () =
  t ~name:"extension control preserves long fill output and callbacks"
    (fun () ->
      let n = 300_000 in
      let width_calls = ref 0 in
      let callbacks = ref 0 in
      let style =
        Ext.make ~name:"long-fill"
          ~width:(fun () ->
            incr width_calls;
            0)
          ~pre:(fun out ~inside:_ () ->
            incr callbacks;
            out.string "\027[31m")
          ~post:(fun out ~inside:_ () ->
            incr callbacks;
            out.string "\027[0m")
          ()
      in
      let repeat leaf =
        let rec loop n acc =
          if n = 0 then
            acc
          else
            loop (n - 1) (leaf :: acc)
        in
        loop n []
      in
      let leaf = char 'x' in
      let sep = char ',' in
      let styled_leaf = ext style () leaf in
      let styled_sep = ext style () sep in
      let plain = fill sep (repeat leaf) in
      let styled = fill styled_sep (repeat styled_leaf) in
      let original = output_signature (Pretty.to_out ~width:80) styled in
      let original_bytes, _, _ = original in
      assert_equal (10 * ((2 * n) - 1)) original_bytes;
      assert_equal (2 * ((2 * n) - 1)) !callbacks;
      callbacks := 0;
      assert_equal original
        (output_signature
           (Pretty.to_out_with ~disabled_ext:[ "unknown" ] ~width:80)
           styled);
      assert_equal (2 * ((2 * n) - 1)) !callbacks;
      callbacks := 0;
      assert_equal
        (output_signature (Pretty.to_out ~width:80) plain)
        (output_signature
           (Pretty.to_out_with ~disabled_ext:[ "long-fill" ] ~width:80)
           styled);
      assert_equal 0 !callbacks;
      assert_equal 2 !width_calls;
      true)

let () =
  let w =
    Ext.make ~name:"weighted"
      ~width:(fun () -> 2)
      ~pre:(fun out ~inside:_ () -> out.string "[")
      ~post:(fun out ~inside:_ () -> out.string "]")
      ()
  in
  eq ~name:"flatten group with nonzero ext width" ~printer:(spf "%S") "[aaa] bb"
    (let d = group (ext w () (text "aaa") ^ nl ^ text "bb") in
     Pretty.to_string ~width:8 d)

let () =
  let n = ref 0 in
  let flaky =
    Ext.make ~name:"flaky"
      ~width:(fun () ->
        incr n;
        if !n = 1 then
          4
        else
          0)
      ~pre:(fun out ~inside:_ () -> out.string "<f>")
      ~post:(fun out ~inside:_ () -> out.string "</f>")
      ()
  in
  let inner =
    Ext.make ~name:"inner"
      ~width:(fun () -> 1)
      ~pre:(fun out ~inside:_ () -> out.string "<i>")
      ~post:(fun out ~inside:_ () -> out.string "</i>")
      ()
  in
  let d = group (ext flaky () (ext inner () (text "aa")) ^ nl ^ text "bbb") in
  (* the strip-render rebuilds the enabled wrap and re-evaluates its width,
     which drops from 4 to 0 *)
  eq ~name:"re-evaluated ext width is picked up" ~printer:(spf "%S")
    "<f>aa</f> bbb"
    (Pretty.to_string_with ~disabled_ext:[ "inner" ] ~width:80 d);
  assert_equal 2 !n
