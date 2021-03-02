module CG = Containers_codegen
module Vec = CCVector

let spf = Printf.sprintf

let emit_bitfields () =
  let module B = CG.Bitfield in
  let ml = Vec.create() in
  let mli = Vec.create() in
  begin
    let b = B.make ~name:"t" () in
    B.field_bit b "x";
    B.field_bit b "y";
    B.field_bit b "z";
    B.field_int b ~width:5 "foo";

    Vec.push ml (CG.Code.in_struct "T1" [B.gen_ml b]);
    Vec.push mli (CG.Code.in_sig "T1" [B.gen_mli b]);
    (* check width *)
    Vec.push ml
      (CG.Code.mk_str (spf "let() = assert (%d = 8);;" (B.total_width b)));
    ()
  end;

  Vec.push ml @@ CG.Code.mk_str {|
    let n_fails = ref 0;;
    at_exit (fun () -> if !n_fails > 0 then exit 1);;
    let assert_true line s =
      if not s then ( incr n_fails; Printf.eprintf "test failure at %d\n%!" line);;

    |};

  let test1 = {|
    assert_true __LINE__ T1.(get_y (empty |> set_x true |> set_y true |> set_foo 10));;
    assert_true __LINE__ T1.(get_x (empty |> set_x true |> set_y true |> set_foo 10));;
    assert_true __LINE__ T1.(get_y (empty |> set_x true |> set_z true
                            |> set_y false |> set_x false |> set_y true));;
    assert_true __LINE__ T1.(get_z (empty |> set_z true));;
    assert_true __LINE__ T1.(not @@ get_x (empty |> set_z true));;
    assert_true __LINE__ T1.(not @@ get_y (empty |> set_z true |> set_x true));;
    assert_true __LINE__ T1.(not @@ get_y (empty |> set_z true |> set_foo 18));;
    (* check width of foo *)
    assert_true __LINE__ T1.(try ignore (empty |> set_foo (1 lsl 6)); false with _ -> true);;
    assert_true __LINE__ T1.(12 = get_foo (empty |> set_x true |> set_foo 12 |> set_x false));;
    assert_true __LINE__ T1.(24 = get_foo (empty |> set_y true |> set_foo 24 |> set_z true));;
  |} |> CG.Code.mk_str in
  Vec.push ml test1;

  CG.emit_file "test_bitfield.ml" (Vec.to_list ml);
  CG.emit_file "test_bitfield.mli" (Vec.to_list mli);
  ()

let () =
  emit_bitfields();
  ()

