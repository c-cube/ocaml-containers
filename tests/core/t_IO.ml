open CCIO
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () ->
let s = String.make 200 'y' in
let s = Printf.sprintf "a\nb\n %s\nlast line\n" s in
File.with_temp ~prefix:"test_containers" ~suffix:"" (fun name ->
    with_out name @@ fun oc ->
    output_string oc s;
    flush oc;
    let s' = with_in name read_all in
    assert_equal ~printer:(fun s -> s) s s');
true
;;

q
  Q.(list_of_size Gen.(0 -- 40) printable_string)
  (fun l ->
    let l' = ref [] in
    File.with_temp ~prefix:"test_containers" ~suffix:"" (fun name ->
        with_out name @@ fun oc ->
        write_lines_l oc l;
        flush oc;
        l' := with_in name read_lines_l);
    String.concat "\n" l = String.concat "\n" !l')
;;

q
  Q.(list_of_size Gen.(0 -- 40) printable_string)
  (fun l ->
    let l' = ref [] in
    File.with_temp ~prefix:"test_containers" ~suffix:"" (fun name ->
        with_out name @@ fun oc ->
        write_lines oc (Gen.of_list l);
        flush oc;
        l' := with_in name (fun ic -> read_lines_gen ic |> Gen.to_list));
    String.concat "\n" l = String.concat "\n" !l')
;;

q
  Q.(list_of_size Gen.(0 -- 40) printable_string)
  (fun l ->
    let s = ref "" in
    File.with_temp ~prefix:"test_containers1" ~suffix:"" (fun name1 ->
        with_out name1 @@ fun oc1 ->
        write_gen ~sep:"" oc1 (Gen.of_list l);
        flush oc1;
        File.with_temp ~prefix:"test_containers2" ~suffix:"" (fun name2 ->
            with_out name2 @@ fun oc2 ->
            CCIO.with_in name1 (fun ic1 -> copy_into ic1 oc2);
            flush oc2;
            s := with_in name2 read_all));
    String.concat "" l = !s)
;;

t @@ fun () ->
File.walk "."
|> Gen.for_all (function
     | `File, f -> not (Sys.is_directory f)
     | `Dir, f -> Sys.is_directory f)
