module T = (val Containers_testlib.make ~__FILE__ ())
include T
open CCUnix;;

t @@ fun () -> escape_str "foo" = "foo";;
t @@ fun () -> escape_str "foo bar" = "'foo bar'";;
t @@ fun () -> escape_str "fo'o b'ar" = "'fo'\\''o b'\\''ar'";;
t @@ fun () -> call_full ~stdin:(`Str "abc") "cat" |> stdout = "abc";;
t @@ fun () -> call_full "echo %s" (escape_str "a'b'c") |> stdout = "a'b'c\n";;
t @@ fun () -> call_full "echo %s" "a'b'c" |> stdout = "abc\n";;
t @@ fun () -> call_stdout ~stdin:(`Str "abc") "cat" = "abc";;
t @@ fun () -> call_stdout "echo %s" (escape_str "a'b'c") = "a'b'c\n";;
t @@ fun () -> call_stdout "echo %s" "a'b'c" = "abc\n";;

t @@ fun () ->
let m = 200 in
let n = 50 in
let write_atom filename s =
  with_file_lock ~kind:`Write filename (fun () ->
      CCIO.with_out ~flags:[ Open_append; Open_creat ] filename (fun oc ->
          output_string oc s;
          flush oc))
in
let f filename =
  for _j = 1 to m do
    write_atom filename "foo\n"
  done
in
CCIO.File.with_temp ~prefix:"containers_" ~suffix:".txt" (fun filename ->
    let a = Array.init n (fun _ -> Thread.create f filename) in
    Array.iter Thread.join a;
    let lines = CCIO.with_in filename CCIO.read_lines_l in
    assert_equal ~printer:string_of_int (n * m) (List.length lines);
    assert (List.for_all (( = ) "foo") lines));
true
;;

t @@ fun () ->
let filename =
  with_temp_dir "test_containers" (fun dir ->
      let name = Filename.concat dir "test" in
      CCIO.with_out name (fun oc ->
          output_string oc "content";
          flush oc);
      assert (Sys.file_exists name);
      name)
in
assert (not (Sys.file_exists filename));
true
