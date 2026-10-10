(* guess how many cores we can use *)
let guess_cpu_count () =
  let default = 4 in
  try
    let cmd = "grep -c processor /proc/cpuinfo" in
    let p = Unix.open_process_in cmd in
    try
      let x = input_line p |> int_of_string in
      ignore (Unix.close_process_in p);
      x
    with _ ->
      ignore (Unix.close_process_in p);
      default
  with _ -> default
