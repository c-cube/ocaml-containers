
let read_input_char file =
  CCIO.with_in file
    (fun ic ->
      let count = ref 0 in
      try
        while true do
          let _ = input_char ic in
          incr count
        done;
        assert false
      with End_of_file -> !count
    )

let read_input file =
  CCIO.with_in file
    (fun ic ->
      let count = ref 0 in
      let n = 4096 in
      let b = Bytes.make n ' ' in
      try
        while true do
          let n' = input ic b 0 n in
          if n'=0 then raise Exit;
          count := !count + n'
        done;
        assert false
      with Exit ->
        !count
    )

let read_read file =
  let fd = Unix.openfile file [Unix.O_RDONLY] 0o644 in
  let count = ref 0 in
  let n = 4096 in
  let b = Bytes.make n ' ' in
  try
    while true do
      let n' = Unix.read fd b 0 n in
      if n'=0 then raise Exit;
      count := !count + n'
    done;
    assert false
  with Exit ->
    Unix.close fd;
    !count

let read_lwt file =
  let open Lwt.Infix in
  Lwt_io.with_file ~mode:Lwt_io.input file
    (fun ic ->
      let n = 4096 in
      let b = Bytes.make n ' ' in
      let rec read_chunk count =
        Lwt_io.read_into ic b 0 n >>= fun n' ->
        let count = count + n' in
        if n'>0 then read_chunk count else Lwt.return count
      in
      read_chunk 0
    )

let read_lwt' file = Lwt_main.run (read_lwt file)
 
let profile ~f file () = (f file)

let bench file =
  let n1 = read_input_char file in
  let n2 = read_input file in
  let n3 = read_read file in
  let n4 = read_lwt' file in
  Printf.printf "results: %d, %d, %d, %d\n" n1 n2 n3 n4;
  assert (n1=n2 && n2 = n3 && n3=n4);
  Benchmark.throughputN ~repeat:5 4
    [ "input_char", profile ~f:read_input_char file, ()
    ; "input", profile ~f:read_input file, ()
    ; "Unix.read", profile ~f:read_read file, ()
    ; "Lwt_io.read", profile ~f:read_lwt' file, ()
    ]

let () =
  if Array.length Sys.argv < 2 then invalid_arg "use: truc file";
  let file = Sys.argv.(1) in
  Printf.printf "read file %s\n" file;
  let res = bench file in
  Benchmark.tabulate res;
  ()


