
(** Write 10_000 Bencode values on the given file *)

(* write n times the same value in the file *)
let write_values file n =
  let out = BencodeOnDisk.open_out file in
  Printf.printf "[%d] opened file\n" (Unix.getpid ());
  let v = Bencode.(L [I 0; I 1; S "foo"]) in
  for i = 0 to n-1 do
    Printf.printf "[%d] iteration %d\n" (Unix.getpid ()) i;
    flush stdout;
    BencodeOnDisk.write out v;
  done;
  BencodeOnDisk.close_out out;
  Printf.printf "done\n";
  ()

let _ =
  let file = Sys.argv.(1) in
  Printf.printf "[%d] start: write to %s\n" (Unix.getpid ()) file;
  flush stdout;
  write_values file 100
