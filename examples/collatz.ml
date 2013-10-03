
(** Display the graph of the collatz conjecture, starting from the given int *)

let g = LazyGraph.map
  ~edges:(fun () -> [])
  ~vertices:(fun i -> [`Label (string_of_int i)])
  LazyGraph.collatz_graph

let collatz n filename =
  Format.printf "print graph to %s@." filename;
  let out = open_out filename in
  let fmt = Format.formatter_of_out_channel out in
  LazyGraph.Dot.pp ~name:"collatz" g fmt (Sequence.singleton n);
  Format.pp_print_flush fmt ();
  close_out out

let _ =
  if Array.length Sys.argv < 3
    then (Format.printf "use: collatz num file@."; exit 0)
    else collatz (int_of_string Sys.argv.(1)) Sys.argv.(2)
