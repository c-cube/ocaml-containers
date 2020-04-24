
(* This file is free software, part of containers. See file "license" for more details. *)

type 'a printer = Format.formatter -> 'a -> unit

let eval_exn str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase false Format.err_formatter phrase

let install_printer s =
  try
    ignore (eval_exn ("#install_printer " ^ s ^ " ;; "))
  with _ ->
    Printexc.print_backtrace stderr;
    ()
let install_printers = List.iter install_printer

let () =
  install_printers
    [ "CCBV.pp";
      "CCDeque.pp";
      "CCFQueue.pp";
      "CCFun_vec.pp";
      "CCIntMap.pp";
      "CCPersistentArray.pp";
    ]
