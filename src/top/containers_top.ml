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

let pp_rw_vector pp_x out (v: _ CCVector.vector) = CCVector.pp pp_x out v
let pp_ro_vector pp_x out (v: _ CCVector.ro_vector) = CCVector.pp pp_x out v
let pp_klist (ppx:Format.formatter -> 'a -> unit) out l = CCKList.pp ppx out l

let () =
  install_printers
    [ "CCHashtbl.pp"
    ; "Containers_top.pp_rw_vector"
    ; "Containers_top.pp_ro_vector"
    ; "CCBV.pp"
    ; "CCDeque.pp"
    ; "CCFQueue.pp"
    ; "CCIntMap.pp"
    ; "CCPersistentArray.pp"
    ; "CCBigstring.pp"
    ; "Containers_top.pp_klist"
    ; "CCKTree.pp"
    ; "CCSexpM.pp"
    ]
