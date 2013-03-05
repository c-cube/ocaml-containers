
(** Some helpers for tests *)

let print_int_list l =
  let b = Buffer.create 20 in
  Format.bprintf b "@[<h>[%a]@]"
    (Sequence.pp_seq ~sep:", " Format.pp_print_int)
    (Sequence.of_list l);
  Buffer.contents b
