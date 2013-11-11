
(** Some helpers for tests *)

let print_int_list l =
  let b = Buffer.create 20 in
  Format.bprintf b "@[<h>[%a]@]"
    (Sequence.pp_seq ~sep:", " Format.pp_print_int)
    (Sequence.of_list l);
  Buffer.contents b

let print_int_int_list l =
  let printer fmt (i,j) = Format.fprintf fmt "%d, %d" i j in
  let b = Buffer.create 20 in
  Format.bprintf b "@[<h>[%a]@]"
    (Sequence.pp_seq ~sep:", " printer)
    (Sequence.of_list l);
  Buffer.contents b
