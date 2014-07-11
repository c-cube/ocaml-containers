
(** Some helpers for tests *)

let print_int_list l =
  let b = Buffer.create 20 in
  CCList.pp CCInt.pp b l;
  Buffer.contents b

let print_int_int_list l =
  let b = Buffer.create 20 in
  CCList.pp (CCPair.pp CCInt.pp CCInt.pp) b l;
  Buffer.contents b
