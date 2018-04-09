
(* This file is free software, part of containers. See file "license" for more details. *)

(* fallback function *)
external make_float : int -> float array = "caml_make_float_vect"

include CCArray

(*$inject
  module type A = module type of CCArray
  module type AL = module type of CCArrayLabels
*)

(*$R
  ignore (module CCArrayLabels : A)
*)

(*$R
  ignore (module CCArray : AL)
*)

