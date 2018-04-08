
(* This file is free software, part of containers. See file "license" for more details. *)

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

