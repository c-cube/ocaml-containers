
(* This file is free software, part of containers. See file "license" for more details. *)

include CCList

(*$inject
  module type L = module type of CCList
  module type LL = module type of CCListLabels
*)

(*$R
  ignore (module CCListLabels : L)
*)

(*$R
  ignore (module CCList : LL)
*)

