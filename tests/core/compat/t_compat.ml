
(* test consistency of interfaces *)
module type L = module type of CCEqual
module type LL = module type of CCEqualLabels ;;

ignore (module CCEqualLabels : L);;

ignore (module CCEqual : LL);;

