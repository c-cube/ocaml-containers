#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;
#load "containers.cma";;
open Containers;;

module T = RatTerm.Default;; 
#install_printer T.fmt;;
#install_printer T.Subst.fmt;;

let t = T.(app "f" [const "a"; app "f" [mk_ref 1; const "b"]]);;
let t2 = T.(app "f" [var (); app "f" [mk_ref 1; var ()]]);;
let t3 = T.(app "f" [var (); app "f" [var (); const "b"]]);;
let subst2 = match T.unify t t3 with Some s -> s | None -> assert false;;
let t3' = T.Subst.apply subst2 t3;;
T.eq_set t t3';;

ok();;
