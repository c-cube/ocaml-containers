(* This code is extracted from Msat ( https://github.com/Gbury/mSAT ).
   As such it is under the Apache 2 License.
*)

(** {1 Mutable Heaps}

    The classic binary heap in a vector.

    {b STATUS}: experimental, this might change in breaking ways.

    @since 3.1 *)

module type RANKED = CCMutHeap_intf.RANKED

module type S = CCMutHeap_intf.S

module Make(X : RANKED) : S with type elt = X.t
