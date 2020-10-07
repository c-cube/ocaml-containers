module type RANKED = CCMutHeap_intf.RANKED

module type S = CCMutHeap_intf.S

module Make(X : RANKED) : S with type elt = X.t
