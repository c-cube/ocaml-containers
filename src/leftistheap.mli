(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Leftist heaps *)

(* -----------------------------------------------------------------------------
  functor interface
 ----------------------------------------------------------------------------- *)

module type Ordered = sig
  type t
  val le: t -> t -> bool
end

exception Empty

module Make(X: Ordered) :
sig
  type t

  val empty: t

  val is_empty: t -> bool
    (* runs in O(1) *)

  val insert: X.t -> t -> t
    (* runs in O(log n) *)

  val min: t -> X.t
    (* runs in O(1) *)

  val extract_min: t -> X.t * t
    (* runs in O(log n) *)

  val merge: t -> t -> t
    (* runs in O(log max(n1, n2)) *)

  val filter: t -> (X.t -> bool) -> t
    (* O(n ln(N))? keep only the elements that satisfy the predicate *)

  val remove: t -> X.t list -> t
    (* runs in O(n), removing all elements in the list (assuming X.le is total) *)
end
