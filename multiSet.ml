(*
copyright (c) 2013, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
  damages (including, but not limited to, procurement of substitute goods or
  services; loss of use, data, or profits; or business interruption) however
  caused and on any theory of liability, whether in contract, strict liability,
  or tort (including negligence or otherwise) arising in any way out of the use
  of this software, even if advised of the possibility of such damage.
*)

(** {1 Multiset} *)

module type S = sig
  type elt
  type t

  val empty : t

  val mem : t -> elt -> bool

  val count : t -> elt -> int

  val singleton : elt -> t

  val add : t -> elt -> t

  val remove : t -> elt -> t

  val min : t -> elt

  val max : t -> elt

  val union : t -> t -> t

  val intersection : t -> t -> t

  val diff : t -> t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val cardinal : t -> int
    (** Number of distinct elements *)

  val iter : t -> (int -> elt -> unit) -> unit

  val fold : t -> 'b -> ('b -> int -> elt -> 'b) -> 'b

  val of_list : elt list -> t

  val to_list : t -> elt list
end

module Make(O : Set.OrderedType) = struct
  module M = Map.Make(O)

  type t = int M.t

  type elt = O.t

  let empty = M.empty

  let mem ms x = M.mem x ms

  let count ms x =
    try M.find x ms
    with Not_found -> 0

  let singleton x = M.singleton x 1

  let add ms x =
    let n = count ms x in
    M.add x (n+1) ms

  let remove ms x =
    let n = count ms x in
    match n with
    | 0 -> ms
    | 1 -> M.remove x ms
    | _ -> M.add x (n-1) ms

  let min ms =
    fst (M.min_binding ms)

  let max ms =
    fst (M.max_binding ms)

  let union m1 m2 =
    M.merge
      (fun x n1 n2 -> match n1, n2 with
        | None, None -> assert false
        | Some n1, None -> Some n1
        | None, Some n2 -> Some n2
        | Some n1, Some n2 -> Some (n1+n2))
      m1 m2

  let intersection m1 m2 =
    M.merge
      (fun x n1 n2 -> match n1, n2 with
        | None, None -> assert false
        | Some _, None
        | None, Some _ -> None
        | Some n1, Some n2 -> Some (Pervasives.min n1 n2))
      m1 m2

  let diff m1 m2 =
    M.merge
      (fun x n1 n2 -> match n1, n2 with
        | None, None -> assert false
        | Some n1, None -> Some n1
        | None, Some n2 -> None
        | Some n1, Some n2 ->
          if n1 > n2 
            then Some (n1 - n2)
            else None)
      m1 m2

  let compare m1 m2 =
    M.compare (fun x y -> x - y) m1 m2

  let equal m1 m2 =
    M.equal (fun x y -> x = y) m1 m2

  let cardinal m = M.cardinal m

  let iter m f =
    M.iter (fun x n -> f n x) m

  let fold m acc f =
    M.fold (fun x n acc -> f acc n x) m acc

  let of_list l =
    let rec build acc l = match l with
    | [] -> acc
    | x::l' -> build (add acc x) l'
    in
    build empty l

  let to_list m =
    (* [n_cons n x l] is the result of applying [fun l -> x :: l]  [n] times
        to [l] *)
    let rec n_cons n x l = match n with
    | 0 -> l
    | 1 -> x::l
    | _ -> n_cons (n-1) x (x::l)
    in
    fold m [] (fun acc n x -> n_cons n x acc)
end
