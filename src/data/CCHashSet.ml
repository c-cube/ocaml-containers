(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Mutable Set} *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

module type S = sig
  type t
  type elt

  val create : int -> t
  (** [create n] makes a new set with the given capacity [n] *)

  val singleton : elt -> t
  (** [singleton x] is the singleton [{x}] *)

  val clear : t -> unit
  (** [clear s] removes all elements from [s] *)

  val copy : t -> t
  (** Fresh copy *)

  val copy_into : into:t -> t -> unit
  (** [copy_into ~into s] copies all elements of [s] into [into] *)

  val insert : t -> elt -> unit
  (** [insert s x] adds [x] into [s] *)

  val remove : t -> elt -> unit
  (** Remove the element, if it were in there *)

  val cardinal : t -> int
  (** [cardinal s] returns the number of elements in [s] *)

  val mem : t -> elt -> bool
  (** [mem s x] returns [true] iff [x] is in [s] *)

  val find_exn : t -> elt -> elt
  (** [find_exn s x] returns [y] if [x] and [y] are equal, and [mem s y].
      @raise Not_found if [x] not in [s] *)

  val find : t -> elt -> elt option
  (** Safe version of {!find_exn} *)

  val inter : t -> t -> t
  (** [inter a b] returns [a ∩ b] *)

  val inter_mut : into:t -> t -> unit
  (** [inter_mut ~into a] changes [into] into [a ∩ into] *)

  val union : t -> t -> t
  (** [union a b] returns [a ∪ b] *)

  val union_mut : into:t -> t -> unit
  (** [union_mut ~into a] changes [into] into [a ∪ into] *)

  val diff : t -> t -> t
  (** [diff a b] returns [a - b] *)

  val subset : t -> t -> bool
  (** [subset a b] returns [true] if all elements of [a] are in [b] *)

  val equal : t -> t -> bool
  (** [equal a b] is extensional equality ([a] and [b] have the same elements) *)

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val iter : (elt -> unit) -> t -> unit
  (** Iterate on values *)

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** Fold on values *)

  val elements : t -> elt list
  (** List of elements *)

  val of_list : elt list -> t

  val to_seq : t -> elt sequence

  val of_seq : elt sequence -> t

  val add_seq : t -> elt sequence -> unit

  val pp : ?sep:string -> elt printer -> t printer
  (** [pp pp_elt] returns a set printer, given a printer for
      individual elements *)
end

module type ELEMENT = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int (** Positive value *)
end

module Make(E : ELEMENT) : S with type elt = E.t = struct
  module Tbl = Hashtbl.Make(E)

  type elt = E.t

  type t = elt Tbl.t  (* map [x -> x], for find *)

  let create = Tbl.create

  let singleton x =
    let s = create 8 in
    Tbl.replace s x x;
    s

  let clear = Tbl.clear

  let copy = Tbl.copy

  let copy_into ~into s =
    Tbl.iter (fun x _ -> Tbl.replace into x x) s

  let insert s x = Tbl.replace s x x

  let remove = Tbl.remove

  let cardinal = Tbl.length

  (*$T
    let module IS = Make(CCInt) in \
      IS.cardinal (IS.create 10) = 0
  *)

  let mem = Tbl.mem

  let find_exn = Tbl.find

  let find s x =
    try Some (Tbl.find s x)
    with Not_found -> None

  (*$T
    let module IS = Make(CCInt) in IS.find (IS.of_list [1;2;3]) 3 = Some 3
    let module IS = Make(CCInt) in IS.find (IS.of_list [1;2;3]) 5 = None
  *)

  let iter f s = Tbl.iter (fun x _ -> f x) s

  let fold f acc s = Tbl.fold (fun x _ acc -> f acc x) s acc

  let inter a b =
    let res = create (min (cardinal a) (cardinal b)) in
    iter (fun x -> if mem a x then insert res x) b;
    res

  (*$T
    let module IS = Make(CCInt) in \
      IS.(equal (inter (of_list [1;2;3]) (of_list [2;5;4])) (of_list [2]))
  *)

  let inter_mut ~into a =
    iter
      (fun x ->
        if not (mem a x) then remove into x
      ) into

  let union a b =
    let res = copy a in
    copy_into ~into:res b;
    res

  (*$T
    let module IS = Make(CCInt) in \
      IS.(equal (union (of_list [1;2;3]) (of_list [2;5;4])) (of_list [1;2;3;4;5]))
  *)

  let union_mut ~into a =
    copy_into ~into a

  let diff a b =
    let res = copy a in
    iter
      (fun x -> remove res x) b;
    res

  (*$T
    let module IS = Make(CCInt) in \
      IS.(equal (diff (of_list [1;2;3]) (of_list [2;4;5])) (of_list [1;3]))
  *)

  exception FastExit

  let for_all p s =
    try
      Tbl.iter (fun x _ -> if not (p x) then raise FastExit) s;
      true
    with FastExit -> false

  let exists p s =
    try
      Tbl.iter (fun x _ -> if p x then raise FastExit) s;
      false
    with FastExit -> true

  let subset a b =
    for_all (fun x -> mem b x) a

  let equal a b = subset a b && subset b a

  let elements s =
    Tbl.fold (fun x _ acc -> x::acc) s []

  let of_list l =
    let res = create (List.length l) in
    List.iter (insert res) l;
    res

  let to_seq s yield = iter yield s

  let add_seq s seq = seq (insert s)

  let of_seq seq =
    let s = create 32 in
    seq (insert s);
    s

  let pp ?(sep=",") pp_x out s =
    Format.pp_print_string out "{";
    let first = ref true in
    Tbl.iter
      (fun x _ ->
         if !first
         then first := false
         else (
          Format.pp_print_string out sep;
          Format.pp_print_cut out ();
         );
         pp_x out x
      ) s;
    Format.pp_print_string out "}"
end
