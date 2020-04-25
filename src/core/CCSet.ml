
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Wrapper around Set} *)

type 'a iter = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

module type OrderedType = Set.OrderedType

(*$inject
  module S = CCSet.Make(struct
    type t = int
    let compare x y = Stdlib.compare x y
  end)
*)

module type S = sig
  include Set.S

  val min_elt_opt : t -> elt option
  (** Safe version of {!min_elt}.
      @since 1.5 *)

  val max_elt_opt : t -> elt option
  (** Safe version of {!max_elt}.
      @since 1.5 *)

  val choose_opt : t -> elt option
  (** Safe version of {!choose}.
      @since 1.5 *)

  val find_opt : elt -> t -> elt option
  (** Safe version of {!find}.
      @since 1.5 *)

  val find_first : (elt -> bool) -> t -> elt
  (** Find minimum element satisfying predicate.
      @since 1.5 *)

  val find_first_opt : (elt -> bool) -> t -> elt option
  (** Safe version of {!find_first}.
      @since 1.5 *)

  val find_last : (elt -> bool) -> t -> elt
  (** Find maximum element satisfying predicate.
      @since 1.5 *)

  val find_last_opt : (elt -> bool) -> t -> elt option
  (** Safe version of {!find_last}.
      @since 1.5 *)

  val of_iter : elt iter -> t
  (** Build a set from the given [iter] of elements.
      @since 2.8 *)

  val add_iter : t -> elt iter -> t
  (** @since 2.8 *)

  val to_iter : t -> elt iter
  (** [to_iter t] converts the set [t] to a [iter] of the elements.
      @since 2.8 *)

  val add_list : t -> elt list -> t
  (** @since 0.14 *)

  val to_list : t -> elt list
  (** [to_list t] converts the set [t] to a list of the elements. *)

  val to_string :
    ?start:string -> ?stop:string -> ?sep:string ->
    (elt -> string) -> t -> string
  (**  Print the set in a string
       @since 2.7 *)

  val pp :
    ?start:string -> ?stop:string -> ?sep:string ->
    elt printer -> t printer
    (** Print the set. *)
end

module Make(O : Map.OrderedType) = struct
  module S = Set.Make(O)

  (* backport functions from recent stdlib.
     they will be shadowed by inclusion of [S] if present. *)

  let find_opt x s =
    try Some (S.find x s)
    with Not_found -> None

  let choose_opt s =
    try Some (S.choose s)
    with Not_found -> None

  let min_elt_opt s =
    try Some (S.min_elt s)
    with Not_found -> None

  let max_elt_opt s =
    try Some (S.max_elt s)
    with Not_found -> None

  exception Find_binding_exit

  let find_first_opt f m =
    let res = ref None in
    try
      S.iter
        (fun x ->
           if f x then (
             res := Some x;
             raise Find_binding_exit
           ))
        m;
      None
    with Find_binding_exit ->
      !res

  let find_first f m = match find_first_opt f m with
    | None -> raise Not_found
    | Some x -> x

  (* linear time, must traverse the whole set… *)
  let find_last_opt f m =
    let res = ref None in
    S.iter
      (fun x -> if f x then res := Some x)
      m;
    !res

  let find_last f m = match find_last_opt f m with
    | None -> raise Not_found
    | Some x -> x

  include S

  let add_std_seq set seq =
    let set = ref set in
    Seq.iter (fun x -> set := add x !set) seq;
    !set

  let of_std_seq s = add_std_seq empty s

  let add_iter set i =
    let set = ref set in
    i (fun x -> set := add x !set);
    !set

  let of_iter s = add_iter empty s
  let to_iter s yield = iter yield s

  let add_list = List.fold_left (fun set x -> add x set)

  let to_list = elements

  let to_string ?(start="") ?(stop="") ?(sep=",") elt_to_string h =
    to_list h
    |> CCList.to_string ~start ~stop ~sep elt_to_string

  (*$= & ~printer:(fun s -> s)
    (S.to_string string_of_int (S.of_list [4; 3])) "3,4"
  *)
  (*$Q
    Q.(list int) (fun l -> \
      let s = S.of_list l in \
      (S.to_string string_of_int s) \
        = (CCList.sort_uniq ~cmp:CCInt.compare l \
           |> List.map string_of_int |> String.concat ","))
    Q.(list int) (fun l -> \
      let s = S.of_list l in \
      (S.to_string ~sep:" " string_of_int s) \
        = (CCList.sort_uniq ~cmp:CCInt.compare l \
           |> List.map string_of_int |> String.concat " "))
  *)

  let pp ?(start="") ?(stop="") ?(sep=", ") pp_x fmt m =
    Format.pp_print_string fmt start;
    let first = ref true in
    iter
      (fun x ->
         if !first then first := false
         else (
           Format.pp_print_string fmt sep;
           Format.pp_print_cut fmt ()
         );
         pp_x fmt x)
      m;
    Format.pp_print_string fmt stop
end
