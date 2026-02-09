(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Wrapper around Set} *)

type 'a iter = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S

  val find_first_map : (elt -> 'a option) -> t -> 'a option
  (** [find_first_map f s] find the minimum element [x] of [s] such that [f x = Some y]
      and return [Some y]. Otherwise returns [None].
      @since 3.12 *)

  val find_last_map : (elt -> 'a option) -> t -> 'a option
  (** [find_last_map f s] find the maximum element [x] of [s] such that [f x = Some y]
      and return [Some y]. Otherwise returns [None].
      @since 3.12 *)

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
    ?start:string ->
    ?stop:string ->
    ?sep:string ->
    (elt -> string) ->
    t ->
    string
  (**  Print the set in a string
       @since 2.7 *)

  val pp :
    ?pp_start:unit printer ->
    ?pp_stop:unit printer ->
    ?pp_sep:unit printer ->
    elt printer ->
    t printer
  (** Print the set. *)
end

module Make (O : Map.OrderedType) = struct
  module S = Set.Make (O)

  (* backport functions from recent stdlib.
     they will be shadowed by inclusion of [S] if present. *)

  [@@@ocaml.warning "-32"]

  exception Find_binding_exit

  let find_first_map f m =
    let res = ref None in
    try
      S.iter
        (fun x ->
          match f x with
          | None -> ()
          | Some y ->
            res := Some y;
            raise Find_binding_exit)
        m;
      None
    with Find_binding_exit -> !res

  [@@@ocaml.warning "+32"]

  include S

  let find_last_map f m =
    let res = ref None in
    let _ =
      find_last_opt
        (fun x ->
          match f x with
          | None -> false
          | Some y ->
            res := Some y;
            true)
        m
    in
    !res

  let add_iter set i =
    let set = ref set in
    i (fun x -> set := add x !set);
    !set

  let of_iter s = add_iter empty s
  let to_iter s yield = iter yield s
  let add_list = List.fold_left (fun set x -> add x set)
  let to_list = elements

  let to_string ?(start = "") ?(stop = "") ?(sep = ",") elt_to_string h =
    to_list h |> CCList.to_string ~start ~stop ~sep elt_to_string

  let pp ?(pp_start = fun _ () -> ()) ?(pp_stop = fun _ () -> ())
      ?(pp_sep = fun fmt () -> Format.fprintf fmt ",@ ") pp_x fmt m =
    pp_start fmt ();
    let first = ref true in
    iter
      (fun x ->
        if !first then
          first := false
        else
          pp_sep fmt ();
        pp_x fmt x)
      m;
    pp_stop fmt ()
end
