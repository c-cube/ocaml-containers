
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extensions of Standard Map} *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

module type S = sig
  include Map.S

  val get : key -> 'a t -> 'a option
  (** Safe version of {!find} *)

  val get_or : key -> 'a t -> or_:'a -> 'a
  (** [get_or k m ~or_] returns the value associated to [k] if present,
      and returns [or_] otherwise (if [k] doesn't belong in [m])
      @since 0.16 *)

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** [update k f m] calls [f (Some v)] if [find k m = v],
      otherwise it calls [f None]. In any case, if the result is [None]
      [k] is removed from [m], and if the result is [Some v'] then
      [add k v' m] is returned. *)

  val merge_safe :
    f:(key -> [`Left of 'a | `Right of 'b | `Both of 'a * 'b] -> 'c option) ->
    'a t -> 'b t -> 'c t
  (** [merge_safe ~f a b] merges the maps [a] and [b] together.
      @since 0.17 *)

  val of_seq : (key * 'a) sequence -> 'a t

  val add_seq : 'a t -> (key * 'a) sequence -> 'a t
  (** @since 0.14 *)

  val to_seq : 'a t -> (key * 'a) sequence

  val of_list : (key * 'a) list -> 'a t

  val add_list : 'a t -> (key * 'a) list -> 'a t
  (** @since 0.14 *)

  val keys : _ t -> key sequence
  (** Iterate on keys only
      @since 0.15 *)

  val values : 'a t -> 'a sequence
  (** Iterate on values only
      @since 0.15 *)

  val to_list : 'a t -> (key * 'a) list

  val pp :
    ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
    key printer -> 'a printer -> 'a t printer

  val print :
    ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
    key formatter -> 'a formatter -> 'a t formatter
end

module Make(O : Map.OrderedType) = struct
  include Map.Make(O)

  let get k m =
    try Some (find k m)
    with Not_found -> None

  let get_or k m ~or_ =
    try find k m
    with Not_found -> or_

  let update k f m =
    let x =
      try f (Some (find k m))
      with Not_found -> f None
    in
    match x with
    | None -> remove k m
    | Some v' -> add k v' m

  let merge_safe ~f a b =
    merge
      (fun k v1 v2 -> match v1, v2 with
         | None, None -> assert false
         | Some v1, None -> f k (`Left v1)
         | None, Some v2 -> f k (`Right v2)
         | Some v1, Some v2 -> f k (`Both (v1,v2)))
      a b

  let add_seq m s =
    let m = ref m in
    s (fun (k,v) -> m := add k v !m);
    !m

  let of_seq s = add_seq empty s

  let to_seq m yield =
    iter (fun k v -> yield (k,v)) m

  let keys m yield =
    iter (fun k _ -> yield k) m

  let values m yield =
    iter (fun _ v -> yield v) m

  let add_list m l = List.fold_left (fun m (k,v) -> add k v m) m l

  let of_list l = add_list empty l

  let to_list m =
    fold (fun k v acc -> (k,v)::acc) m []

  let pp ?(start="{") ?(stop="}") ?(arrow="->") ?(sep=", ") pp_k pp_v buf m =
    let first = ref true in
    Buffer.add_string buf start;
    iter
      (fun k v ->
         if !first then first := false else Buffer.add_string buf sep;
         pp_k buf k;
         Buffer.add_string buf arrow;
         pp_v buf v)
      m;
    Buffer.add_string buf stop

  let print ?(start="[") ?(stop="]") ?(arrow="->") ?(sep=", ") pp_k pp_v fmt m =
    Format.pp_print_string fmt start;
    let first = ref true in
    iter
      (fun k v ->
         if !first then first := false
         else (
           Format.pp_print_string fmt sep;
           Format.pp_print_cut fmt ()
         );
         pp_k fmt k;
         Format.pp_print_string fmt arrow;
         pp_v fmt v)
      m;
    Format.pp_print_string fmt stop
end
