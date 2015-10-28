
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Extensions of Standard Map} *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

module type S = sig
  include Map.S

  val get : key -> 'a t -> 'a option
  (** Safe version of {!find} *)

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** [update k f m] calls [f (Some v)] if [find k m = v],
      otherwise it calls [f None]. In any case, if the result is [None]
      [k] is removed from [m], and if the result is [Some v'] then
      [add k v' m] is returned. *)

  val of_seq : (key * 'a) sequence -> 'a t

  val add_seq : 'a t -> (key * 'a) sequence -> 'a t
  (** @since NEXT_RELEASE *)

  val to_seq : 'a t -> (key * 'a) sequence

  val of_list : (key * 'a) list -> 'a t

  val add_list : 'a t -> (key * 'a) list -> 'a t
  (** @since NEXT_RELEASE *)

  val to_list : 'a t -> (key * 'a) list

  val pp : ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
            key printer -> 'a printer -> 'a t printer

  val print : ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
              key formatter -> 'a formatter -> 'a t formatter
end

module Make(O : Map.OrderedType) = struct
  include Map.Make(O)

  let get k m =
    try Some (find k m)
    with Not_found -> None

  let update k f m =
    let x =
      try f (Some (find k m))
      with Not_found -> f None
    in
    match x with
      | None -> remove k m
      | Some v' -> add k v' m

  let add_seq m s =
    let m = ref m in
    s (fun (k,v) -> m := add k v !m);
    !m

  let of_seq s = add_seq empty s

  let to_seq m yield =
    iter (fun k v -> yield (k,v)) m

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
        pp_v buf v
      ) m;
    Buffer.add_string buf stop

  let print ?(start="[") ?(stop="]") ?(arrow="->") ?(sep=", ") pp_k pp_v fmt m =
    Format.pp_print_string fmt start;
    let first = ref true in
    iter
      (fun k v ->
        if !first then first := false else (
          Format.pp_print_string fmt sep;
          Format.pp_print_cut fmt ()
        );
        pp_k fmt k;
        Format.pp_print_string fmt arrow;
        pp_v fmt v;
      ) m;
    Format.pp_print_string fmt stop
end

