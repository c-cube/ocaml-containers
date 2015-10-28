
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

(** {1 Wrapper around Set} *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

module type S = sig
  include Set.S

  val of_seq : elt sequence -> t

  val add_seq : t -> elt sequence -> t
  (** @since NEXT_RELEASE *)

  val to_seq : t -> elt sequence

  val of_list : elt list -> t

  val add_list : t -> elt list -> t
  (** @since NEXT_RELEASE *)

  val to_list : t -> elt list

  val pp : ?start:string -> ?stop:string -> ?sep:string ->
            elt printer -> t printer

  val print : ?start:string -> ?stop:string -> ?sep:string ->
              elt formatter -> t formatter
end

module Make(O : Map.OrderedType) = struct
  include Set.Make(O)

  let add_seq set seq =
    let set = ref set in
    seq (fun x -> set := add x !set);
    !set

  let of_seq s = add_seq empty s

  let to_seq s yield = iter yield s

  let add_list = List.fold_left (fun set x -> add x set)

  let of_list l = add_list empty l

  let to_list = elements

  let pp ?(start="{") ?(stop="}") ?(sep=", ") pp_x buf m =
    let first = ref true in
    Buffer.add_string buf start;
    iter
      (fun x ->
        if !first then first := false else Buffer.add_string buf sep;
        pp_x buf x;
      ) m;
    Buffer.add_string buf stop

  let print ?(start="[") ?(stop="]") ?(sep=", ") pp_x fmt m =
    Format.pp_print_string fmt start;
    let first = ref true in
    iter
      (fun x ->
        if !first then first := false else (
          Format.pp_print_string fmt sep;
          Format.pp_print_cut fmt ()
        );
        pp_x fmt x;
      ) m;
    Format.pp_print_string fmt stop
end

