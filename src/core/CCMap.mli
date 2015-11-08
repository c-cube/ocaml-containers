
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

(** {1 Extensions of Standard Map}

Provide useful functions and iterators on [Map.S]
@since 0.5 *)

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
  (** @since 0.14 *)

  val to_seq : 'a t -> (key * 'a) sequence

  val of_list : (key * 'a) list -> 'a t

  val add_list : 'a t -> (key * 'a) list -> 'a t
  (** @since 0.14 *)

  val to_list : 'a t -> (key * 'a) list

  val pp : ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
            key printer -> 'a printer -> 'a t printer

  val print : ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
              key formatter -> 'a formatter -> 'a t formatter
end

module Make(O : Map.OrderedType) : S
  with type 'a t = 'a Map.Make(O).t
  and type key = O.t
