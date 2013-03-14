(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
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

(** {1 Universal type} *)

(** This is largely inspired by https://ocaml.janestreet.com/?q=node/18 . *)

type t
  (** The universal type *)

type 'a embedding = {
  pack : 'a -> t;           (** Pack a 'a into a univ value *)
  unpack : t -> 'a option;  (** Try to unpack the univ value into an 'a *)
  set : t -> 'a -> unit;    (** Change, in-place, the content of the univ value *)
  compatible : t -> bool;   (** Check whether the univ value can be unpacked *)
} (** Conversion between the universal type and 'a *)

val embed : unit -> 'a embedding
  (** Create a new embedding. Values packed by a given embedding can
      only be unpacked by the same embedding. *)

val pack : 'a embedding -> 'a -> t

val unpack : 'a embedding -> t -> 'a option

val compatible : 'a embedding -> t -> bool

val set : 'a embedding -> t -> 'a -> unit
