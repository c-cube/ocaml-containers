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

type t = { 
  mutable id : unit ref;
  mutable store : unit -> unit;
} (** The universal type *)

type 'a embedding = {
  pack : 'a -> t;           (** Pack a 'a into a univ value *)
  unpack : t -> 'a option;  (** Try to unpack the univ value into an 'a *)
  set : t -> 'a -> unit;    (** Change, in-place, the content of the univ value *)
  compatible : t -> bool;   (** Check whether the univ value can be unpacked *)
} (** Conversion between the universal type and 'a *)

(** Create a new embedding. Values packed by a given embedding can
    only be unpacked by the same embedding. *)
let embed () = 
  let id = ref () in  (* unique ID of the embedding *)
  let r = ref None in (* place to store values *)
  let pack a =        (* pack the 'a value into a new univ cell *)
    let o = Some a in
    { id = id; store = (fun () -> r := o); }
  in
  let unpack t =      (* try to extract the content of a univ cell *)
    r := None;
    t.store ();
    let a = !r in
    a
  in
  let set t a =       (* change, in place, the embedding and content of the cell *)
    t.id <- id;
    let o = Some a in
    t.store <- (fun () -> r := o)
  in
  let compatible t =  (* check whether the univ cell is from this embedding *)
    id == t.id
  in
  { pack; unpack; compatible; set; }

let pack emb x = emb.pack x

let unpack emb t = emb.unpack t

let compatible emb t = emb.compatible t

let set emb t x = emb.set t x
