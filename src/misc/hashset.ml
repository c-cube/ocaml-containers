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

(** {1 Mutable polymorphic hash-set} *)

type 'a sequence = ('a -> unit) -> unit

type 'a t = ('a, unit) PHashtbl.t
  (** A set is a hashtable, with trivial values *)

let empty ?max_load ?eq ?hash size =
  PHashtbl.create ?max_load ?eq ?hash size

let copy set = PHashtbl.copy set

let clear set = PHashtbl.clear set

let cardinal set = PHashtbl.length set

let mem set x = PHashtbl.mem set x

let add set x = PHashtbl.add set x ()

let remove set x = PHashtbl.remove set x

let iter f set = PHashtbl.iter (fun x () -> f x) set

let fold f acc set = PHashtbl.fold (fun acc x () -> f acc x) acc set

let filter p set = PHashtbl.filter (fun x () -> p x) set

let to_seq set k = iter k set

let of_seq set seq =
  seq (fun x -> add set x)

let union ?into (s1 : 'a t) (s2 : 'a t) =
  let into = match into with
  | Some s -> of_seq s (to_seq s1); s
  | None -> copy s1 in
  of_seq into (to_seq s2);
  into

let seq_filter p seq k =
  seq (fun x -> if p x then k x)

let inter ?into (s1 : 'a t) (s2 : 'a t) =
  let into = match into with
  | Some s -> s
  | None -> empty ~eq:s1.PHashtbl.eq ~hash:s1.PHashtbl.hash (cardinal s1) in
  (* add to [into] elements of [s1] that also belong to [s2] *)
  of_seq into (seq_filter (fun x -> mem s2 x) (to_seq s1));
  into
