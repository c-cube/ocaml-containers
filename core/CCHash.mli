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

(** {1 Hash combinators}

Combination of hashes based on the
SDBM simple hash (see for instance
{{:http://www.cse.yorku.ca/~oz/hash.html} this page})
*)

type t = int

type 'a hash_fun = 'a -> t

val combine : t -> t -> t
  (** Combine two hashes. Non-commutative. *)

val (<<>>) : t -> t -> t
  (** Infix version of {!combine} *)

val hash_int : int -> t
val hash_int2 : int -> int -> t
val hash_int3 : int -> int -> int -> t
val hash_int4 : int -> int -> int -> int -> t

val hash_string : string -> t

val hash_list : 'a hash_fun -> t -> 'a list hash_fun
(** Hash a list. Each element is hashed using [f]. *)

val hash_array : 'a hash_fun -> t -> 'a array hash_fun

val hash_pair : 'a hash_fun -> 'b hash_fun -> ('a * 'b) hash_fun
val hash_triple : 'a hash_fun -> 'b hash_fun -> 'c hash_fun -> ('a * 'b * 'c) hash_fun

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = [`Nil | `Cons of 'a * (unit -> 'a klist)]

val hash_seq : 'a hash_fun -> t -> 'a sequence hash_fun
val hash_gen : 'a hash_fun -> t -> 'a gen hash_fun
val hash_klist : 'a hash_fun -> t -> 'a klist hash_fun
