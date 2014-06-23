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

Combination of hashes based on the Murmur Hash (64 bits). See
{{:https://sites.google.com/site/murmurhash/MurmurHash2_64.cpp?attredirects=0} this page}
*)

(** {2 Definitions} *)

type t = private int64

type 'a hash_fun = 'a -> t -> t
(** Hash function for values of type ['a], merging a fingerprint of the
    value into the state of type [t] *)

val init : t
(** Initial value *)

val finish : t -> int
(** Extract a usable hash value *)

val apply : 'a hash_fun -> 'a -> int
(** Apply a hash function to a value *)

(** {2 Basic Combinators} *)

val bool_ : bool hash_fun
val char_ : char hash_fun
val int_ : int hash_fun
val string_ : string hash_fun
val int32_ : int32 hash_fun
val int64_ : int64 hash_fun
val nativeint_ : nativeint hash_fun

val list_ : 'a hash_fun -> 'a list hash_fun
(** Hash a list. Each element is hashed using [f]. *)

val array_ : 'a hash_fun -> 'a array hash_fun

val pair : 'a hash_fun -> 'b hash_fun -> ('a * 'b) hash_fun
val triple : 'a hash_fun -> 'b hash_fun -> 'c hash_fun -> ('a * 'b * 'c) hash_fun

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

val seq : 'a hash_fun -> 'a sequence hash_fun
val gen : 'a hash_fun -> 'a gen hash_fun
val klist : 'a hash_fun -> 'a klist hash_fun
