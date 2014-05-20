
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

(** {1 Tuple Functions} *)

type ('a,'b) t = ('a * 'b)

val map1 : ('a -> 'b) -> ('a * 'c) -> ('b * 'c)

val map2 : ('a -> 'b) -> ('c * 'a) -> ('c * 'b)

val map : ('a -> 'c) -> ('b -> 'd) -> ('a * 'b) -> ('c * 'd)

val map_same : ('a -> 'b) -> ('a*'a) -> ('b*'b)

val swap : ('a * 'b) -> ('b * 'a)
(** Swap the components of the tuple *)

val (<<<) : ('a -> 'b) -> ('a * 'c) -> ('b * 'c)
(** Map on the left side of the tuple *)

val (>>>) : ('a -> 'b) -> ('c * 'a) -> ('c * 'b)
(** Map on the right side of the tuple *)

val ( *** ) : ('a -> 'c) -> ('b -> 'd) -> ('a * 'b) -> ('c * 'd)
(** Map on both sides of a tuple *)

val ( &&& ) : ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c)
(** [f &&& g] is [fun x -> f x, g x]. It splits the computations into
    two parts *)

val merge : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
(** Uncurrying (merges the two components of a tuple) *)

val equal : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a * 'b) -> ('a * 'b) -> bool

val compare : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a * 'b) -> ('a * 'b) -> int

type 'a printer = Buffer.t -> 'a -> unit

val pp : 'a printer -> 'b printer -> ('a*'b) printer
