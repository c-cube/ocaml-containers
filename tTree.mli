
(*
copyright (c) 2013, simon cruanes
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

(** {1 T-Trees}

Shallow, cache-friendly associative data structure.
See {{:http://en.wikipedia.org/wiki/T-tree} wikipedia}.

Not thread-safe.
*)

(** {2 signature} *)

module type S = sig
  type key

  type 'a t

  val empty : 'a t
    (** Empty tree *)

  val add : 'a t -> key -> 'a -> 'a t
    (** Add a binding key/value. If the key already was bound to some
        value, the old binding is erased. *)

  val remove : 'a t -> key -> 'a t
    (** Remove the key *)

  val find : 'a t -> key -> 'a
    (** Find the element associated with this key.
        @raise Not_found if the key is not present *)

  val length : 'a t -> int
    (** Number of bindings *)

  val fold : 'a t -> 'b -> ('b -> key -> 'a -> 'b) -> 'b
    (** Fold on bindings *)
end

(** {2 Functor that builds T trees for comparable keys} *)

module Make(X : Set.OrderedType) : S with type key = X.t
