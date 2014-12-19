
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

(** {1 B-Trees}

Shallow, cache-friendly associative data structure.
See {{: https://en.wikipedia.org/wiki/B-tree} wikipedia}.

Not thread-safe.  *)

type 'a sequence = ('a -> unit) -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 signature} *)

module type S = sig
  type key
  type 'a t

  val create : unit -> 'a t
  (** Empty map *)

  val size : _ t -> int
  (** Number of bindings *)

  val add : key -> 'a -> 'a t -> unit
  (** Add a binding to the tree. Erases the old binding, if any *)

  val remove : key -> 'a t -> unit
  (** Remove the given key, or does nothing if the key isn't present *)

  val get : key -> 'a t -> 'a option
  (** Key lookup *)

  val get_exn : key -> 'a t -> 'a
  (** Unsafe version of {!get}.
      @raise Not_found if the key is not present *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on bindings *)

  val of_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list
  val to_tree : 'a t -> (key * 'a) list ktree
end

(** {2 Functor that builds trees for comparable keys} *)

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module Make(X : ORDERED) : S with type key = X.t

(* note: to print a B-tree in dot:
{[
let t = some_btree in
let t' = CCKTree.map
  (fun t ->
    [`Shape "square";
     `Label (CCPrint.to_string (CCList.pp (CCPair.pp CCInt.pp CCString.pp)) t)]
  ) (T.to_tree t);;
CCPrint.to_file "/tmp/some_file.dot" "%a\n" (CCKTree.Dot.pp_single "btree") t';
]}
*)

