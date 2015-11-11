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


(** {1 Utils around Mutex}

@since 0.8 *)

type 'a t
(** A value surrounded with a lock *)

val create : 'a -> 'a t
(** Create a new protected value *)

val with_lock : 'a t -> ('a -> 'b) -> 'b
(** [with_lock l f] runs [f x] where [x] is the value protected with
    the lock [l], in a critical section. If [f x] fails, [with_lock l f]
    fails too but the lock is released *)

(** Type allowing to manipulate the lock as a reference
    @since 0.13 *)
module LockRef : sig
  type 'a t

  val get : 'a t -> 'a

  val set : 'a t -> 'a -> unit

  val update : 'a t -> ('a -> 'a) -> unit
end

val with_lock_as_ref : 'a t -> f:('a LockRef.t -> 'b) -> 'b
(** [with_lock_as_ref l f] calls [f] with a reference-like object
    that allows to manipulate the value of [l] safely.
    The object passed to [f] must not escape the function call
    @since 0.13 *)

val update : 'a t -> ('a -> 'a) -> unit
(** [update l f] replaces the content [x] of [l] with [f x], atomically *)

val mutex : _ t -> Mutex.t
(** Underlying mutex *)

val get : 'a t -> 'a
(** Get the value in the lock. The value that is returned isn't protected! *)

val set : 'a t -> 'a -> unit
(** Atomically set the value
    @since 0.13 *)

val incr : int t -> unit
(** Atomically increment the value
    @since 0.13 *)

val decr : int t -> unit
(** Atomically decrement the value
    @since 0.13 *)
