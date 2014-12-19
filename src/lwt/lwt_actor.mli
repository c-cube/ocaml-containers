
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

(** {1 Small Actor system for Lwt}

Let's draw inspiration from Erlang. Just a tiny bit. Currently
this module is unstable and experimental.

{b NOTE}: this module is not thread-safe at all.
*)

(** {2 Actors Basics} *)

type 'a t
(** An actor that can receive messages of type 'a. In practice, 'a will
    often be a variant or a polymorphic variant. *)

type any_actor =
  | AnyActor : _ t -> any_actor

val spawn : ?links:any_actor list ->
            ?setup:(unit -> unit Lwt.t) ->
            ('a t -> 'a -> unit Lwt.t) -> 'a t
(** Spawn a new actor with the given loop function. The function will
    be called repeatedly with [(self, message)] where [self] is the actor
    itself, and [msg] some incoming message..
    @param setup function that is called when the actor (re)starts
    @param links list of other actors that are linked to immediately *)

val send : 'a t -> 'a -> unit Lwt.t
(** Send a message to an actor's inbox *)

val pid : _ t -> int
(** Pid of an actor *)

val timeout : [> `Timeout of int ] t -> float -> int
(** [timeout a f] returns some unique integer ticket [i],
    and, [f] seconds later, sends [`Timeout i] to [a] *)

val link : _ t -> _ t -> unit
(** [link a b] links the two actors together, so that if one dies, the
    other dies too. The linking relationship is transitive and symmetric. *)

val kill : _ t -> unit
(** Kill the actor, and all its linked actors *)

val monitor : [> `Died of any_actor] t -> _ t -> unit
(** [monitor m a] adds [a] to the list of actors monitored by [m]. If [a]
    dies for any reason, [m] is sent [`Died a] and can react consequently. *)

val wait_all : unit -> unit Lwt.t
(** Wait for all actors to finish. Typically used directly in {!Lwt_main.run} *)

(* TODO: some basic patterns: monitor strategies, pub/sub... *)
