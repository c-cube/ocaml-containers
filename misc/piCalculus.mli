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

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption) however
caused and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
*)

(** {1 Pi-calculus model of concurrency} *)

type 'a chan
  (** Channel conveying values of type 'a *)

type process = private
  | Parallel : process list -> process      (** Spawn several processes *)
  | Sum : transition list -> process        (** Choice point *)
  | Replicate : process -> process          (** Replication of a process *)
  | New : ('a chan -> process) -> process   (** New local name *)
  | Escape : (unit -> process) -> process   (** Run a user function *)
  | Stop : process                          (** Stop this process *)
and 'a __transition =
  | Receive : 'a chan * ('a -> process) -> 'a __transition
  | Send : 'a chan * 'a * process -> 'a __transition
and transition =
  | Transition : 'a __transition -> transition

val parallel : process list -> process
val sum : transition list -> process
val replicate : process -> process
val new_ : ('a chan -> process) -> process
val escape : (unit -> process) -> process
val stop : process

val send : 'a chan -> 'a -> process -> transition
val receive : 'a chan -> ('a -> process) -> transition

(** Be careful: there must be at least one send/receive between a replicate
   and a stop, otherwise {! run} will get stuck in a loop, replicating the
   process forever. *)

val send_one : 'a chan -> 'a -> process -> process
  (** Send a value, with no alternative *)

val receive_one : 'a chan -> ('a -> process) -> process
  (** Receive a value, with no alternative *)  

val (>>) : (unit -> unit) -> process -> process
  (** Perform the action, then proceed to the following process *)

val (|||) : process -> process -> process
  (** Infix version of {! parallel} for two processes *)

val (++) : transition -> transition -> process
  (** Infix version of {! sum} for two processes *)

val run : process -> unit
  (** Run the simulation until all processes are stuck, or stopped. *)
