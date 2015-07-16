
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

(** {1 interface lwt-automaton} *)

open Containers_misc

module I : sig
  val send : 'a Lwt.t -> 'a Automaton.I.t -> unit
  (** Feed the content of the Lwt value into the automaton input, as soon as
      available *)

  val iter_stream : 'a Lwt_stream.t -> 'a Automaton.I.t -> unit Lwt.t
  (** Iterate on the given stream, sending its elements to the automaton *)
end

module O : sig
  val next : 'a Automaton.O.t -> 'a Lwt.t
  (** Wait for the next output *)
end

val next_transition :
  ('s,'i,'o) Automaton.Instance.t ->
  ('s * 'i * 's * 'o list) Lwt.t

(** {2 Interface with Unix} *)
module Unix : sig
  val read_write : Lwt_unix.file_descr ->
    ( [  `Active | `Stopped | `Error of exn ]
    , [ `Stop | `Write of Bytes.t | `JustRead of string | `Failwith of exn ]
    , [> `Read of string | `Closed | `Error of exn ]
    ) Automaton.Instance.t
  (** Read and write on the given filedescriptor *)

  val timeout : float -> [`Timeout] Automaton.O.t
  (** Wait the given amount of time, then trigger [`Timeout] *)
end
