
(*
copyright (c) 2014, simon cruanes
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

(** {1 Hierarchic logging} *)

type t

val to_file : string -> t
(** Create a logger that outputs to the given file *)

val to_chan : ?cleanup:bool -> out_channel -> t
(** Obtain a logger that outputs to the given channel.
    @param cleanup if true, will close the channel on exit;
    if false or not explicited, won't do anything. *)

(** {2 Raw functions} *)

val step : t -> string -> unit

val close : t -> unit
(** Close the logger. It will be unusable afterwards. *)

(** {2 Hierarchy} *)

val enter : t -> unit
(** Enter a new subsection *)

val exit : t -> unit
(** Exit the current subsection *)

val within : log:t -> (unit -> 'a) -> 'a
(** Enter a new subsection, evaluate the given function,
    exit the subsection and return the function's result.
    Also protects against exceptions. *)

(** {2 Buffer-formatting output}
The following functions use a {!Buffer.t} to create the message,
then send it to their logger. *)

module B : sig
  val enter : log:t -> ('a, Buffer.t, unit, unit) format4 -> 'a
  (** Enter a new (sub-)section with the given message *)

  val exit : log:t -> ('a, Buffer.t, unit, unit) format4 -> 'a
  (** Exit (close) the current sub-section. *)

  val step : log:t -> ('a, Buffer.t, unit, unit) format4 -> 'a
  (** Unit step within the current section *)
end

