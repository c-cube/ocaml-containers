
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

(** {1 Extending CCIO with unix bindings}

A few utils to make {!Unix} more convenient to use.

@since NEXT_RELEASE *)

(** {2 Sub-process} *)
module Proc : sig
  type t
  (** A sub-process *)

  val pid : t -> int
  (** PID of the process *)

  val kill : ?signal:int -> t -> unit
  (** Kill the process with signal (default [15]) *)

  val wait : t -> Unix.process_status
  (** Wait for the process to stop, and return its status *)

  val stdin : t -> out_channel
  (** Input stream of the process *)

  val stderr : t -> in_channel
  (** Error stream of the process *)

  val stdout : t -> in_channel
  (** Output of the process *)

  (** {6 Running a sub-process} *)

  type cmd = string * string array
  (** A command used to start a process *)

  type redirect = [ `Redirect of Unix.file_descr | `Pipe ]
  (** Redirecting a file descriptor: either to an existing one, or create
      a pipe to communicate with the sub-process *)

  val sh : string -> cmd
  (** Use the shell to run a command, even with pipes and other redirections *)

  val with_proc : ?stdin:redirect ->
                  ?stdout:redirect ->
                  ?stderr:redirect ->
                  ?env:string array ->
                  cmd -> (t -> 'a) -> 'a
  (** [with_proc cmd f] starts a subprocess and gives it to [f]. When [f]
      returns, we wait for the subprocess to end. *)

end

