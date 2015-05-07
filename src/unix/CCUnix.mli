
(*
copyright (c) 2013-2015, simon cruanes
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

(** {1 High-level Functions on top of Unix}

Some useful functions built on top of Unix.

{b status: unstable}
@since 0.10 *)

type 'a or_error = [`Ok of 'a | `Error of string]
type 'a gen = unit -> 'a option

(** {2 Calling Commands} *)

val escape_str : Buffer.t -> string -> unit
(** Escape a string so it can be a shell argument.
*)

(*$T
  CCPrint.sprintf "%a" escape_str "foo" = "foo"
  CCPrint.sprintf "%a" escape_str "foo bar" = "'foo bar'"
  CCPrint.sprintf "%a" escape_str "fo'o b'ar" = "'fo''o b''ar'"
*)

type call_result =
  < stdout:string;
    stderr:string;
    status:Unix.process_status;
    errcode:int; (** extracted from status *)
  >

val call : ?bufsize:int ->
           ?stdin:[`Gen of string gen | `Str of string] ->
           ?env:string array ->
           ('a, Buffer.t, unit, call_result) format4 ->
           'a
(** [call cmd] wraps the result of [Unix.open_process_full cmd] into an
    object. It reads the full stdout and stderr of the subprocess before
    returning.
    @param stdin if provided, the generator or string is consumed and fed to
      the subprocess input channel, which is then closed.
    @param bufsize buffer size used to read stdout and stderr
    @param env environment to run the command in
*)

(*$T
  (call ~stdin:(`Str "abc") "cat")#stdout = "abc"
  (call "echo %a" escape_str "a'b'c")#stdout = "abc\n"
  (call "echo %s" "a'b'c")#stdout = "abc\n"
*)

type line = string

type async_call_result =
  < stdout:line gen;
    stderr:line gen;
    stdin:line -> unit; (* send a line *)
    close_in:unit; (* close stdin *)
    close_err:unit;
    close_out:unit;
    close_all:unit;  (* close all 3 channels *) (** @since NEXT_RELEASE *)
    wait:Unix.process_status;  (* block until the process ends *)
    wait_errcode:int; (* block until the process ends, then extract errcode *)
       (** @since NEXT_RELEASE *)
  >
(** A subprocess for interactive usage (read/write channels line by line)
    @since NEXT_RELEASE *)

val async_call : ?env:string array ->
                 ('a, Buffer.t, unit, async_call_result) format4 ->
                 'a
(** Spawns a subprocess, like {!call}, but the subprocess's channels are
    line generators and line sinks (for stdin).
    if [p] is [async_call "cmd"], then [p#wait] waits for the subprocess
    to die. Channels can be closed independently.
    @since NEXT_RELEASE *)

(** {2 Accessors}

@since NEXT_RELEASE *)

val stdout : < stdout : 'a; .. > -> 'a
val stderr : < stderr : 'a; .. > -> 'a
val status : < status : 'a; .. > -> 'a
val errcode : < errcode : 'a; .. > -> 'a

(** {2 Infix Functions} *)

module Infix : sig
  val (?|) : ('a, Buffer.t, unit, call_result) format4 -> 'a
  (** Infix version of {!call}
      @since NEXT_RELEASE *)

  val (?|&) : ('a, Buffer.t, unit, async_call_result) format4 -> 'a
  (** Infix version of {!async_call}
      @since NEXT_RELEASE *)
end

include module type of Infix


