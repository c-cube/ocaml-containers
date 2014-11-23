
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

(** {1 Extending CCIO with unix bindings} *)

module Proc = struct
  type t = {
    pid : int;
    mutable status : Unix.process_status option;
    stdin : out_channel;
    stdout : in_channel;
    stderr : in_channel;
  }

  let pid t = t.pid

  let kill ?(signal=15) t =
    Unix.kill t.pid signal

  let wait t =
    match t.status with
    | Some s -> s
    | None ->
        let _, status = Unix.waitpid [] t.pid in
        t.status <- Some status;
        status

  let stdin t = t.stdin
  let stderr t = t.stderr
  let stdout t = t.stdout

  (** {6 Running a sub-process} *)

  type cmd = string * string array
  type redirect = [ `Redirect of Unix.file_descr | `Pipe ]
  
  let sh cmd = "sh", [| "sh"; "-c"; cmd |]

  let with_proc ?(stdin=`Pipe) ?(stdout=`Pipe) ?(stderr=`Pipe) ?env
  (prog, args) f =
    let stdin = match
 
end
