
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

(** {1 High-level Functions on top of Unix} *)

type 'a or_error = [`Ok of 'a | `Error of string]

(** {2 Calling Commands} *)

type cmd = string * string array
(** A command: program + arguments *)

let cmd_of_sh s = "/bin/sh", [| "/bin/sh"; "-c"; s |]

let int_of_process_status = function
  | Unix.WEXITED i
  | Unix.WSIGNALED i
  | Unix.WSTOPPED i -> i

let read_all ?(size=1024) ic =
  let buf = ref (Bytes.create size) in
  let len = ref 0 in
  try
    while true do
      (* resize *)
      if !len = Bytes.length !buf then (
        buf := Bytes.extend !buf 0 !len;
      );
      assert (Bytes.length !buf > !len);
      let n = input ic !buf !len (Bytes.length !buf - !len) in
      len := !len + n;
      if n = 0 then raise Exit;  (* exhausted *)
    done;
    assert false (* never reached*)
  with Exit ->
    Bytes.sub_string !buf 0 !len

let call ?(stdin="") cmd =
  let cmd, args = match cmd with
    | `Sh s -> cmd_of_sh s
    | `Cmd (c, args) -> c, args
  in
  let oc, ic, errc = Unix.open_process_full cmd args in
  (* send stdin *)
  output_string ic stdin;
  close_out ic;
  (* read out and err *)
  let out = read_all oc in
  let err = read_all errc in
  let status = Unix.close_process_full (oc, ic, errc) in
  object
    method stdout = out
    method stderr = err
    method status = status
    method errcode = int_of_process_status status
  end


