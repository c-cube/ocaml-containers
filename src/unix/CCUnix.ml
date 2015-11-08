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
type 'a gen = unit -> 'a option

(** {2 Calling Commands} *)

let int_of_process_status = function
  | Unix.WEXITED i
  | Unix.WSIGNALED i
  | Unix.WSTOPPED i -> i

let str_exists s p =
  let rec f s p i =
    if i = String.length s then false
    else p s.[i] || f s p (i+1)
  in
  f s p 0

let rec iter_gen f g = match g() with
  | None -> ()
  | Some x -> f x; iter_gen f g

(* print a string, but escaped if required *)
let escape_str buf s =
  if str_exists s
      (function ' ' | '"' | '\'' | '\n' | '\t'-> true | _ -> false)
  then (
    Buffer.add_char buf '\'';
    String.iter
      (function
        | '\'' -> Buffer.add_string buf "'\\''"
        | c -> Buffer.add_char buf c
      ) s;
    Buffer.add_char buf '\'';
  ) else Buffer.add_string buf s

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

type call_result =
  < stdout:string;
    stderr:string;
    status:Unix.process_status;
    errcode:int; (** Extracted from status *)
  >

let kbprintf' buf fmt k = Printf.kbprintf k buf fmt

let call ?(bufsize=2048) ?(stdin=`Str "") ?(env=Unix.environment()) cmd =
  (* render the command *)
  let buf = Buffer.create 256 in
  kbprintf' buf cmd
    (fun buf ->
       let cmd = Buffer.contents buf in
        let oc, ic, errc = Unix.open_process_full cmd env in
        (* send stdin *)
        begin match stdin with
          | `Str s -> output_string ic s
          | `Gen g -> iter_gen (output_string ic) g
        end;
        close_out ic;
        (* read out and err *)
        let out = read_all ~size:bufsize oc in
        let err = read_all ~size:bufsize errc in
        let status = Unix.close_process_full (oc, ic, errc) in
        object
          method stdout = out
          method stderr = err
          method status = status
          method errcode = int_of_process_status status
        end
    )

type line = string

type async_call_result =
  < stdout:line gen;
    stderr:line gen;
    stdin:line -> unit; (* send a line *)
    close_in:unit; (* close stdin *)
    close_err:unit;
    close_out:unit;
    close_all:unit;  (* close all 3 channels *)
    wait:Unix.process_status;  (* block until the process ends *)
    wait_errcode:int; (* block until the process ends, then extract errcode *)
  >

let async_call ?(env=Unix.environment()) cmd =
  (* render the command *)
  let buf = Buffer.create 256 in
  kbprintf' buf cmd
    (fun buf ->
       let cmd = Buffer.contents buf in
       let oc, ic, errc = Unix.open_process_full cmd env in
       object (self)
         method stdout () =
           try Some (input_line oc)
           with End_of_file -> None
         method stderr () =
           try Some (input_line errc)
           with End_of_file -> None
         method stdin l = output_string ic l; output_char ic '\n'
         method close_in = close_out ic
         method close_out = close_in oc
         method close_err = close_in errc
         method close_all = close_out ic; close_in oc; close_in errc; ()
         method wait = Unix.close_process_full (oc, ic, errc)
         method wait_errcode = int_of_process_status self#wait
       end
    )

let stdout x = x#stdout
let stderr x = x#stderr
let status x = x#status
let errcode x = x#errcode

module Infix = struct
  let (?|) fmt = call fmt

  let (?|&) fmt = async_call fmt
end

include Infix
