(*
Copyright (c) 2014, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
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

(** {1 Interface to Delimcc (Invert control flow)} *)

type 'a gen = unit -> 'a option

type 'a res =
  | Start
  | Yield of 'a
  | Stop

let _ret_none () = None
let _ret_unit () = ()

let to_gen seq =
  let p = Delimcc.new_prompt () in
  let _next = ref None in
  ignore (Delimcc.push_prompt p
    (fun () ->
      Delimcc.take_subcont p (fun c () -> _next := Some c; Start);
      seq
        (fun x ->
          Delimcc.take_subcont p (fun c () -> _next := Some c; Yield x)
        );
      _next := None;
      Stop
    ));
  (* call next subcont *)
  let rec next () =
    match !_next with
    | None -> None
    | Some f ->
        begin match Delimcc.push_delim_subcont f _ret_unit with
        | Start -> next ()
        | Yield x -> Some x
        | Stop -> None
        end
  in
  next
