
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

module BS = BencodeStream

type t = {
  name : string;
  out : out_channel;
  encoder : BS.Encode.t;
  cleanup : bool;
  mutable context : string list;
}

let __new_name =
  let r = ref 0 in
  fun () ->
    let name = Printf.sprintf "Tell.log_%d" !r in
    incr r;
    name

let to_chan ?(cleanup=false) o = {
  name = __new_name ();
  out = o;
  encoder = BS.Encode.create (`Out o);
  cleanup;
  context = [];
}

let to_file filename =
  let o = open_out filename in
  to_chan ~cleanup:true o

let close log =
  if log.cleanup
    then close_out log.out

let step log msg =
  BS.Encode.push log.encoder BS.BeginDict;
  BS.Encode.push log.encoder (BS.String "step");
  BS.Encode.push log.encoder (BS.String msg);
  BS.Encode.push log.encoder BS.End
  
let enter log =
  BS.Encode.push log.encoder BS.BeginList

let exit log =
  BS.Encode.push log.encoder BS.End

let within ~log f =
  BS.Encode.push log.encoder BS.BeginDict;
  BS.Encode.push log.encoder (BS.String "section");
  try
    let x = f () in
    BS.Encode.push log.encoder BS.End;
    x
  with e ->
    BS.Encode.push log.encoder BS.End;
    raise e

module B = struct
  let step ~log format =
    exit log;
    let b = Buffer.create 24 in
    Printf.kbprintf
      (fun b ->
        BS.Encode.push log.encoder (BS.String (Buffer.contents b)))
      b format

  let enter ~log format =
    let b = Buffer.create 24 in
    let x = Printf.kbprintf
      (fun b ->
        BS.Encode.push log.encoder (BS.String (Buffer.contents b)))
      b format
    in
    enter log;
    x

  let exit ~log format =
    exit log;
    let b = Buffer.create 24 in
    Printf.kbprintf
      (fun b ->
        BS.Encode.push log.encoder (BS.String (Buffer.contents b)))
      b format
end
