
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

(** {1 IO Utils} *)

type 'a gen = unit -> 'a option  (** See {!CCGen} *)

let with_in ?(mode=0o644) ?(flags=[]) filename f =
  let ic = open_in_gen flags mode filename in
  try
    let x = f ic in
    close_in ic;
    x
  with e ->
    close_in ic;
    raise e

let read_chunks ?(size=1024) ic =
  let buf = Bytes.create size in
  let eof = ref false in
  let next() =
    if !eof then None
    else
      let n = input ic buf 0 size in
      if n = 0
      then None
      else Some (Bytes.sub_string buf 0 n)
  in
  next

let read_line ic =
  try Some (input_line ic)
  with End_of_file -> None

let read_lines ic =
  let stop = ref false in
  fun () ->
    if !stop then None
    else try Some (input_line ic)
    with End_of_file -> (stop:=true; None)

let read_lines_l ic =
  let l = ref [] in
  try
    while true do
      l := input_line ic :: !l
    done;
    assert false
  with End_of_file ->
    List.rev !l

let read_all ic =
  let buf = ref (Bytes.create 256) in
  let len = ref 0 in
  try
    while true do
      (* resize *)
      if !len = Bytes.length !buf then (
        let buf' = Bytes.create (2* !len) in
        Bytes.blit !buf 0 buf' 0 !len;
        buf := buf'
      );
      assert (Bytes.length !buf > !len);
      let n = input ic !buf !len (Bytes.length !buf - !len) in
      len := !len + n;
      if n = 0 then raise Exit;  (* exhausted *)
    done;
    assert false (* never reached*)
  with Exit ->
    Bytes.sub_string !buf 0 !len

let with_out ?(mode=0o644) ?(flags=[]) filename f =
  let oc = open_out_gen flags mode filename in
  try
    let x = f oc in
    close_out oc;
    x
  with e ->
    close_out oc;
    raise e

let with_out_a ?mode ?(flags=[]) filename f =
  with_out ?mode ~flags:(Open_creat::Open_append::flags) filename f

let write_line oc s =
  output_string oc s;
  output_char oc '\n'

let write_gen ?(sep="") oc g =
  let rec recurse () = match g() with
    | None -> ()
    | Some s ->
        output_string oc sep;
        output_string oc s;
        recurse ()
  in match g() with
    | None -> ()
    | Some s ->
        output_string oc s;
        recurse ()

let rec write_lines oc g = match g () with
  | None -> ()
  | Some l ->
      write_line oc l;
      write_lines oc g

let write_lines_l oc l =
  List.iter (write_line oc) l

let tee funs g () = match g() with
  | None -> None
  | Some x as res ->
      List.iter
        (fun f ->
          try f x
          with _ -> ()
        ) funs;
      res

(* TODO: lines/unlines:  string gen -> string gen *)

(* TODO:  words: string gen -> string gen,
  with a state machine that goes:
  - 0: read input chunk
  - switch to "search for ' '", and yield word
  - goto 0 if no ' ' found
  - yield leftover when g returns Stop
*)

module File = struct
  type t = string

  let to_string f = f

  let make f =
    if Filename.is_relative f
      then Filename.concat (Sys.getcwd()) f
      else f

  let exists f = Sys.file_exists f

  let is_directory f = Sys.is_directory f

  let remove f = Sys.remove f

  let read_dir_base d =
    if Sys.is_directory d
    then
      let arr = Sys.readdir d in
      CCGen.of_array arr
    else CCGen.empty

  let cons_ x tl =
    let first=ref true in
    fun () ->
      if !first then (
        first := false;
        Some x
      ) else tl ()

  let rec walk d =
    if Sys.is_directory d
    then
      let arr = Sys.readdir d in
      let tail = CCGen.of_array arr in
      let tail = CCGen.flat_map
        (fun s -> walk (Filename.concat d s))
        tail
      in cons_ (`Dir,d) tail
    else CCGen.singleton (`File, d)

  type walk_item = [`File | `Dir] * t

  let read_dir ?(recurse=false) d =
    if recurse
    then
      CCGen.filter_map
        (function
          | `File, f -> Some f
          | `Dir, _ -> None
        ) (walk d)
    else read_dir_base d

  let show_walk_item (i,f) =
    (match i with
      | `File -> "file:"
      | `Dir -> "dir: "
    ) ^ f
end
