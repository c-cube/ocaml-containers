
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

(** {1 Serialize Bencode on disk with persistency guarantees}

    This module provides an append-only interface to some file, with
    synchronized access and fsync() called after every write.

    It currently uses [Unix.O_SYNC] to guarantee that writes are saved to
    the disk, so {b WRITES ARE SLOW}. On the other hand, several
    processes can access the same file and append data without risks of
    losing written values or race conditions.
    
    Similarly, reads are atomic (require locking) and provide only
    a fold interface.
    *)

type t = {
  file : Unix.file_descr;
  lock_file : Unix.file_descr;
}

let open_out ?lock filename =
  let lock = match lock with
  | None -> filename
  | Some l -> l
  in
  let lock_file = Unix.openfile lock [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
  let file = Unix.openfile filename
    [Unix.O_CREAT; Unix.O_APPEND; Unix.O_WRONLY; Unix.O_SYNC] 0o644
  in
  { file; lock_file; }

let close_out out =
  Unix.close out.file

let write_string out s =
  Unix.lockf out.lock_file Unix.F_LOCK 0;
  try
    (* go to the end of the file *)
    ignore (Unix.lseek out.file 0 Unix.SEEK_END);
    (* call write() until everything is written *)
    let rec write_all n =
      if n >= String.length s
        then ()
        else
          let n' = n + Unix.write out.file s n (String.length s - n) in
          write_all n'
    in
    write_all 0;
    Unix.lockf out.lock_file Unix.F_ULOCK 0;
  with e ->
    (* unlock in any case *)
    Unix.lockf out.lock_file Unix.F_ULOCK 0;
    raise e

let write out b =
  let s = Bencode.to_string b in
  write_string out s

let write_batch out l =
  let buf = Buffer.create 255 in
  List.iter (fun b -> Bencode.to_buf buf b) l;
  let s = Buffer.contents buf in
  write_string out s

type 'a result =
  | Ok of 'a
  | Error of string

let read ?lock filename acc f =
  let lock = match lock with
  | None -> filename
  | Some l -> l
  in
  (* lock file before reading, to observe a consistent state *)
  let lock_file = Unix.openfile lock [Unix.O_CREAT; Unix.O_RDONLY] 0o644 in
  Unix.lockf lock_file Unix.F_RLOCK 0;
  try
    let file = Unix.openfile filename [Unix.O_RDONLY] 0o644 in
    (* read bencode values *)
    let decoder = Bencode.mk_decoder () in
    let len = 256 in
    let buf = String.create len in
    (* read a chunk of input and parse it *)
    let rec next_val acc =
      let n = Unix.read file buf 0 len in
      if n = 0
        then Ok acc  (* finished *)
        else match Bencode.parse decoder buf 0 n with
          | Bencode.ParseOk v ->
            let acc = f acc v in
            resume acc
          | Bencode.ParseError e -> Error e
          | Bencode.ParsePartial -> next_val acc
    (* consume what remains of input *)
    and resume acc = match Bencode.parse_resume decoder with
      | Bencode.ParseOk v ->
        let acc = f acc v in
        resume acc
      | Bencode.ParseError e -> Error e
      | Bencode.ParsePartial -> next_val acc
    in
    let res = next_val acc in
    (* cleanup *)
    Unix.close file;
    Unix.lockf lock_file Unix.F_ULOCK 0;
    Unix.close lock_file;
    res
  with e ->
    Unix.lockf lock_file Unix.F_ULOCK 0;
    Unix.close lock_file;
    raise e
