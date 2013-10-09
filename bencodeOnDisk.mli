
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
    It needs {b Extunix} to compile (needs fsync).
    *)

type t
  (** Handle to a file on which we can append values atomically *)

val open_out : ?lock:string -> string -> t
  (** Open the given file for appending values. Creates the file
      if it doesn't exist.
      @param lock, if provided, is the name of the lock file used. By default,
        the file that is provided for writing is also used for locking.
      @raise Unix.Unix_error if some IO error occurs. *)

val close_out : t -> unit
  (** Close the file descriptor *)

val write : t -> Bencode.t -> unit
  (** Write "atomically" a value to the end of the file *)

type 'a result =
  | Ok of 'a
  | Error of string

val read : ?lock:string -> string -> 'a -> ('a -> Bencode.t -> 'a) -> 'a result
  (** Fold on values serialized in the given file.
      @param lock see {!open_out}.
      @raise Unix.Unix_error if some IO error occurs. *)
