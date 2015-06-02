
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

(** {1 IO Utils}

Simple utilities to deal with basic Input/Output tasks in a resource-safe
way. For advanced IO tasks, the user is advised to use something
like Lwt or Async, that are far more comprehensive.

{b NOTE} this was formerly a monadic IO module. The old module is now
in [containers.advanced] under the name [CCMonadIO].

Examples:

- obtain the list of lines of a file:

{[
# let l = CCIO.(with_in "/tmp/some_file" read_lines);;
]}

- transfer one file into another:

{[
# CCIO.(
  with_in "/tmp/input"
    (fun ic ->
      let chunks = read_chunks ic in
      with_out ~flags:[Open_binary] ~mode:0o644 "/tmp/output"
        (fun oc ->
          write_gen oc chunks
        )
    )
) ;;
]}

@since 0.6

*)


type 'a gen = unit -> 'a option  (** See {!Gen} *)

(** {2 Input} *)

val with_in : ?mode:int -> ?flags:open_flag list ->
              string -> (in_channel -> 'a) -> 'a
(** Open an input file with the given optional flag list, calls the function
    on the input channel. When the function raises or returns, the
    channel is closed.
    @param flags opening flags (default [[Open_text]]). [Open_rdonly] is used in any cases *)

val read_chunks : ?size:int -> in_channel -> string gen
(** Read the channel's content into chunks of size [size] *)

val read_line : in_channel -> string option
(** Read a line from the channel. Returns [None] if the input is terminated.
    The "\n" is removed from the line. *)

val read_lines : in_channel -> string gen
(** Read all lines. The generator should be traversed only once. *)

val read_lines_l : in_channel -> string list
(** Read all lines into a list *)

val read_all : ?size:int -> in_channel -> string
(** Read the whole channel into a buffer, then converted into a string.
    @param size the internal buffer size @since 0.7 *)

(** {6 Output} *)

val with_out : ?mode:int -> ?flags:open_flag list ->
               string -> (out_channel -> 'a) -> 'a
(** Same as {!with_in} but for an output channel
    @param flags opening flags (default [[Open_creat; Open_trunc; Open_text]]).
    [Open_wronly] is used in any cases *)

val with_out_a : ?mode:int -> ?flags:open_flag list ->
                  string -> (out_channel -> 'a) -> 'a
(** Similar to {!with_out} but with the [[Open_append; Open_creat; Open_wronly]]
    flags activated, to append to the file *)

val write_line : out_channel -> string -> unit
(** Write the given string on the channel, followed by "\n" *)

val write_gen : ?sep:string -> out_channel -> string gen -> unit
(** Write the given strings on the output. If provided, add [sep] between
    every two string (but not at the end) *)

val write_lines : out_channel -> string gen -> unit
(** Write every string on the output, followed by "\n". *)

val write_lines_l : out_channel -> string list -> unit

(** {2 Both} *)

val with_in_out : ?mode:int -> ?flags:open_flag list ->
                  string -> (in_channel -> out_channel -> 'a) -> 'a
(** Combines {!with_in} and {!with_out}.
    @param flags opening flags (default [[Open_creat]])
    @since NEXT_RELEASE *)

(** {2 Misc for Generators} *)

val tee : ('a -> unit) list -> 'a gen -> 'a gen
(** [tee funs gen] behaves like [gen], but each element is given to
    every function [f] in [funs] at the time the element is produced. *)

(** {6 File and file names}

How to list recursively files in a directory:
{[
# let files = CCIO.File.read_dir ~recurse:true (CCIO.File.make "/tmp");;
# CCIO.write_lines stdout files;;
]}

See {!File.walk} if you also need to list directories:

{[
# let content = CCIO.File.walk (CCIO.File.make "/tmp");;
# Gen.map CCIO.File.show_walk_item content |> CCIO.write_lines stdout;;
]}
*)

module File : sig
  type 'a or_error = [`Ok of 'a | `Error of string]
  type t = string
  (** A file is always represented by its absolute path *)

  val to_string : t -> string

  val make : string -> t
  (** Build a file representation from a path (absolute or relative) *)

  val exists : t -> bool

  val is_directory : t -> bool

  val remove_exn : t -> unit
  (** [remove_exn path] tries to remove the file at [path] from the
      file system.

      {b Raises} [Sys_error] if there is no file at [path].
      @since 0.8 *)

  val remove : t -> unit or_error
  (** Like [remove_exn] but with an error monad.
      @since 0.8 *)

  val remove_noerr : t -> unit
  (** Like [remove_exn] but do not raise any exception on failure.
      @since 0.8 *)

  val read_dir : ?recurse:bool -> t -> t gen
  (** [read_dir d] returns a sequence of files and directory contained
      in the directory [d] (or an empty stream if [d] is not a directory)
      @param recurse if true (default [false]), sub-directories are also
        explored *)

  type walk_item = [`File | `Dir] * t

  val walk : t -> walk_item gen
  (** similar to {!read_dir} (with [recurse=true]), this function walks
      a directory recursively and yields either files or directories.
      Is a file anything that doesn't satisfy {!is_directory} (including
      symlinks, etc.) *)

  val show_walk_item : walk_item -> string
end
