(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 IO Utils}

    Simple utilities to deal with basic Input/Output tasks in a resource-safe
    way. For advanced IO tasks, the user is advised to use something
    like Lwt or Async, that are far more comprehensive.

    Examples:

    - obtain the list of lines of a file:

    {[
      # let l = CCIO.(with_in "/tmp/some_file" read_lines_l);;
    ]}

    - transfer one file into another:

    {[
      # CCIO.(
          with_in "/tmp/input"
            (fun ic ->
               let chunks = read_chunks_gen ic in
               with_out ~flags:[Open_binary; Open_creat] ~mode:0o644 "/tmp/output"
                 (fun oc ->
                    write_gen oc chunks
                 )
            )
        ) ;;
    ]}

    - Note that the lifetime of an IO generator is tied to the underlying
      channel. In the example above, [chunks] must be used in the scope of [ic].
      This will raise an error: 

    {[
      # CCIO.(
          let chunks =
            with_in "/tmp/input"
              (fun ic -> read_chunks_gen ic)
          in
          with_out ~flags:[Open_binary;Open_creat] ~mode:0o644 "/tmp/output"
             (fun oc ->
                write_gen oc chunks
             )
        ) ;;
    ]}

    @since 0.6

    @before 0.12 was in 'containers.io', now moved into 'containers'

*)

type 'a or_error = ('a, string) result
type 'a gen = unit -> 'a option
(** See [Gen] in the {{: https://github.com/c-cube/gen} gen library}. *)

(** {2 Input} *)

val with_in : ?mode:int -> ?flags:open_flag list ->
  string -> (in_channel -> 'a) -> 'a
(** Open an input file with the given optional flag list, calls the function
    on the input channel. When the function raises or returns, the
    channel is closed.
    @raise Sys_error in case of error (same as {!open_in} and {!close_in}).
    @param flags opening flags (default [[Open_text]]). [Open_rdonly] is used in any cases. *)

val read_chunks_gen : ?size:int -> in_channel -> string gen
(** Read the channel's content into chunks of size [size].
    {b NOTE} the generator must be used within the lifetime of the channel,
    see warning at the top of the file. *)

val read_line : in_channel -> string option
(** Read a line from the channel. Returns [None] if the input is terminated.
    The "\n" is removed from the line. *)

val read_lines_gen : in_channel -> string gen
(** Read all lines. The generator should be traversed only once.
    {b NOTE} the generator must be used within the lifetime of the channel,
    see warning at the top of the file. *)

val read_lines_l : in_channel -> string list
(** Read all lines into a list. *)

val read_all : ?size:int -> in_channel -> string
(** Read the whole channel into a buffer, then converted into a string.
    @param size the internal buffer size.
    @since 0.7 *)

val read_all_bytes : ?size:int -> in_channel -> Bytes.t
(** Read the whole channel into a mutable byte array.
    @param size the internal buffer size.
    @since 0.12 *)

(** {2 Output} *)

val with_out : ?mode:int -> ?flags:open_flag list ->
  string -> (out_channel -> 'a) -> 'a
(** Like {!with_in} but for an output channel.
    @param flags opening flags (default [[Open_creat; Open_trunc; Open_text]]).
    @raise Sys_error in case of error (same as {!open_out} and {!close_out}).
    [Open_wronly] is used in any cases. *)

val with_out_a : ?mode:int -> ?flags:open_flag list ->
  string -> (out_channel -> 'a) -> 'a
(** Like {!with_out} but with the [[Open_append; Open_creat; Open_wronly]]
    flags activated, to append to the file.
    @raise Sys_error in case of error (same as {!open_out} and {!close_out}). *)

val write_line : out_channel -> string -> unit
(** Write the given string on the channel, followed by "\n". *)

val write_gen : ?sep:string -> out_channel -> string gen -> unit
(** Write the given strings on the output. If provided, add [sep] between
    every two strings (but not at the end). *)

val write_lines : out_channel -> string gen -> unit
(** Write every string on the output, followed by "\n". *)

val write_lines_l : out_channel -> string list -> unit

(** {2 Both} *)

val with_in_out : ?mode:int -> ?flags:open_flag list ->
  string -> (in_channel -> out_channel -> 'a) -> 'a
(** Combines {!with_in} and {!with_out}.
    @param flags opening flags (default [[Open_creat]]).
    @raise Sys_error in case of error.
    @since 0.12 *)

val copy_into : ?bufsize:int -> in_channel -> out_channel -> unit
(** [copy_into ic oc] writes the content of [ic] into [oc].
    It is a blocking call.
    @since NEXT_RELEASE *)

(** {2 Misc for Generators} *)

val tee : ('a -> unit) list -> 'a gen -> 'a gen
(** [tee funs gen] behaves like [gen], but each element is given to
    every function [f] in [funs] at the time the element is produced.
    The returned generator will raise any exception that [f] raises *)

(** {2 File and file names}

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
  type t = string
  (** A file should be represented by its absolute path, but currently
      this is not enforced. *)

  val to_string : t -> string

  val make : string -> t
  (** Build a file representation from a path (absolute or relative). *)

  val exists : t -> bool

  val is_directory : t -> bool

  val remove_exn : t -> unit
  (** [remove_exn path] tries to remove the file at [path] from the
      file system.

      @raise Sys_error if there is no file at [path] or access rights are wrong.
      @since 0.8 *)

  val remove : t -> unit or_error
  (** Like [remove_exn] but with an error monad.
      @since 0.8 *)

  val remove_noerr : t -> unit
  (** Like [remove_exn] but do not raise any exception on failure.
      @since 0.8 *)

  val read_dir : ?recurse:bool -> t -> t gen
  (** [read_dir d] returns a sequence of files and directory contained
      in the directory [d] (or an empty stream if [d] is not a directory).
      @raise Sys_error in case of error (e.g. permission denied).
      @param recurse if true (default [false]), sub-directories are also
        explored. *)

  val read_exn : t -> string
  (** Read the content of the given file, or raises some exception.
      @raise Sys_error in case of error.
      @since 0.16 *)

  val read : t -> string or_error
  (** Read the content of the given file.
      @since 0.16 *)

  val append_exn : t -> string -> unit
  (** Append the given string into the given file, possibly raising.
      @raise Sys_error in case of error.
      @since 0.16 *)

  val append : t -> string -> unit or_error
  (** Append the given string into the given file.
      @since 0.16 *)

  val write_exn : t -> string -> unit
  (** Write the given string into the given file, possibly raising.
      @raise Sys_error in case of error.
      @since 0.16 *)

  val write : t -> string -> unit or_error
  (** Write the given string into the given file.
      @since 0.16 *)

  type walk_item = [`File | `Dir] * t

  val walk : t -> walk_item gen
  (** Like {!read_dir} (with [recurse=true]), this function walks
      a directory recursively and yields either files or directories.
      Is a file anything that doesn't satisfy {!is_directory} (including
      symlinks, etc.)
      @raise Sys_error in case of error (e.g. permission denied) during iteration. *)

  val walk_l : t -> walk_item list
  (** Like {!walk} but returns a list (therefore it's eager and might
      take some time on large directories).
      @since 1.1 *)

  val show_walk_item : walk_item -> string

  val with_temp :
    ?temp_dir:string -> prefix:string -> suffix:string ->
    (string -> 'a) -> 'a
    (** [with_temp ~prefix ~suffix f] will call [f] with the name of a new
        temporary file (located in [temp_dir]).
        After [f] returns, the file is deleted. Best to be used in
        combination with {!with_out}.
        See {!Filename.temp_file}.
        @since 0.17 *)
end
