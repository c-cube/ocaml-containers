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

(** {1 IO Monad}

A simple abstraction over blocking IO, with strict evaluation. This is in
no way an alternative to Lwt/Async if you need concurrency.

@since 0.3.3
*)

(**
Examples:

- obtain the list of lines of a file:

{[
let l = CCIO.((with_in "/tmp/some_file" >>>= read_lines) |> run_exn);;
]}

- transfer one file into another:

{[
# let a = CCIO.(
  with_in "input" >>>= fun ic ->
  with_out ~flags:[Open_creat] "output" >>>= fun oc ->
  Seq.chunks 512 ic
  |> Seq.output oc
) ;;

# run a;;
]}
*)

type 'a t
type 'a io = 'a t

type 'a with_finalizer
(** A value of type ['a with_finalizer] is similar to a value ['a t] but
    also contains a finalizer that must be run to cleanup.
    See {!(>>>=)} to get rid of it. *)

type 'a or_error = [ `Ok of 'a | `Error of string ]

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Wait for the result of an action, then use a function to build a
    new action and execute it *)

val return : 'a -> 'a t
(** Just return a value *)

val repeat : int -> 'a t -> 'a list t
(** Repeat an IO action as many times as required *)

val repeat' : int -> 'a t -> unit t
(** Same as {!repeat}, but ignores the result *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map values *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t

val bind : ?finalize:(unit t) -> ('a -> 'b t) -> 'a t -> 'b t
(** [bind f a] runs the action [a] and applies [f] to its result
    to obtain a new action. It then behaves exactly like this new
    action.
    @param finalize an optional action that is always run after evaluating
      the whole action *)

val pure : 'a -> 'a t
val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

val lift : ('a -> 'b) -> 'a t -> 'b t
(** Synonym to {!map} *)

val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

val sequence : 'a t list -> 'a list t
(** Runs operations one by one and gather their results *)

val sequence_map : ('a -> 'b t) -> 'a list -> 'b list t
(** Generalization of {!sequence} *)

val fail : string -> 'a t
(** [fail msg] fails with the given message. Running the IO value will
    return an [`Error] variant *)

(** {2 Finalizers} *)

val (>>>=) : 'a with_finalizer -> ('a -> 'b t) -> 'b t
(** Same as {!(>>=)}, but taking the finalizer into account. Once this
    IO value is done executing, the finalizer is executed and the resource,
    fred. *)

(** {2 Running} *)

val run : 'a t -> 'a or_error
(** Run an IO action.
    @return either [`Ok x] when [x] is the successful result of the
      computation, or some [`Error "message"] *)

exception IO_error of string

val run_exn : 'a t -> 'a
(** Unsafe version of {!run}. It assumes non-failure.
    @raise IO_error if the execution didn't go well *)

val register_printer : (exn -> string option) -> unit
(** [register_printer p] register [p] as a possible failure printer.
    If [run a] raises an exception [e], [p e] is evaluated. If [p e = Some msg]
    then the error message will be [msg], otherwise other printers will
    be tried *)

(** {2 Standard Wrappers} *)

(** {6 Input} *)

val with_in : ?mode:int -> ?flags:open_flag list ->
              string -> in_channel with_finalizer
(** Open an input file with the given optional flag list.
    It yields a [in_channel] with a finalizer attached. See {!(>>>=)} to
    use it. *)

val read : in_channel -> Bytes.t -> int -> int -> int t
(** Read a chunk into the given string *)

val read_line : in_channel -> string option t
(** Read a line from the channel. Returns [None] if the input is terminated. *)

val read_lines : in_channel -> string list t
(** Read all lines eagerly *)

val read_all : in_channel -> string t
(** Read the whole channel into a buffer, then converted into a string *)

(** {6 Output} *)

val with_out : ?mode:int -> ?flags:open_flag list ->
               string -> out_channel with_finalizer
(** Same as {!with_in} but for an output channel *)

val with_out_a : ?mode:int -> ?flags:open_flag list ->
                  string -> out_channel with_finalizer
(** Similar to {!with_out} but with the [Open_append] and [Open_creat]
    flags activated *)

val write : out_channel -> string -> int -> int -> unit t

val write_str : out_channel -> string -> unit t

val write_buf : out_channel -> Buffer.t -> unit t

val write_line : out_channel -> string -> unit t

val flush : out_channel -> unit t

(* TODO: printf/fprintf wrappers *)

(** {2 Streams}

Iterators on chunks of bytes, or lines, or any other value using combinators.
Those iterators are usable only once, because their source might
be usable only once (think of a socket) *)

module Seq : sig
  type 'a t
  (** An IO stream of values of type 'a, consumable (iterable only once) *)

  val map : ('a -> 'b io) -> 'a t -> 'b t
  (** Map values with actions *)

  val map_pure : ('a -> 'b) -> 'a t -> 'b t
  (** Map values with a pure function *)

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t

  val filter : ('a -> bool) -> 'a t -> 'a t

  val flat_map : ('a -> 'b t io) -> 'a t -> 'b t
  (** Map each value to a sub sequence of values *)

  val take : int -> 'a t -> 'a t

  val drop : int -> 'a t -> 'a t

  val take_while : ('a -> bool io) -> 'a t -> 'a t

  val drop_while : ('a -> bool io) -> 'a t -> 'a t

  val general_iter : ('b -> 'a -> [`Stop | `Continue of ('b * 'c option)] io) ->
                      'b -> 'a t -> 'c t
  (** [general_iter f acc seq] performs a [filter_map] over [seq],
      using [f]. [f] is given a state and the current value, and
      can either return [`Stop] to indicate it stops traversing,
      or [`Continue (st, c)] where [st] is the new state and
      [c] an optional output value.
      The result is the stream of values output by [f] *)

  val tee : ('a -> unit io) list -> 'a t -> 'a t
  (** [tee funs seq] behaves like [seq], but each element is given to
      every function [f] in [funs]. This function [f] returns an action that
      is eagerly executed. *)

  (** {6 Consume} *)

  val iter : ('a -> _ io) -> 'a t -> unit io
  (** Iterate on the stream, with an action for each element *)

  val length : _ t -> int io
  (** Length of the stream *)

  val fold : ('b -> 'a -> 'b io) -> 'b -> 'a t -> 'b io
  (** [fold f acc seq] folds over [seq], consuming it. Every call to [f]
      has the right to return an IO value. *)

  val fold_pure : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b io
  (** [fold f acc seq] folds over [seq], consuming it. [f] is pure. *)

  (** {6 Standard Wrappers} *)

  type 'a step_result =
    | Yield of 'a
    | Stop

  type 'a gen = unit -> 'a step_result io

  val of_fun : 'a gen -> 'a t
  (** Create a stream from a function that yields an element or stops *)

  val empty : 'a t
  val singleton : 'a -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val of_list : 'a list -> 'a t
  val of_array : 'a array -> 'a t

  val chunks : size:int -> in_channel -> string t
  (** Read the channel's content into chunks of size [size] *)

  val lines : in_channel -> string t
  (** Lines of an input channel *)

  val words : string t -> string t
  (** Split strings into words at " " boundaries.
      {b NOT IMPLEMENTED} *)

  val output : ?sep:string -> out_channel -> string t -> unit io
  (** [output oc seq] outputs every value of [seq] into [oc], separated
      with the optional argument [sep] (default: None).
      It blocks until all values of [seq] are produced and written to [oc]. *)
end

(** {6 File and file names}

How to list recursively files in a directory:
{[
  CCIO.(
    File.read_dir ~recurse:true (File.make "/tmp")
    >>= Seq.output ~sep:"\n" stdout
  ) |> CCIO.run_exn ;;

  ]}

See {!File.walk} if you also need to list directories.
*)

module File : sig
  type t = string
  (** A file is always represented by its absolute path *)

  val to_string : t -> string

  val make : string -> t
  (** Build a file representation from a path (absolute or relative) *)

  val exists : t -> bool io

  val is_directory : t -> bool io

  val remove : t -> unit io

  val read_dir : ?recurse:bool -> t -> t Seq.t io
  (** [read_dir d] returns a sequence of files and directory contained
      in the directory [d] (or an empty stream if [d] is not a directory)
      @param recurse if true (default [false]), sub-directories are also
        explored *)

  val walk : t -> ([`File | `Dir] * t) Seq.t io
  (** Similar to {!read_dir} (with [recurse=true]), this function walks
      a directory recursively and yields either files or directories.
      Is a file anything that doesn't satisfy {!is_directory} (including
      symlinks, etc.) *)
end

(** {2 Low level access} *)
module Raw : sig
  val wrap : (unit -> 'a) -> 'a t
  (** [wrap f] is the IO action that, when executed, returns [f ()].
      [f] should be callable as many times as required *)
end
