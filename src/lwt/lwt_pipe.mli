
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

(** {1 Pipes, Readers, Writers}

  Stream processing using:

  - Pipe: a possibly buffered channel that can act as a reader or as a writer
  - Reader: accepts values, produces effects
  - Writer: yield values

Examples:
{[
#require "containers.lwt";;

module P = Containers_lwt.Lwt_pipe;;

let p1 =
  P.of_list CCList.(1 -- 100)
  |> P.Reader.map ~f:string_of_int;;

Lwt_io.with_file ~mode:Lwt_io.output "/tmp/foo"
  (fun oc ->
     let p2 = P.IO.write_lines oc in
     P.connect ~ownership:`InOwnsOut p1 p2;
     P.wait p2
  );;
]}

@since NEXT_RELEASE
*)

type 'a or_error = [`Ok of 'a | `Error of string]
type 'a step = ['a or_error | `End]

module LwtErr : sig
  type 'a t = 'a or_error Lwt.t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val return : 'a -> 'a t
  val fail : string -> 'a t
end

exception Closed

type ('a, +'perm) t constraint 'perm = [< `r | `w]
(** A pipe between producers of values of type 'a, and consumers of values
    of type 'a. *)

type ('a, 'perm) pipe = ('a, 'perm) t

val keep : _ t -> unit Lwt.t -> unit
(** [keep p fut] adds a pointer from [p] to [fut] so that [fut] is not
    garbage-collected before [p] *)

val is_closed : _ t -> bool

val close : _ t -> unit Lwt.t
(** [close p] closes [p], which will not accept input anymore.
    This sends [`End] to all readers connected to [p] *)

val close_async : _ t -> unit
(** Same as {!close} but closes in the background *)

val wait : _ t -> unit Lwt.t
(** Evaluates once the pipe closes *)

val create : ?max_size:int -> unit -> ('a, 'perm) t
(** Create a new pipe.
    @param max_size size of internal buffer. Default 0. *)

val connect : ?ownership:[`None | `InOwnsOut | `OutOwnsIn] ->
              ('a, [>`r]) t -> ('a, [>`w]) t -> unit
(** [connect p1 p2] forwards every item output by [p1] into [p2]'s input
    until [p1] is closed.
    @param own determines which pipes owns which (the owner, when it
      closes, also closes the ownee) *)

val link_close : _ t -> after:_ t -> unit
(** [link_close p ~after] will close [p] when [after] closes.
    if [after] is closed already, closes [p] immediately *)

val read : ('a, [>`r]) t -> 'a step Lwt.t
(** Read the next value from a Pipe *)

val write : ('a, [>`w]) t -> 'a -> unit Lwt.t
(** @raise Pipe.Closed if the writer is closed *)

val write_list : ('a, [>`w]) t -> 'a list -> unit Lwt.t
(** @raise Pipe.Closed if the writer is closed *)

val write_error : (_, [>`w]) t -> string -> unit Lwt.t
(** @raise Pipe.Closed if the writer is closed *)

(** {2 Write-only Interface and Combinators} *)

module Writer : sig
  type 'a t = ('a, [`w]) pipe

  val map : f:('a -> 'b) -> ('b, [>`w]) pipe -> 'a t
  (** Map values before writing them *)

  val send_both : 'a t -> 'a t -> 'a t
  (** [send_both a b] returns a writer [c] such that writing to [c]
      writes to [a] and [b], and waits for those writes to succeed
      before returning *)

  val send_all : 'a t list -> 'a t
  (** Generalized version of {!send_both}
      @raise Invalid_argument if the list is empty *)
end

(** {2 Read-only Interface and Combinators} *)

module Reader : sig
  type 'a t = ('a, [`r]) pipe

  val map : f:('a -> 'b) -> ('a, [>`r]) pipe -> 'b t

  val filter_map : f:('a -> 'b option) -> 'a t -> 'b t

  val fold : f:('acc -> 'a -> 'acc) -> x:'acc -> 'a t -> 'acc LwtErr.t

  val fold_s : f:('acc -> 'a -> 'acc Lwt.t) -> x:'acc -> 'a t -> 'acc LwtErr.t

  val iter : f:('a -> unit) -> 'a t -> unit LwtErr.t

  val iter_s : f:('a -> unit Lwt.t) -> 'a t -> unit LwtErr.t

  val merge_both : 'a t -> 'a t -> 'a t
  (** Merge the two input streams in a non-specified order *)

  val merge_all : 'a t list -> 'a t
  (** Merge all the input streams
      @raise Invalid_argument if the list is empty *)

  val append : 'a t -> 'a t -> 'a t
  (** [append a b] reads from [a] until [a] closes, then reads from [b]
      and closes when [b] closes *)
end

(** {2 Conversions} *)

val of_list : 'a list -> 'a Reader.t

val of_array : 'a array -> 'a Reader.t

val of_string : string -> char Reader.t

val to_list_rev : ('a,[>`r]) t -> 'a list LwtErr.t

val to_list : ('a,[>`r]) t -> 'a list LwtErr.t

val to_list_exn : ('a,[>`r]) t -> 'a list Lwt.t
(** Same as {!to_list}, but can fail with
    @raise Failure if some error is met *)

val to_buffer : Buffer.t -> (char ,[>`r]) t -> unit LwtErr.t

val to_buffer_str : ?sep:string -> Buffer.t -> (string, [>`r]) t -> unit LwtErr.t

val to_string : (char, [>`r]) t -> string LwtErr.t

val join_strings  : ?sep:string -> (string, [>`r]) t -> string LwtErr.t

(** {2 Basic IO wrappers} *)

module IO : sig
  val read : ?bufsize:int -> Lwt_io.input_channel -> string Reader.t

  val read_lines : Lwt_io.input_channel -> string Reader.t

  val write : Lwt_io.output_channel -> string Writer.t

  val write_lines : Lwt_io.output_channel -> string Writer.t
end
