
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

(** {6 Action Language for command line} *)

(** {2 Command-line Actions} *)

module Action : sig
  type 'a t
    (** Action returning a 'a *)

  type trigger = string
    (** Trigger a given action, based on the next token *)

  val return : 'a -> 'a t
    (** Return a pure value *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    (** Sequence of arguments *)

  val (>>) : 'a t -> (unit -> 'b t) -> 'b t
    (** Same as {! (>>=)}, but ignores the result of left side *)

  val ( *>) : 'a t -> 'b t -> 'b t
    (** Accept left, then returns right *)

  val accept : trigger -> unit t
    (** Accept the given trigger, fails otherwise *)

  val any : string t
    (** Any token *)

  val with_string : ?trigger:trigger -> (string -> 'a t) -> 'a t
    (** Command that takes a string *)

  val with_int : ?trigger:trigger -> (int -> 'a t) -> 'a t
    (** Command that takes an integer *)

  val with_bool : ?trigger:trigger -> (bool -> 'a t) -> 'a t

  val opt : 'a t -> 'a option t
    (** Optional action *)

  val repeat : 'a t -> 'a list t
    (** Repeated action *)

  val choice : 'a t list -> 'a t
    (** Choice between options. The first option of the list that
        does not fail will be the result (backtracking is used!) *)

  val ignore : 'a t -> unit t
    (** Ignore result *)

  val fail : string -> 'a t
    (** Fail with given message *)
end

(** {2 Main interface} *)

type 'a result =
  | Ok of 'a
  | Error of string

val parse_args : string array -> 'a Action.t -> 'a result
  (** Parse given command line *)

val parse : 'a Action.t -> 'a result
  (** Parse Sys.argv *)

val print_doc : out_channel -> 'a Action.t -> unit
  (** Print documentation on given channel *)
