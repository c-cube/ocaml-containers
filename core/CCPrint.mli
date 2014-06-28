
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

(** {1 GADT Description of Printers}

This module provides combinators to build printers for user-defined types.
It doesn't try to do {b pretty}-printing (see for instance Pprint for this),
but a simple way to print complicated values without writing a lot of code.
*)

type 'a sequence = ('a -> unit) -> unit

type 'a t = Buffer.t -> 'a -> unit
  (** A printer for the type ['a] *)

(** {2 Combinators} *)

val silent : 'a t (** prints nothing *)

val unit : unit t
val int : int t
val string : string t
val bool : bool t
val float3 : float t (* 3 digits after . *)
val float : float t

val list : ?start:string -> ?stop:string -> ?sep:string -> 'a t -> 'a list t
val array : ?start:string -> ?stop:string -> ?sep:string -> 'a t -> 'a array t
val arrayi : ?start:string -> ?stop:string -> ?sep:string -> (int * 'a) t -> 'a array t
val seq : ?start:string -> ?stop:string -> ?sep:string -> 'a t -> 'a sequence t

val opt : 'a t -> 'a option t

val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val map : ('a -> 'b) -> 'b t -> 'a t

(** {2 IO} *)

val output : out_channel -> 'a t -> 'a -> unit
val to_string : 'a t -> 'a -> string

val sprintf : ('a, Buffer.t, unit, string) format4 -> 'a
  (** print into a string *)

val fprintf : out_channel -> ('a, Buffer.t, unit, unit) format4 -> 'a
  (** Print on a channel *)

val to_file : string -> ('a, Buffer.t, unit, unit) format4 -> 'a
  (** Print to the given file *)

val printf : ('a, Buffer.t, unit, unit) format4 -> 'a
val eprintf : ('a, Buffer.t, unit, unit) format4 -> 'a

(** {2 Monadic IO} *)

module type MONAD_IO = sig
  type 'a t     (** the IO monad *)
  type output   (** Output channels *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val write : output -> string -> unit t
end

module MakeIO(M : MONAD_IO) : sig
  val output : M.output -> 'a t -> 'a -> unit M.t
  (** Output a single value *)

  val printl : M.output -> 'a t -> 'a -> unit M.t
  (** Output a value and add a newline "\n" after. *)

  val fprintf : M.output -> ('a, Buffer.t, unit, unit M.t) format4 -> 'a
  (** Fprintf on a monadic output *)
end
