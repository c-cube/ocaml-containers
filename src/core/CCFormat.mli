
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

(** {1 Helpers for Format}

@since 0.8 *)

type 'a sequence = ('a -> unit) -> unit

type t = Format.formatter
type 'a printer = t -> 'a -> unit

(** {2 Combinators} *)

val silent : 'a printer (** prints nothing *)

val unit : unit printer
val int : int printer
val string : string printer
val bool : bool printer
val float3 : float printer (* 3 digits after . *)
val float : float printer

val char : char printer (** @since NEXT_RELEASE *)
val int32 : int32 printer (** @since NEXT_RELEASE *)
val int64 : int64 printer (** @since NEXT_RELEASE *)
val nativeint : nativeint printer (** @since NEXT_RELEASE *)


val list : ?start:string -> ?stop:string -> ?sep:string -> 'a printer -> 'a list printer
val array : ?start:string -> ?stop:string -> ?sep:string -> 'a printer -> 'a array printer
val arrayi : ?start:string -> ?stop:string -> ?sep:string ->
            (int * 'a) printer -> 'a array printer
val seq : ?start:string -> ?stop:string -> ?sep:string -> 'a printer -> 'a sequence printer

val opt : 'a printer -> 'a option printer

val pair : 'a printer -> 'b printer -> ('a * 'b) printer
val triple : 'a printer -> 'b printer -> 'c printer -> ('a * 'b * 'c) printer
val quad : 'a printer -> 'b printer -> 'c printer -> 'd printer -> ('a * 'b * 'c * 'd) printer

val map : ('a -> 'b) -> 'b printer -> 'a printer

(** {2 IO} *)

val output : t -> 'a printer -> 'a -> unit
val to_string : 'a printer -> 'a -> string

val stdout : t
val stderr : t

val sprintf : ('a, t, unit, string) format4 -> 'a
(** print into a string *)

val fprintf : t -> ('a, t, unit ) format -> 'a
(** Alias to {!Format.fprintf}
    @since NEXT_RELEASE *)

val to_file : string -> ('a, t, unit, unit) format4 -> 'a
(** Print to the given file *)
