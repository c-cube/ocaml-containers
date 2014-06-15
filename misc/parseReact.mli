
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

(** {1 Parser combinators driven by the input} *)

type ('input, 'result) t
(** parser that takes some type as input and outputs a value of type 'result
when it's done *)

(** {6 Basic Building Blocs} *)

val stop : ('a, unit) t
(** Succeed exactly at the end of input *)

val return : 'b -> ('a, 'b) t
(** Return a value *) 

val return' : (unit -> 'b) -> ('a, 'b) t
(** Suspended version of {!return}, does not evaluate yet *)

val delay : (unit -> ('a, 'b) t) -> ('a, 'b) t
(** Delay evaluation of the parser *)

val fail : ('a, 'b) t
(** Failure *)

val one : ('a, 'a) t
(** Parse one value exactly *)

val skip : ('a, unit) t
(** Ignore the next value *)

val exact : ?eq:('a -> 'a -> bool) -> 'a -> ('a, unit) t
(** Accept one value as input exactly *)

val guard : ('b -> bool) -> ('a, 'b) t -> ('a, 'b) t
(** Ensure the return value of the given parser satisfies the predicate.
    [guard f p] will be the same as [p] if [p] returns
    some [x] with [f x = true]. If [not (f x)], then [guard f p] fails. *)

val bind : ('b -> ('a, 'c) t) -> ('a, 'b) t -> ('a, 'c) t

val (>>=) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t

val (>>) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t
(** Wait for the first parser to succeed, then switch to the second one *)

val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
(** Map outputs *)

val (>>|) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
(** Infix version of {!map} *)

val (<|>) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
(** Non-deterministic choice. Both branches are evaluated in parallel *)

val pair : ('a,'b) t -> ('a, 'c) t -> ('a, ('b * 'c)) t
val triple : ('a,'b) t -> ('a, 'c) t -> ('a, 'd) t -> ('a, ('b * 'c * 'd)) t

val if_then_else : ('a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
(** Test the next input, and choose the parser based on it. Does not consume
    the input token for the test *)

(** {6 Utils} *)

val take_while : ('a -> bool) -> ('a, 'a list) t
(** Take input while it satisfies the given predicate *)

val take_n : int -> ('a, 'a list) t
(** Take n input elements *)

val skip_spaces : (char, unit) t
(** Skip whitespace (space,tab,newline) *)

val ident : (char, string) t
(** Parse identifiers (stops on whitespaces) *)

val many : sep:('a,_) t -> ('a, 'b) t -> ('a, 'b list) t
(** [many ~sep p] parses as many [p] as possible, separated by [sep]. *)

val many1 : sep:('a,_) t -> ('a, 'b) t -> ('a, 'b list) t

(** {6 Run} *)

type 'a sequence = ('a -> unit) -> unit

val run : ('a,'b) t -> 'a sequence -> 'b list
(** List of results. Each element of the list comes from a successful
    series of choices [<|>]. If no choice operator was used, the list
    contains 0 or 1 elements *)
