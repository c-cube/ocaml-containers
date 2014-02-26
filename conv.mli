
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

(** {1 Bidirectional Conversion} *)

exception ConversionFailure of string

module Sink : sig
  (** A specific sink that requires a given shape to produce
   * a value of type 'a *)
  type 'a t =
    | Int : (int -> 'a) -> 'a t
    | String : (string -> 'a) -> 'a t
    | List : (('b t -> 'b list) -> 'a) -> 'a t
    | Record : 'a record_sink -> 'a t
    | Tuple : 'a tuple_sink -> 'a t
    | Sum : (string -> ('b t -> 'b) -> 'a) -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t

  and 'r record_sink =
    | RecordField : string * 'a t * ('a -> 'r record_sink) -> 'r record_sink
    | RecordStop : 'r -> 'r record_sink

  and 't tuple_sink =
    | TupleField : 'a t * ('a -> 't tuple_sink) -> 't tuple_sink
    | TupleStop : 't -> 't tuple_sink

  and 's sum_sink =
    | SumSink : (string -> ('b t -> 'b) -> 's) -> 's sum_sink

  val int_ : int t
  val string_ : string t
  val list_ : 'a t -> 'a list t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val array_ : 'a t -> 'a array t

  val (-->) : 'a -> 'b -> 'a * 'b
  val (|:|) : (string * 'a t) -> ('a -> 'r record_sink) -> 'r record_sink
  val yield_record : 'r -> 'r record_sink
  val record : 'r record_sink -> 'r t

  val (|+|) : 'a t -> ('a -> 't tuple_sink) -> 't tuple_sink
  val yield_tuple : 't -> 't tuple_sink
  val tuple : 't tuple_sink -> 't t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  val sum : (string -> ('b t -> 'b) -> 'a) -> 'a t

  (** Universal sink, such as a serialization format *)
  class type ['a] universal = object
    method int_ : int -> 'a
    method string_ : string -> 'a
    method list_ : 'a list -> 'a
    method record : (string*'a) list -> 'a
    method tuple : 'a list -> 'a
    method sum : string -> 'a -> 'a
  end
end

module Source : sig
  (** A specific source that follows the shape of the type 'a *)
  type 'a t =
    | Int : int t
    | String : string t
    | List : 'a t -> 'a list t
    | Record : 'a record_src -> 'a t
    | Tuple : 'a tuple_src -> 'a t
    | Sum : ('a -> string * 'b t * 'b) -> 'a t
    | Map : 'a t * ('b -> 'a) -> 'b t

  and 'r record_src =
    | RecordField : string * ('r -> 'a) * 'a t * 'r record_src -> 'r record_src
    | RecordStop : 'r record_src

  and 't tuple_src =
    | TupleField : 'a t * ('t -> 'a) * 't tuple_src -> 't tuple_src
    | TupleStop : 't tuple_src

  val int_ : int t
  val string_ : string t
  val list_ : 'a t -> 'a list t

  val map : ('b -> 'a) -> 'a t -> 'b t
  val array_ : 'a t -> 'a array t

  val record_field : string -> ('r -> 'a) -> 'a t -> 'r record_src -> 'r record_src
  val record_stop : 'r record_src
  val record : 'r record_src -> 'r t

  val tuple_field : 'a t -> ('t -> 'a) -> 't tuple_src -> 't tuple_src
  val tuple_stop : 't tuple_src
  val tuple : 't tuple_src -> 't t
  val (@@@) : ('a -> 'b) -> 'a -> 'b

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  val sum : ('a -> string * 'b t * 'b) -> 'a t

  (** Universal source from type 'a. A universal type should inherit from it
      and implement the visit method by calling self-methods. *)
  class virtual ['a] universal : object
    method private int_ : 'b. 'b Sink.t -> int -> 'b
    method private string_ : 'b. 'b Sink.t -> string -> 'b
    method private list_ : 'b. 'b Sink.t -> 'a list -> 'b
    method private record : 'b. 'b Sink.t -> (string*'a) list -> 'b
    method private tuple : 'b. 'b Sink.t -> 'a list -> 'b
    method private sum : 'b. 'b Sink.t -> string -> 'a -> 'b
    method virtual visit : 'b. 'b Sink.t -> 'a -> 'b
  end
end

val into : 'a Source.t -> 'b Sink.universal -> 'a -> 'b
  (** Conversion to universal sink *)

val from : 'a Source.universal -> 'b Sink.t -> 'a -> 'b
  (** Conversion from universal source *)
