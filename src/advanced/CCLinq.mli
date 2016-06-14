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

(** {1 LINQ-like operations on collections}

The purpose is to provide powerful combinators to express iteration,
transformation and combination of collections of items. This module depends
on several other modules, including {!CCList} and {!CCSequence}.

Functions and operations are assumed to be referentially transparent, i.e.
they should not rely on external side effects, they should not rely on
the order of execution.

@deprecated use {{: https://github.com/c-cube/olinq} OLinq}

{[

CCLinq.(
  of_list [1;2;3]
  |> flat_map (fun x -> Sequence.(x -- (x+10)))
  |> sort ()
  |> count ()
  |> flat_map PMap.to_seq
  |> List.run
);;
- : (int * int) list = [(13, 1); (12, 2); (11, 3); (10, 3); (9, 3);
    (8, 3); (7, 3); (6, 3); (5, 3); (4, 3); (3, 3); (2, 2); (1, 1)]


CCLinq.(
  IO.slurp_file "/tmp/foo"
  |> IO.lines
  |> sort ()
  |> IO.to_file_lines "/tmp/bar"
);;
- :  `Ok ()
]}

{b DEPRECATED, use "OLinq" (standalone library) instead}

{b status: deprecated}

*)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a hash = 'a -> int
type 'a with_err = [`Ok of 'a | `Error of string ]

(** {2 Polymorphic Maps} *)
module PMap : sig
  type ('a, 'b) t

  val get : ('a,'b) t -> 'a -> 'b option

  val size : (_,_) t -> int

  val to_seq : ('a, 'b) t -> ('a * 'b) sequence

  val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Transform values *)

  val to_list : ('a,'b) t -> ('a*'b) list

  val reverse : ?cmp:'b ord -> ?eq:'b equal -> ?hash:'b hash -> unit ->
                ('a,'b) t -> ('b,'a list) t
  (** Reverse relation of the map, as a multimap *)

  val reverse_multimap : ?cmp:'b ord -> ?eq:'b equal -> ?hash:'b hash -> unit ->
                          ('a,'b list) t -> ('b,'a list) t
  (** Reverse relation of the multimap *)

  val fold : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> ('a,'b) t -> 'acc
  (** Fold on the items of the map *)

  val fold_multimap : ('acc -> 'a -> 'b -> 'acc) -> 'acc ->
                      ('a,'b list) t -> 'acc
  (** Fold on the items of the multimap *)

  val get_seq : 'a -> ('a, 'b) t -> 'b sequence
  (** Select a key from a map and wrap into sequence *)

  val iter : ('a,'b) t -> ('a*'b) sequence
  (** View a multimap as a proper collection *)

  val flatten : ('a,'b sequence) t -> ('a*'b) sequence
  (** View a multimap as a collection of individual key/value pairs *)

  val flatten_l : ('a,'b list) t -> ('a*'b) sequence
  (** View a multimap as a list of individual key/value pairs *)
end

(** {2 Query operators} *)

type 'a t
(** Type of a query that returns zero, one or more values of type 'a *)

(** {6 Initial values} *)

val empty : 'a t
(** Empty collection *)

val start : 'a -> 'a t
(** Start with a single value
    @deprecated since 0.13, use {!return} instead *)

val return : 'a -> 'a t
(** Return one value *)

val of_list : 'a list -> 'a t
(** Query that just returns the elements of the list *)

val of_array : 'a array -> 'a t
val of_array_i : 'a array -> (int * 'a) t

val range : int -> int -> int t
(** [range i j] goes from [i] up to [j] included *)

val (--) : int -> int -> int t
(** Synonym to {!range} *)

val of_hashtbl : ('a,'b) Hashtbl.t -> ('a * 'b) t

val of_seq : 'a sequence -> 'a t
(** Query that returns the elements of the given sequence. *)

val of_queue : 'a Queue.t -> 'a t

val of_stack : 'a Stack.t -> 'a t

val of_string : string -> char t
(** Traverse the characters of the string *)

(** {6 Execution} *)

val run : ?limit:int -> 'a t -> 'a sequence
(** Execute the query, possibly returning an error if things go wrong
    @param limit max number of values to return *)

val run1 : 'a t -> 'a
(** Run the query and return the first value
    @raise Not_found if the query succeeds with 0 element *)

val run_no_optim : ?limit:int -> 'a t -> 'a sequence
(** Run without any optimization *)

(** {6 Basics} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map each value *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Infix synonym of {!map} *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter out values that do not satisfy predicate *)

val size : _ t -> int t
(** [size t] returns one value, the number of items returned by [t] *)

val choose : 'a t -> 'a t
(** Choose one element (if any, otherwise empty) in the collection.
    This is like a "cut" in prolog. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Filter and map elements at once *)

val flat_map : ('a -> 'b sequence) -> 'a t -> 'b t
(** Same as {!flat_map} but using sequences *)

val flat_map_l : ('a -> 'b list) -> 'a t -> 'b t
(** map each element to a collection and flatten the result *)

val flatten : 'a list t -> 'a t

val flatten_seq : 'a sequence t -> 'a t

val take : int -> 'a t -> 'a t
(** Take at most [n] elements *)

val take_while : ('a -> bool) -> 'a t -> 'a t
(** Take elements while they satisfy a predicate *)

val sort : ?cmp:'a ord -> unit -> 'a t -> 'a t
(** Sort items by the given comparison function *)

val distinct : ?cmp:'a ord -> unit -> 'a t -> 'a t
(** Remove duplicate elements from the input collection.
    All elements in the result are distinct. *)

(** {6 Aggregation} *)

val group_by : ?cmp:'b ord -> ?eq:'b equal -> ?hash:'b hash ->
               ('a -> 'b) -> 'a t -> ('b,'a list) PMap.t t
(** [group_by f] takes a collection [c] as input, and returns
    a multimap [m] such that for each [x] in [c],
    [x] occurs in [m] under the key [f x]. In other words, [f] is used
    to obtain a key from [x], and [x] is added to the multimap using this key. *)

val group_by' : ?cmp:'b ord -> ?eq:'b equal -> ?hash:'b hash ->
                ('a -> 'b) -> 'a t -> ('b * 'a list) t

val count : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
             unit -> 'a t -> ('a, int) PMap.t t
(** [count c] returns a map from elements of [c] to the number
    of time those elements occur. *)

val count' : ?cmp:'a ord -> unit -> 'a t -> ('a * int) t

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
(** Fold over the collection *)

val reduce : ('a -> 'b) -> ('a -> 'b -> 'b) -> ('b -> 'c) ->
             'a t -> 'c t
(** [reduce start mix stop q] uses [start] on the first element of [q],
    and combine the result with following elements using [mix]. The final
    value is transformed using [stop]. *)

val is_empty : 'a t -> bool t

val sum : int t -> int t

val contains : ?eq:'a equal -> 'a -> 'a t -> bool t

val average : int t -> int t
val max : int t -> int t
val min : int t -> int t

val for_all : ('a -> bool) -> 'a t -> bool t
val exists : ('a -> bool) -> 'a t -> bool t
val find : ('a -> bool) -> 'a t -> 'a option t
val find_map : ('a -> 'b option) -> 'a t -> 'b option t

(** {6 Binary Operators} *)

val join : ?cmp:'key ord -> ?eq:'key equal -> ?hash:'key hash ->
            ('a -> 'key) -> ('b -> 'key) ->
            merge:('key -> 'a -> 'b -> 'c option) ->
            'a t -> 'b t -> 'c t
(** [join key1 key2 ~merge] is a binary operation
    that takes two collections [a] and [b], projects their
    elements resp. with [key1] and [key2], and combine
    values [(x,y)] from [(a,b)] with the same [key]
    using [merge]. If [merge] returns [None], the combination
    of values is discarded. *)

val group_join : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
                  ('b -> 'a) -> 'a t -> 'b t ->
                  ('a, 'b list) PMap.t t
(** [group_join key2] associates to every element [x] of
    the first collection, all the elements [y] of the second
    collection such that [eq x (key y)] *)

val product : 'a t -> 'b t -> ('a * 'b) t
(** Cartesian product *)

val append : 'a t -> 'a t -> 'a t
(** Append two collections together *)

val inter : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash -> unit ->
            'a t -> 'a t -> 'a t
(** Intersection of two collections. Each element will occur at most once
    in the result *)

val union : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash -> unit ->
            'a t -> 'a t -> 'a t
(** Union of two collections. Each element will occur at most once
    in the result *)

val diff : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash -> unit ->
            'a t -> 'a t -> 'a t
(** Set difference *)

(** {6 Tuple and Options} *)

(** Specialized projection operators *)

val fst : ('a * 'b) t -> 'a t

val snd : ('a * 'b) t -> 'b t

val map1 : ('a -> 'b) -> ('a * 'c) t -> ('b * 'c) t

val map2 : ('a -> 'b) -> ('c * 'a) t -> ('c * 'b) t

val flatten_opt : 'a option t -> 'a t
(** Flatten the collection by removing options *)

(** {6 Applicative} *)

val pure : 'a -> 'a t
(** Synonym to {!return} *)

val app : ('a -> 'b) t -> 'a t -> 'b t
(** Apply each function to each value *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** Infix synonym to {!app} *)

(** {6 Monad}

Careful, those operators do not allow any optimization before running the
query, they might therefore be pretty slow. *)

val bind : ('a -> 'b t) -> 'a t -> 'b t
(** Use the result of a query to build another query and immediately run it. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Infix version of {!bind} *)

(** {6 Misc} *)

val lazy_ : 'a lazy_t t -> 'a t

val opt_unwrap : 'a option t -> 'a t

val reflect : 'a t -> 'a sequence t
(** [reflect q] evaluates all values in [q] and returns a sequence
    of all those values. Also blocks optimizations *)

(** {6 Infix} *)

module Infix : sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val (--) : int -> int -> int t
end

(** {6 Adapters} *)

val to_seq : 'a t  -> 'a sequence t
(** Build a (re-usable) sequence of elements, which can then be
    converted into other structures *)

val to_hashtbl : ('a * 'b) t -> ('a, 'b) Hashtbl.t t
(** Build a hashtable from the collection *)

val to_queue : 'a t -> 'a Queue.t t

val to_stack : 'a t -> 'a Stack.t t

module List : sig
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list t
  val run : 'a t -> 'a list
end

module Array : sig
  val of_array : 'a array -> 'a t
  val to_array : 'a t -> 'a array t
  val run : 'a t -> 'a array
end

module AdaptSet(S : Set.S) : sig
  val of_set : S.t -> S.elt t
  val to_set : S.elt t -> S.t t
  val run : S.elt t -> S.t
end

module AdaptMap(M : Map.S) : sig
  val of_map : 'a M.t -> (M.key * 'a) t
  val to_pmap : 'a M.t -> (M.key, 'a) PMap.t
  val to_map : (M.key * 'a) t -> 'a M.t t
  val run : (M.key * 'a) t -> 'a M.t
end

module IO : sig
  val slurp : in_channel -> string t
  (** Slurp the whole channel in (blocking), returning the
      corresponding string. The channel will be read at most once
      during execution, and its content cached; however the channel
      might never get read because evaluation is lazy. *)

  val slurp_file : string -> string t
  (** Read a whole file (given by name) and return its content as
      a string *)

  val lines : string t -> string t
  (** Convert a string into a collection of lines *)

  val lines' : string t -> string list t
  (** Convert a string into a list of lines *)

  val join : string -> string t -> string t

  val unlines : string t -> string t
  (** Join lines together *)

  val out : out_channel -> string t -> unit
  val out_lines : out_channel -> string t -> unit
  (** Evaluate the query and print it line by line on the output *)

  (** {8 Run methods} *)

  val to_file : string -> string t -> unit with_err
  val to_file_exn : string -> string t -> unit

  val to_file_lines : string -> string t -> unit with_err
  val to_file_lines_exn : string -> string t -> unit
end
