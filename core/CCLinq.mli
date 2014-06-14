
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

The purpose it to provide powerful combinators to express iteration,
transformation and combination of collections of items. This module depends
on several other modules, including {!CCList} and {!CCSequence}.

Functions and operations are assumed to be referentially transparent, i.e.
they should not rely on external side effects, they should not rely on
the order of execution.

{[

CCLinq.(
  start_list [1;2;3]
  |> flat_map_l (fun x -> CCList.(x -- (x+10)))
  |> sort ()
  |> count ()
  |> M.to_list |> run
);;
- : (int * int) list = [(13, 1); (12, 2); (11, 3); (10, 3); (9, 3);
    (8, 3); (7, 3); (6, 3); (5, 3); (4, 3); (3, 3); (2, 2); (1, 1)]
]}

*)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a hash = 'a -> int

type 'a collection
(** Abstract type of collections of objects of type 'a. Those cannot
    be used directly, they are to be processed using a query (type {!'a t})
    and converted to some list/sequence/array *)

(** {2 Polymorphic Maps} *)
module PMap : sig
  type ('a, 'b) t

  val get : ('a,'b) t -> 'a -> 'b option

  val get_exn : ('a,'b) t -> 'a -> 'b
  (** Unsafe version of {!get}.
      @raise Not_found if the element is not present *)

  val size : (_,_) t -> int
  
  val to_seq : ('a, 'b) t -> ('a * 'b) sequence

  val to_list : ('a, 'b) t -> ('a * 'b) list

  val to_coll : ('a, 'b) t -> ('a * 'b) collection
end

(** {2 Query operators} *)

type 'a t
(** Type of a query that returns some value of type 'a *)

(** {6 Initial values} *)

val start : 'a -> 'a t
(** Start with a single value *)

val of_list : 'a list -> 'a collection t
(** Query that just returns the elements of the list *)

val of_array : 'a array -> 'a collection t
val of_array_i : 'a array -> (int * 'a) collection t

val of_hashtbl : ('a,'b) Hashtbl.t -> ('a * 'b) collection t

val of_seq : 'a sequence -> 'a collection t
(** Query that returns the elements of the given sequence. *)

val of_queue : 'a Queue.t -> 'a collection t

val of_stack : 'a Stack.t -> 'a collection t

val of_string : string -> char collection t
(** Traverse the characters of the string *)

(** {6 Execution} *)

val run : 'a t -> 'a
(** Execute the actual query *)

val run_no_opt : 'a t -> 'a
(** Execute the query, without optimizing it at all *)

val run_list : 'a collection t -> 'a list
(** Shortcut to obtain a list *)

(** {6 Basics on Collections} *)

val map : ('a -> 'b) -> 'a collection t -> 'b collection t

val filter : ('a -> bool) -> 'a collection t -> 'a collection t

val size : _ collection t -> int t

val choose : 'a collection t -> 'a option t
(** Choose one element (if any) in the collection *)

val choose_exn : 'a collection t -> 'a t
(** Choose one element or fail.
    @raise Invalid_argument if the collection is empty *)

val filter_map : ('a -> 'b option) -> 'a collection t -> 'b collection t
(** Filter and map elements at once *)

val flat_map : ('a -> 'b collection) -> 'a collection t -> 'b collection t
(** Monadic "bind", maps each element to a collection
    and flatten the result *)

val flat_map_seq : ('a -> 'b sequence) -> 'a collection t -> 'b collection t
(** Same as {!flat_map} but using sequences *)

val flat_map_l : ('a -> 'b list) -> 'a collection t -> 'b collection t

val flatten : 'a collection collection t -> 'a collection t

val flatten_l : 'a list collection t -> 'a collection t

val take : int -> 'a collection t -> 'a collection t
(** take at most [n] elements *)

val take_while : ('a -> bool) -> 'a collection t -> 'a collection t
(** take elements while they satisfy a predicate *)

val sort : ?cmp:'a ord -> unit -> 'a collection t -> 'a collection t
(** Sort items by the given comparison function *)

val distinct : ?cmp:'a ord -> unit -> 'a collection t -> 'a collection t
(** Remove duplicate elements from the input collection.
    All elements in the result are distinct. *)

(** {6 Queries on Maps} *)

module M : sig
  val get : 'a -> ('a, 'b) PMap.t t -> 'b option t
  (** Select a key from a map *)

  val get_exn : 'a -> ('a, 'b) PMap.t t -> 'b t
  (** Unsafe version of {!get}.
      @raise Not_found if the key is not present. *)

  val iter : ('a,'b) PMap.t t -> ('a*'b) collection t
  (** View a multimap as a proper collection *)

  val flatten : ('a,'b collection) PMap.t t -> ('a*'b) collection t
  (** View a multimap as a collection of individual key/value pairs *)

  val flatten' : ('a,'b list) PMap.t t -> ('a*'b) collection t
  (** View a multimap as a collection of individual key/value pairs *)

  val map : ('b -> 'c) -> ('a, 'b) PMap.t t -> ('a, 'c) PMap.t t
  (** Transform values *)

  val to_list : ('a,'b) PMap.t t -> ('a*'b) list t

  val reverse : ?cmp:'b ord -> ?eq:'b equal -> ?hash:'b hash -> unit ->
                ('a,'b) PMap.t t -> ('b,'a list) PMap.t t
  (** Reverse relation of the map, as a multimap *)

  val reverse_multimap : ?cmp:'b ord -> ?eq:'b equal -> ?hash:'b hash -> unit ->
                          ('a,'b list) PMap.t t -> ('b,'a list) PMap.t t
  (** Reverse relation of the multimap *)

  val fold : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> ('a,'b) PMap.t t -> 'acc t
  (** Fold on the items of the map *)

  val fold_multimap : ('acc -> 'a -> 'b -> 'acc) -> 'acc ->
                      ('a,'b list) PMap.t t -> 'acc t
  (** Fold on the items of the multimap *)
end

(** {6 Aggregation} *)

val group_by : ?cmp:'b ord -> ?eq:'b equal -> ?hash:'b hash ->
               ('a -> 'b) -> 'a collection t -> ('b,'a list) PMap.t t
(** [group_by f] takes a collection [c] as input, and returns
    a multimap [m] such that for each [x] in [c],
    [x] occurs in [m] under the key [f x]. In other words, [f] is used
    to obtain a key from [x], and [x] is added to the multimap using this key. *)

val group_by' : ?cmp:'b ord -> ?eq:'b equal -> ?hash:'b hash ->
                ('a -> 'b) -> 'a collection t -> ('b * 'a list) collection t

val count : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
             unit -> 'a collection t -> ('a, int) PMap.t t
(** [count c] returns a map from elements of [c] to the number
    of time those elements occur. *)

val count' : ?cmp:'a ord -> unit -> 'a collection t -> ('a * int) collection t

val fold : ('b -> 'a -> 'b) -> 'b -> 'a collection t -> 'b t
(** Fold over the collection *)

val size : _ collection t -> int t
(** Count how many elements the collection contains *)

val reduce : ('a -> 'b) -> ('a -> 'b -> 'b) -> ('b -> 'c) ->
             'a collection t -> 'c option t
(** [reduce start mix stop q] uses [start] on the first element of [q],
    and combine the result with following elements using [mix]. The final
    value is transformed using [stop]. This returns [None] if the collection
    is empty *)

val reduce_exn : ('a -> 'b) -> ('a -> 'b -> 'b) -> ('b -> 'c) ->
                 'a collection t -> 'c t
(** Same as {!reduce} but fails on empty collections.
    @raise Invalid_argument if the collection is empty *)

val is_empty : 'a collection t -> bool t

val sum : int collection t -> int t

val contains : ?eq:'a equal -> 'a -> 'a collection t -> bool t

val average : int collection t -> int option t
val max : int collection t -> int option t
val min : int collection t -> int option t

val average_exn : int collection t -> int t
val max_exn : int collection t -> int t
val min_exn : int collection t -> int t

val for_all : ('a -> bool) -> 'a collection t -> bool t
val exists : ('a -> bool) -> 'a collection t -> bool t
val find : ('a -> bool) -> 'a collection t -> 'a option t
val find_map : ('a -> 'b option) -> 'a collection t -> 'b option t

(** {6 Binary Operators} *)

val join : ?cmp:'key ord -> ?eq:'key equal -> ?hash:'key hash ->
            ('a -> 'key) -> ('b -> 'key) ->
            merge:('key -> 'a -> 'b -> 'c option) ->
            'a collection t -> 'b collection t -> 'c collection t
(** [join key1 key2 ~merge] is a binary operation
    that takes two collections [a] and [b], projects their
    elements resp. with [key1] and [key2], and combine
    values [(x,y)] from [(a,b)] with the same [key]
    using [merge]. If [merge] returns [None], the combination
    of values is discarded. *)

val group_join : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
                  ('b -> 'a) -> 'a collection t -> 'b collection t ->
                  ('a, 'b list) PMap.t t
(** [group_join key2] associates to every element [x] of
    the first collection, all the elements [y] of the second
    collection such that [eq x (key y)] *)

val product : 'a collection t -> 'b collection t -> ('a * 'b) collection t
(** Cartesian product *)

val append : 'a collection t -> 'a collection t -> 'a collection t
(** Append two collections together *)

val inter : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash -> unit ->
            'a collection t -> 'a collection t -> 'a collection t
(** Intersection of two collections. Each element will occur at most once
    in the result *)

val union : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash -> unit ->
            'a collection t -> 'a collection t -> 'a collection t
(** Union of two collections. Each element will occur at most once
    in the result *)

val diff : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash -> unit ->
            'a collection t -> 'a collection t -> 'a collection t
(** Set difference *)

(** {6 Tuple and Options} *)

(** Specialized projection operators *)

val fst : ('a * 'b) collection t -> 'a collection t

val snd : ('a * 'b) collection t -> 'b collection t

val map1 : ('a -> 'b) -> ('a * 'c) collection t -> ('b * 'c) collection t

val map2 : ('a -> 'b) -> ('c * 'a) collection t -> ('c * 'b) collection t

val flatten_opt : 'a option collection t -> 'a collection t
(** Flatten the collection by removing options *)

val opt_get_exn : 'a option t -> 'a t
(** unwrap an option type.
    @raise Invalid_argument if the option value is [None] *)

(** {6 Monad}

Careful, those operators do not allow any optimization before running the
query, they might therefore be pretty slow. *)

val bind : ('a -> 'b t) -> 'a t -> 'b t
(** Use the result of a query to build another query and imediately run it. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Infix version of {!bind} *)

val return : 'a -> 'a t
(** Synonym to {!start} *)

val query_map : ('a -> 'b) -> 'a t -> 'b t
(** PMap results directly, rather than collections of elements *)

(** {6 Misc} *)

val lazy_ : 'a lazy_t t -> 'a t

(** {6 Output Containers} *)

val to_list : 'a collection t -> 'a list t
(** Build a list of results *)

val to_array : 'a collection t -> 'a array t
(** Build an array of results *)

val to_seq : 'a collection t  -> 'a sequence t
(** Build a (re-usable) sequence of elements, which can then be
    converted into other structures *)

val to_hashtbl : ('a * 'b) collection t -> ('a, 'b) Hashtbl.t t
(** Build a hashtable from the collection *)

val to_queue : 'a collection t -> ('a Queue.t -> unit) t

val to_stack : 'a collection t -> ('a Stack.t -> unit) t

(** {6 Adapters} *)

module AdaptSet(S : Set.S) : sig
  val of_set : S.t -> S.elt collection t
  val to_set : S.elt collection t -> S.t t
  val run : S.elt collection t -> S.t
end

module AdaptMap(M : Map.S) : sig
  val of_map : 'a M.t -> (M.key * 'a) collection t
  val to_pmap : 'a M.t -> (M.key, 'a) PMap.t
  val to_map : (M.key * 'a) collection t -> 'a M.t t
  val run : (M.key * 'a) collection t -> 'a M.t
end

module IO : sig
  val slurp : in_channel -> string t
  (** Slurp the whole channel in (blocking), returning the corresponding string *)

  val lines : string t -> string collection t
  (** Convert a string into a collection of lines *)

  val lines' : string t -> string list t
  (** Convert a string into a list of lines *)

  val out : out_channel -> string t -> unit
  val out_lines : out_channel -> string collection t -> unit
  (** Evaluate the query and print it line by line on the output *)
end
