
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

(** {1 LINQ-like operations on collections} *)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a hash = 'a -> int

type 'a collection
(** Abstract type of collections of objects of type 'a. Those cannot
    be used directly, they are to be processed using a query (type {!'a t})
    and converted to some list/sequence/array *)

(** {2 A polymorphic map} *)
module Map : sig
  type ('a, 'b) t

  val get : ('a,'b) t -> 'a -> 'b option

  val get_exn : ('a,'b) t -> 'a -> 'b
  (** Unsafe version of {!get}.
      @raise Not_found if the element is not present *)

  val size : (_,_) t -> int
  
  val to_seq : ('a, 'b) t -> ('a * 'b) sequence
end

(** {2 Query operators} *)

type 'a t
(** Type of a query that returns some value of type 'a *)

(** {6 Initial values} *)

val start : 'a -> 'a t
(** Start with a single value *)

val start_list : 'a list -> 'a collection t
(** Query that just returns the elements of the list *)

val start_array : 'a array -> 'a collection t

val start_hashtbl : ('a,'b) Hashtbl.t -> ('a * 'b) collection t

val start_seq : 'a sequence -> 'a collection t
(** Query that returns the elements of the given sequence. *)

(** {6 Execution} *)

val run : 'a t -> 'a
(** Execute the actual query *)

val run_no_opt : 'a t -> 'a
(** Execute the query, without optimizing it at all *)

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

val take : int -> 'a collection t -> 'a collection t
(** take at most [n] elements *)

val take_while : ('a -> bool) -> 'a collection t -> 'a collection t
(** take elements while they satisfy a predicate *)

val sort : cmp:'a ord -> 'a collection t -> 'a collection t
(** Sort items by the given comparison function *)

val distinct : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
               unit -> 'a collection t -> 'a collection t
(** Remove duplicate elements from the input collection.
    All elements in the result are distinct. *)

(** {6 Maps} *)

val get : 'a -> ('a, 'b) Map.t t -> 'b option t
(** Select a key from a map *)

val get_exn : 'a -> ('a, 'b) Map.t t -> 'b t
(** Unsafe version of {!get}.
    @raise Not_found if the key is not present. *)

val map_to_seq : ('a,'b) Map.t t -> ('a*'b) collection t
(** View a multimap as a proper collection *)

val map_to_seq_flatten : ('a,'b collection) Map.t t -> ('a*'b) collection t
(** View a multimap as a collection of individual key/value pairs *)

(** {6 Aggregation} *)

val group_by : ?cmp_key:'b ord -> ?cmp_val:'a ord ->
               ('a -> 'b) -> 'a collection t -> ('b,'a collection) Map.t t
(** [group_by f] takes a collection [c] as input, and returns
    a multimap [m] such that for each [x] in [c],
    [x] occurs in [m] under the key [f x]. In other words, [f] is used
    to obtain a key from [x], and [x] is added to the multimap using this key. *)

val count : ?cmp:'a ord -> unit -> 'a collection t -> ('a, int) Map.t t
(** [count c] returns a map from elements of [c] to the number
    of time those elements occur. *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a collection t -> 'b t
(** Fold over the collection *)

val size : _ collection t -> int t
(** Count how many elements the collection contains *)

val reduce : ('a -> 'b) -> ('a -> 'b -> 'b) -> ('b -> 'c) -> 'a collection t -> 'c option t
(** [reduce start mix stop q] uses [start] on the first element of [q],
    and combine the result with following elements using [mix]. The final
    value is transformed using [stop]. This returns [None] if the collection
    is empty *)

val reduce_exn : ('a -> 'b) -> ('a -> 'b -> 'b) -> ('b -> 'c) ->
                 'a collection t -> 'c t
(** Same as {!reduce} but fails on empty collections.
    @raise Invalid_argument if the collection is empty *)

val sum : int collection t -> int t

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
            merge:('key -> 'a -> 'b -> 'c) ->
            'a collection t -> 'b collection t -> 'c collection t
(** [join key1 key2 ~merge] is a binary operation
    that takes two collections [a] and [b], projects their
    elements resp. with [key1] and [key2], and combine
    values [(x,y)] from [(a,b)] with the same [key]
    using [merge]. *)

val group_join : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
                  ('b -> 'a) -> 'a collection t -> 'b collection t ->
                  ('a, 'b collection) Map.t t
(** [group_join key2] associates to every element [x] of
    the first collection, all the elements [y] of the second
    collection such that [eq x (key y)] *)

val product : 'a collection t -> 'b collection t -> ('a * 'b) collection t
(** Cartesian product *)

val append : 'a collection t -> 'a collection t -> 'a collection t
(** Append two collections together *)

val inter : ?cmp:'a ord -> unit ->
            'a collection t -> 'a collection t -> 'a collection t
(** Intersection of two collections. Each element will occur at most once
    in the result *)

val union : ?cmp:'a ord -> unit ->
            'a collection t -> 'a collection t -> 'a collection t
(** Union of two collections. Each element will occur at most once
    in the result *)

val diff : ?cmp:'a ord -> unit ->
            'a collection t -> 'a collection t -> 'a collection t
(** Set difference *)

(** {6 Tuple and Options} *)

(** Specialized projection operators *)

val fst : ('a * 'b) collection t -> 'a collection t
val snd : ('a * 'b) collection t -> 'b collection t

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
(** Map results directly, rather than collections of elements *)

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
