(** Growable, mutable vector *)

type 'a t
  (** the type of a vector of 'a *)

val create : int -> 'a t
  (** create a vector of given initial capacity *)

val clear : 'a t -> unit
  (** clear the content of the vector *)

val is_empty : 'a t -> bool
  (** is the vector empty? *)

val push : 'a t -> 'a -> unit
  (** add an element at the end of the vector *)

val append : 'a t -> 'a t -> unit
  (** [append a b] adds all elements of b to a *)

val append_array : 'a t -> 'a array -> unit
  (** same as append, with an array *)

val pop : 'a t -> 'a
  (** remove last element, or raise a Failure if empty *)

val copy : 'a t -> 'a t
  (** shallow copy *)

val shrink : 'a t -> int -> unit
  (** shrink to the given size (remove elements above this size) *)

val member : ?cmp:('a -> 'a -> bool) -> 'a t -> 'a -> bool
  (** is the element a member of the vector? *)

val sort : ?cmp:('a -> 'a -> int) -> 'a t -> unit
  (** sort the array in place*)

val uniq_sort : ?cmp:('a -> 'a -> int) -> 'a t -> unit
  (** sort the array and remove duplicates in place*)

val iter : 'a t -> ('a -> unit) -> unit
  (** iterate on the vector *)

val iteri : 'a t -> (int -> 'a -> unit) -> unit
  (** iterate on the vector with indexes *)

val map : 'a t -> ('a -> 'b) -> 'b t
  (** map elements of the vector *)

val filter : 'a t -> ('a -> bool) -> 'a t
  (** filter elements from vector *)

val fold : 'a t -> 'b -> ('b -> 'a -> 'b) -> 'b
  (** fold on elements of the vector *)

val exists : 'a t -> ('a -> bool) -> bool
  (** existential test *)

val for_all : 'a t -> ('a -> bool) -> bool
  (** universal test *)

val find : 'a t -> ('a -> bool) -> 'a
  (** find an element that satisfies the predicate, or Not_found *)

val get : 'a t -> int -> 'a
  (** access element, or Failure if bad index *)

val set : 'a t -> int -> 'a -> unit
  (** access element, or Failure if bad index *)

val size : 'a t -> int
  (** number of elements in vector *)

val from_array : 'a array -> 'a t
val from_list : 'a list -> 'a t
val to_array : 'a t -> 'a array
val get_array : 'a t -> 'a array  (*  get underlying *shared* array *)
val to_list : 'a t -> 'a list

