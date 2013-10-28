
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

(** {1 Quickcheck inspired property-based testing} *)

(** The library takes inspiration from Haskell's QuickCheck library. The
rough idea is that the programer describes invariants that values of
a certain type need to satisfy ("properties"), as functions from this type
to bool. She also needs to desribe how to generate random values of the type,
so that the property is tried and checked on a number of random instances.

This explains the organization of this module:

- {! Arbitrary} is used to describe how to generate random values. An
  ['a Arbitrary.t] is a random generator of values of type 'a.
- {! Prop} is used to describe and combine properties. Especially interesting
  is [Prop.(==>)], that is such that [a ==> b] only checks the property [b]
  on a value [x] if [a x] holds (precondition).
- {! PP} describes a few combinators to print values. This is used when a
  property fails on some instances, to print the failing instances.

Then, a few functions are provided to test properties. Optional parameters
allow to specify the random generator, the printer for failing cases, the
number of instances to generate and test...


Examples:

    - List.rev is involutive:

{[
let test = QCheck.mk_test ~n:1000 QCheck.Arbitrary.(list alpha)
  (fun l -> List.rev (List.rev l) = l);;
QCheck.run test;;
]}
    - Not all lists are sorted (false property that will fail. The 15 smallest
      counter-example lists will be printed):

{[
let test = QCheck.(
  mk_test
    ~n:10_000 ~size:List.length ~limit:15 ~pp:QCheck.PP.(list int)
    QCheck.Arbitrary.(list small_int)
    (fun l -> l = List.sort compare l));;
QCheck.run test;;
]}


    - generate 20 random trees using {! Arbitrary.fix} :

{[type tree = Int of int | Node of tree list;;
 
 let ar = QCheck.Arbitrary.(fix ~max:10
  ~base:(map small_int (fun i -> Int i))
  (fun t st -> Node (list t st)));;

 Arbitrary.generate ~n:20 ar;;
 ]}
*)

(** {2 Description of how to generate arbitrary values for some type} *)
 
module Arbitrary : sig
  type 'a t = Random.State.t -> 'a
    (** A generator of arbitrary values of type 'a *)

  val return : 'a -> 'a t
    (** Return always the same value (e.g. [4]) *)

  val int : int -> int t
    (** Any integer between 0 (inclusive) and the given higher bound (exclusive) *)

  val int_range : start:int -> stop:int -> int t
    (* Integer range start .. stop-1 *)

  val (--) : int -> int -> int t
    (** Infix synonym for {!int_range} *)

  val small_int : int t
    (** Ints lower than 100 *)

  val split_int : int t -> (int * int) t
    (** [split_int gen] generates a number [n] from [gen], and
        returns [i, j] where [i + j = n] *)

  val bool : bool t
    (** Arbitrary boolean *)

  val char : char t
    (** A (printable) char *)

  val alpha : char t
    (** Alphabetic char *)

  val float : float -> float t
    (** Random float *)

  val string : string t
    (** Random strings of small length *)

  val string_len : int t -> string t
    (** String of random length *)

  val map : 'a t -> ('a -> 'b) -> 'b t
    (** Transform an arbitrary into another *)

  val list : ?len:int t -> 'a t -> 'a list t
    (** List of arbitrary length. Default [len] is between 0 and 10. *)

  val opt : 'a t -> 'a option t
    (** May return a value, or None *)

  val pair : 'a t -> 'b t -> ('a * 'b) t

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  val list_repeat : int -> 'a t -> 'a list t
    (** Lists of given length exactly *)

  val array : ?len:int t -> 'a t -> 'a array t
    (** Random array of random length *)

  val array_repeat : int -> 'a t -> 'a array t
    (** Random array of given length *)

  val among : 'a list -> 'a t
    (** Choose an element among those of the list *)

  val among_array : 'a array -> 'a t
    (** Choose in the array *)

  val choose : 'a t list -> 'a t
    (** Choice among combinations *)

  val fix : ?max:int -> base:'a t -> ('a t -> 'a t) -> 'a t
    (** Recursive arbitrary values. The optional value [max] defines
        the maximal depth, if needed (default 15). [base] is the base case. *)

  val fix_depth : depth:int t -> base:'a t -> ('a t -> 'a t) -> 'a t
    (** Recursive values of at most given random depth *)

  val lift : ('a -> 'b) -> 'a t -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    (** Monadic bind *)

  val retry : 'a option t -> 'a t
    (** Generate until a Some value is returned *)

  val generate : ?n:int -> ?rand:Random.State.t -> 'a t -> 'a list
    (** Generate [n] random values of the given type *)
end

(** {2 Pretty printing} *)

module PP : sig
  type 'a t = 'a -> string

  val int : int t
  val bool : bool t
  val float : float t
  val char : char t
  val string : string t

  val pair : 'a t -> 'b t -> ('a*'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a*'b*'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a*'b*'c*'d) t

  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t
end

(** {2 Testing} *)

module Prop : sig
  type 'a t = 'a -> bool

  val (==>) : ('a -> bool) -> 'a t -> 'a t
    (** Precondition for a test *)

  val assume : bool -> unit
    (** Assume the given precondition holds. A test won't fail if the
        precondition (the boolean argument) is false, but it will be
        discarded. Running tests counts how many instances were
        discarded for not satisfying preconditions. *)

  val assume_lazy : bool lazy_t -> unit
    (** Assume the given (lazy) precondition holds. See {!assume}. *)

  val (&&&) : 'a t -> 'a t -> 'a t
    (** Logical 'and' on tests *)

  val (|||) : 'a t -> 'a t -> 'a t
    (** Logical 'or' on tests *)

  val (!!!) : 'a t -> 'a t
    (** Logical 'not' on tests *)
end

type 'a result =
  | Ok of int * int  (** total number of tests / number of failed preconditions *)
  | Failed of 'a list (** Failed instances *)
  | Error of 'a option * exn  (** Error, and possibly instance that triggered it *)

val check : ?rand:Random.State.t -> ?n:int -> 
            'a Arbitrary.t -> 'a Prop.t -> 'a result
  (** Check that the property [prop] holds on [n] random instances of the type
      'a, as generated by the arbitrary instance [gen] *)

(** {2 Main} *)

type test
  (** A single property test *)

val mk_test : ?n:int -> ?pp:'a PP.t -> ?name:string ->
              ?size:('a -> int) -> ?limit:int ->
              'a Arbitrary.t -> 'a Prop.t -> test
  (** Construct a test. Optional parameters are the same as for {!run}.
      @param name is the name of the property that is checked
      @param pp is a pretty printer for failing instances
      @out is the channel to print results onto
      @n is the number of tests (default 100)
      @rand is the random generator to use
      @size is a size function on values on which tests are performed. If
        the test fails and a size function is given, the smallest 
        counter-examples with respect to [size] will be printed in priority.
      @limit maximal number of counter-examples that will get printed.
        Default is [10]. *)

val run : ?out:out_channel -> ?rand:Random.State.t -> test -> bool
  (** Run a test and print results *)

type suite = test list
  (** A test suite is a list of tests *)

val flatten : suite list -> suite

val run_tests : ?out:out_channel -> ?rand:Random.State.t -> suite -> bool
  (** Run a suite of tests, and print its results *)
