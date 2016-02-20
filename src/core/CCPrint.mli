
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Printer Combinators}

This module provides combinators to build printers for user-defined types.
It doesn't try to do {b pretty}-printing (see for instance Pprint for this),
but a simple way to print complicated values without writing a lot of code.

Those combinators work with "%a". For instance to print a
[(int * bool) list list] and a [float array], one can write:
{[
  CCPrint.(printf "int: %d list: %a, array: %a\n"
    42
    (list (list (pair int bool))) [[1, true; 2, false]; [4, true]]
    (array float) [| 1. ; 2. ; 3e18 |] ;;
]}

Remember that "%a" in this context requires two arguments:
  - a value of type ['a t] (buffer printer)
  - a value of type ['a] (value to print)

To define new printers, one can either use existing ones (e.g. [list int]),
or use {!Printf.bprintf}. For instance a printer for colored points in 2D:

{[ type point = {x:int; y:int; colors: string list};;

let pp_point buf p =
  Printf.bprintf buf "{x=%d, y=%d, colors=%a}"
    p.x p.y CCPrint.(list string) p.colors;;
]}
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
val char : char t
(** @since 0.14 *)

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
  (** Print into a string *)

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
(** Example:
{[ module PrintLwt = CCPrint.MakeIO(struct
    include Lwt
    type output = Lwt_io.output_channel
    let write = Lwt_io.write
  end);;

  PrintLwt.printl Lwt_io.stdout (CCList.pp CCInt.pp) [1;2;3;4];;
  - : unit Lwt.t
]} *)
