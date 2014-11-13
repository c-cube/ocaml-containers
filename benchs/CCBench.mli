
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

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption) however
caused and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
*)

(** {1 helpers for benchmarks} *)

(** {2 Benchmark Tree}

Naming benchmark within a hierarchy that allows to filter them *)

type bench

val throughput1 :
  ?min_count:Int64.t ->
  ?style:Benchmark.style ->
  ?fwidth:int ->
  ?fdigits:int ->
  ?repeat:int -> int -> ?name:string -> ('a -> 'b) -> 'a -> bench

val throughputN :
  ?style:Benchmark.style ->
  ?fwidth:int ->
  ?fdigits:int ->
  ?repeat:int -> int -> (string * ('a -> 'b) * 'a) list -> bench

val raw : (unit -> Benchmark.samples) -> bench
(** Give control to the user to produce her samples *)

val (>::) : string -> bench -> bench

val mk_list : bench list -> bench

val (>:::) : string -> bench list -> bench

val with_int : (int -> bench) -> int list -> bench
(** Parametrize a bench with several values *)

val map_int : ((int -> bench) * int) list -> bench
(** One function for each integer.
    @raise Invalid_argument if the two lists don't have the same length
      or are empty *)

val print : Format.formatter -> bench -> unit
(** Print the tree of benchmarks *)

(** {2 Path}

A path in a benchmark tree *)

type path = string list

val print_path : Format.formatter -> path -> unit

val parse_path : string -> path
(** split a string into a path at the "." separators *)

val prefix : path -> bench -> bench
(** Add the path as a prefix to the tree *)

(** {2 Running} *)

val run : Format.formatter -> bench -> unit
(** [run fmt t] runs all benchmarks of [t] and print the results to [fmt] *)

val run_path : Format.formatter -> bench -> path -> unit
(** Run only a sub-tree of the benchmarks *)

val run_main :
  ?argv:string array ->
  ?out:Format.formatter ->
  bench -> unit
(** Main function: parses the command line arguments and runs benchmarks
    accordingly *)


(** {2 Global Registration} *)

val register : ?path:path -> bench -> unit
(** Register a benchmark to the global register of benchmarks (a global tree) *)

val register' : path:string -> bench -> unit
(** Same as {!register} but applies {!parse_path} first to its argument *)

val global_bench : unit -> bench
(** Global bench tree, built from calls to {!register} *)

val run_main :
  ?argv:string array ->
  ?out:Format.formatter ->
  unit -> unit
  (** Same as {!run_main} but on the global tree of benchmarks *)
