(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Lazy Tree Structure}
    This structure can be used to represent trees and directed
    graphs (as infinite trees) in a lazy fashion. Like {!CCKList}, it
    is a structural type. *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Basics} *)

type +'a t = unit -> [`Nil | `Node of 'a * 'a t list]

val empty : 'a t

val is_empty : _ t -> bool

val singleton : 'a -> 'a t
(** Tree with only one label. *)

val node : 'a -> 'a t list -> 'a t
(** Build a node from a label and a list of children. *)

val node1 : 'a -> 'a t -> 'a t
(** Node with one child. *)

val node2 : 'a -> 'a t -> 'a t -> 'a t
(** Node with two children. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold on values in no specified order. May not terminate if the
    tree is infinite. *)

val iter : ('a -> unit) -> 'a t -> unit

val size : _ t -> int
(** Number of elements. *)

val height : _ t -> int
(** Length of the longest path to empty leaves. *)

val map : ('a -> 'b) -> 'a t -> 'b t

val (>|=) : 'a t -> ('a -> 'b) -> 'b t

val cut_depth : int -> 'a t -> 'a t
(** Cut the tree at the given depth, so it becomes finite. *)

(** {2 Graph Traversals} *)

(** Abstract Set structure *)
class type ['a] pset = object
  method add : 'a -> 'a pset
  method mem : 'a -> bool
end

val set_of_cmp : cmp:('a -> 'a -> int) -> unit -> 'a pset
(** Build a set structure given a total ordering. *)

val dfs : pset:'a pset -> 'a t -> [ `Enter of 'a | `Exit of 'a ] klist
(** Depth-first traversal of the tree. *)

val bfs : pset:'a pset -> 'a t -> 'a klist
(** Breadth-first traversal of the tree. *)

val force : 'a t -> ([ `Nil | `Node of 'a * 'b list ] as 'b)
(** [force t] evaluates [t] completely and returns a regular tree
    structure.
    @since 0.13 *)

val find : pset:'a pset -> ('a -> 'b option) -> 'a t -> 'b option
(** Look for an element that maps to [Some _]. *)

(** {2 Pretty-printing}

    Example (tree of calls for naive Fibonacci function):
    {[
      let mk_fib n =
        let rec fib' l r i =
          if i=n then r else fib' r (l+r) (i+1)
        in fib' 1 1 1;;

      let rec fib n = match n with
        | 0 | 1 -> CCKTree.singleton (`Cst n)
        | _ -> CCKTree.node2 (`Plus (mk_fib n)) (fib (n-1)) (fib (n-2));;

      let pp_node fmt = function
        | `Cst n -> Format.fprintf fmt "%d" n
        | `Plus n -> Format.fprintf fmt "%d" n;;

      Format.printf "%a@." (CCKTree.pp pp_node) (fib 8);;
    ]}
*)

val pp : 'a printer -> 'a t printer
(** A pretty-printer using S-expressions and boxes to render the tree.
    Empty nodes are not rendered; sharing is ignored.
    @since 0.9 *)

(** {2 Pretty printing in the DOT (graphviz) format} *)

module Dot : sig
  type attribute = [
    | `Color of string
    | `Shape of string
    | `Weight of int
    | `Style of string
    | `Label of string
    | `Id of string  (** Unique ID in the graph. Allows sharing. *)
    | `Other of string * string
  ] (** Dot attributes for nodes *)

  type graph = (string * attribute list t list)
  (** A dot graph is a name, plus a list of trees labelled with attributes *)

  val mk_id : ('a, Buffer.t, unit, attribute) format4 -> 'a
  (** Using a formatter string, build an ID. *)

  val mk_label : ('a, Buffer.t, unit, attribute) format4 -> 'a
  (** Using a formatter string, build a label. *)

  val make : name:string -> attribute list t list -> graph

  val singleton : name:string -> attribute list t -> graph

  val pp_single : string -> attribute list t printer

  val pp : graph printer
  (** Printer to DOT with indentation, etc.
      @since 0.6.1 *)

  val print_to_file : string -> graph -> unit
  (** [print_to_file filename g] prints [g] into a file whose name
      is [filename].
      @since 0.6.1 *)

  val to_file : ?name:string -> string -> attribute list t list -> unit
  (** [to_file filename trees] makes a graph out of the trees, opens the
      file [filename] and prints the graph into the file.
      @param name name of the graph.
      @since 0.6.1 *)
end
