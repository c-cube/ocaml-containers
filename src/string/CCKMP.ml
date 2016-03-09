
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Knuth-Morris-Pratt} *)

module type STRING = sig
  type t
  type char

  val length : t -> int
  val get : t -> int -> char
  val char_equal : char -> char -> bool
end

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit

module type S = sig
  type string

  type pattern
  (** Compiled pattern (needle: string to search in another string) *)

  val compile : string -> pattern
  (** Compile a string into a pattern *)

  val find : pattern:pattern -> string -> int -> int option
  (** [find ~pattern s i] finds the next occurrence of [pattern]
      in [s] starting at offset [i], and returns it,
      or returns [None] if the pattern doesn't occur. *)

  val search : pattern:pattern -> string -> int option
  (** [search ~pattern s] is a shortcut for [find ~pattern s 0]. *)

  val find_all : pattern:pattern -> string -> int -> int gen
  (** Generator on all occurrences of the pattern *)

  val seq : pattern:pattern -> string -> int -> int sequence
  (** Iterate on matching positions *)

  (** {6 One-shot functions that compile the pattern on-the-fly} *)

  val search' : pattern:string -> string -> int option

  val find_all' : pattern:string -> string -> int gen

  val seq' : pattern:string -> string -> int sequence
end

module Make(Str : STRING) = struct
  type string = Str.t
  type pattern = {
    failure : int array;
    str : Str.t;
    len : int;  (* = length str = length failure *)
  }

  let compile str =
    let len = Str.length str in
    match len with
    | 0 -> {failure=[| |]; len; str;}
    | 1 -> {failure=[| -1 |]; len; str;}
    | _ ->
      (* at least 2 elements, the algorithm can work *)
      let failure = Array.make len 0 in
      (* i: current index in str *)
      let i = ref 1 in
      (* j: index of candidate substring *)
      let j = ref 0 in
      while !i < len-1 do
        match !j with
        | _ when Str.char_equal (Str.get str !i) (Str.get str !j) ->
          (* substring starting at !j continues matching current char *)
          i := !i+1;
          j := !j+1;
          failure.(!i) <- !j;
        | 0 ->
          (* back to the beginning *)
          i := !i+1;
          failure.(!i) <- 0;
        | _ ->
          (* fallback for the prefix string *)
          assert (!j > 0);
          j := failure.(!j)
      done;
      { failure; str; len; }

  let find ~pattern s idx =
    (* proper search function.
      [i] index in [s]
      [j] index in [pattern]
      [len] length of [s] *)
    let len = Str.length s in
    let i = ref idx in
    let j = ref 0 in
    while !i < len && !j < pattern.len do
      let c = Str.get s !i in
      let expected = Str.get pattern.str !j in
      if Str.char_equal c expected
      then (
        (* char matches *)
        i := !i + 1; j := !j + 1
      ) else
        if !j=0
          then (* beginning of the pattern *)
            i := !i + 1
          else (* follow the failure link *)
            j := pattern.failure.(!j)
    done;
    if !j = pattern.len
      then Some (!i-pattern.len)
      else None

  let search ~pattern s = find ~pattern s 0

  let find_all ~pattern s i =
    let i = ref i in
    fun () ->
      if !i >= Str.length s
      then None
      else match find ~pattern s !i with
      | None -> None
      | (Some j) as res ->
          i := j + pattern.len;
          res

  let seq ~pattern s i k =
    let rec iter i =
      match find ~pattern s i with
      | None -> ()
      | Some j ->
          k j;
          iter (j+pattern.len)
    in
    iter i

  let search' ~pattern s =
    search ~pattern:(compile pattern) s

  let find_all' ~pattern s =
    find_all ~pattern:(compile pattern) s 0

  let seq' ~pattern s =
    seq ~pattern:(compile pattern) s 0
end

include  Make(struct
  type char_ = char
  type char = char_
  type t = string
  let char_equal a b = a=b
  let get = String.get
  let length = String.length
end)
