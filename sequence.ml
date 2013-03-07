(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
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

(** {1 Transient iterators, that abstract on a finite sequence of elements. *)

(** Sequence abstract iterator type *)
type 'a t = ('a -> unit) -> unit

(** Build a sequence from a iter function *)
let from_iter f = f

let empty = fun k -> ()

let singleton x = fun k -> k x

(** Infinite sequence of the same element *)
let repeat x = fun k -> while true do k x done

(** [iterate f x] is the infinite sequence (x, f(x), f(f(x)), ...) *)
let iterate f x =
  let rec iterate k x = k x; iterate k (f x) in
  from_iter (fun k -> iterate k x)

(** Sequence that calls the given function to produce elements *)
let forever f =
  let rec forever k = k (f ()); forever k in
  from_iter forever

(** Cycle forever through the given sequence. O(n). *)
let cycle s = fun k -> while true do s k; done

(** Consume the sequence, passing all its arguments to the function *)
let iter f seq = seq f

(** Iterate on elements and their index in the sequence *)
let iteri f seq =
  let r = ref 0 in
  let k x =
    f !r x;
    incr r
  in seq k

(** Fold over elements of the sequence, consuming it *)
let fold f init seq =
  let r = ref init in
  seq (fun elt -> r := f !r elt);
  !r

(** Fold over elements of the sequence and their index, consuming it *)
let foldi f init seq =
  let i = ref 0 in
  let r = ref init in
  seq (fun elt ->
    r := f !r !i elt;
    incr i);
  !r
    
(** Map objects of the sequence into other elements, lazily *)
let map f seq =
  let seq_fun' k = seq (fun x -> k (f x)) in
  seq_fun'

(** Map objects, along with their index in the sequence *)
let mapi f seq =
  let seq_fun' k =
    let i = ref 0 in
    seq (fun x -> k (f !i x); incr i) in
  seq_fun'

(** Filter on elements of the sequence *)
let filter p seq =
  let seq_fun' k = seq (fun x -> if p x then k x) in
  seq_fun'

(** Append two sequences *)
let append s1 s2 =
  let seq_fun k = s1 k; s2 k in
  seq_fun

(** Concatenate a sequence of sequences into one sequence *)
let concat s =
  from_iter (fun k ->
    (* function that is called on every sub-sequence *)
    let k_seq seq = iter k seq in
    s k_seq)

(** Monadic bind. It applies the function to every element of the
    initial sequence, and calls [concat]. *)
let flatMap f seq =
  from_iter
    (fun k -> seq (fun x -> (f x) k))

(** Insert the second element between every element of the sequence *)
let intersperse seq elem =
  from_iter
    (fun k -> seq (fun x -> k x; k elem))

(** Mutable unrolled list to serve as intermediate storage *)
module MList = struct
  type 'a t = {
    content : 'a array;   (* elements of the node *)
    mutable len : int;    (* number of elements in content *)
    mutable tl : 'a t;    (* tail *)
  } (** A list that contains some elements, and may point to another list *)

  let _empty () : 'a t = Obj.magic 0
    (** Empty list, for the tl field *)

  let make n =
    assert (n > 0);
    { content = Array.make n (Obj.magic 0);
      len = 0;
      tl = _empty ();
    }

  let rec is_empty l =
    l.len = 0 && (l.tl == _empty () || is_empty l.tl)

  let rec iter f l =
    for i = 0 to l.len - 1 do f l.content.(i); done;
    if l.tl != _empty () then iter f l.tl

  let iteri f l =
    let rec iteri i f l =
      for j = 0 to l.len - 1 do f (i+j) l.content.(j); done;
      if l.tl != _empty () then iteri (i+l.len) f l.tl
    in iteri 0 f l

  let rec iter_rev f l =
    (if l.tl != _empty () then iter_rev f l.tl);
    for i = l.len - 1 downto 0 do f l.content.(i); done

  let length l =
    let rec len acc l =
      if l.tl == _empty () then acc+l.len else len (acc+l.len) l.tl
    in len 0 l

  (** Get element by index *)
  let rec get l i =
    if i < l.len then l.content.(i)
    else if i >= l.len && l.tl == _empty () then raise (Invalid_argument "MList.get")
    else get l.tl (i - l.len)

  (** Push [x] at the end of the list. It returns the block in which the
      element is inserted. *)
  let rec push x l =
    if l.len = Array.length l.content
      then begin (* insert in the next block *)
        (if l.tl == _empty () then l.tl <- make (Array.length l.content));
        push x l.tl
      end else begin  (* insert in l *)
        l.content.(l.len) <- x;
        l.len <- l.len + 1;
        l
      end

  (** Reverse list (in place), and returns the new head *)
  let rev l =
    let rec rev prev l =
      (* reverse array *)
      for i = 0 to (l.len-1) / 2 do
        let x = l.content.(i) in
        l.content.(i) <- l.content.(l.len - i - 1);
        l.content.(l.len - i - 1) <- x;
      done;
      (* reverse next block *)
      let l' = l.tl in
      l.tl <- prev;
      if l' == _empty () then l else rev l l'
    in
    rev (_empty ()) l

  (** Build a MList of elements of the Seq. The optional argument indicates
      the size of the blocks *)
  let of_seq ?(size=64) seq =
    (* read sequence into a MList.t *)
    let start = make size in
    let l = ref start in
    seq (fun x -> l := push x !l);
    start
end

(** Iterate on the sequence, storing elements in a data structure.
    The resulting sequence can be iterated on as many times as needed. *)
let persistent (seq : 'a t) : 'a t =
  let l = MList.of_seq seq in
  from_iter (fun k -> MList.iter k l)

(** Cartesian product of the sequences. *)
let product outer inner =
  let outer = persistent outer in
  from_iter
    (fun k ->
      outer (fun x ->
        inner (fun y -> k (x,y))))

(** [unfoldr f b] will apply [f] to [b]. If it
    yields [Some (x,b')] then [x] is returned
    and unfoldr recurses with [b']. *)
let unfoldr f b =
  let rec unfold k b = match f b with
    | None -> ()
    | Some (x, b') -> k x; unfold k b'
  in
  from_iter (fun k -> unfold k b)

(** Max element of the sequence, using the given comparison
    function. A default element has to be provided. *)
let max ?(lt=fun x y -> x < y) seq m =
  fold (fun m x -> if lt m x then x else m) m seq

(** Min element of the sequence, using the given comparison function *)
let min ?(lt=fun x y -> x < y) seq m =
  fold (fun m x -> if lt x m then x else m) m seq

exception ExitSequence

(** Take at most [n] elements from the sequence *)
let take n seq =
  let count = ref 0 in
  fun k ->
    try
      seq
        (fun x -> if !count < n then begin incr count; k x end
                                else raise ExitSequence)
    with ExitSequence -> ()

(** Drop the [n] first elements of the sequence *)
let drop n seq =
  let count = ref 0 in
  fun k -> seq
    (fun x -> if !count >= n then k x else incr count)

(** Reverse the sequence. O(n) memory. *)
let rev seq =
  let l = MList.of_seq seq in
  from_iter (fun k -> MList.iter_rev k l)

(** Do all elements satisfy the predicate? *)
let for_all p seq =
  try
    seq (fun x -> if not (p x) then raise ExitSequence);
    true
  with ExitSequence -> false

(** Exists there some element satisfying the predicate? *)
let exists p seq =
  try
    seq (fun x -> if p x then raise ExitSequence);
    false
  with ExitSequence -> true

(** How long is the sequence? *)
let length seq =
  let r = ref 0 in
  seq (fun _ -> incr r);
  !r

(** Is the sequence empty? *)
let is_empty seq =
  try seq (fun _ -> raise ExitSequence); true
  with ExitSequence -> false

let to_list seq = List.rev (fold (fun y x -> x::y) [] seq)

let to_rev_list seq = fold (fun y x -> x :: y) [] seq
  (** Get the list of the reversed sequence (more efficient) *)

let of_list l = from_iter (fun k -> List.iter k l)

let to_array seq =
  let l = MList.of_seq seq in
  let n = MList.length l in
  if n = 0
    then [||]
    else begin
      let a = Array.make n (MList.get l 0) in
      MList.iteri (fun i x -> a.(i) <- x) l;
      a
    end

let of_array a = from_iter (fun k -> Array.iter k a)

let of_array_i a =
  let seq k =
    for i = 0 to Array.length a - 1 do k (i, a.(i)) done
  in from_iter seq

(** [array_slice a i j] Sequence of elements whose indexes range
    from [i] to [j] *)
let array_slice a i j =
  assert (i >= 0 && j < Array.length a);
  fun k ->
    for idx = i to j do
      k a.(idx);  (* iterate on sub-array *)
    done

(** Sequence of elements of a stream (usable only once) *)
let of_stream s =
  let seq k = Stream.iter k s in
  from_iter seq

(** Convert to a stream. The sequence is made persistent. *)
let to_stream seq =
  let l = ref (MList.of_seq seq) in
  let i = ref 0 in
  let rec get_next () =
    if !l == MList._empty () then None
    else if (!l).MList.len = !i then (l := (!l).MList.tl; i := 0; get_next ())
    else let x = (!l).MList.content.(!i) in (incr i; Some x)
  in
  Stream.from (fun _ -> get_next ())

(** Push elements of the sequence on the stack *)
let to_stack s seq = iter (fun x -> Stack.push x s) seq

(** Sequence of elements of the stack (same order as [Stack.iter]) *)
let of_stack s = from_iter (fun k -> Stack.iter k s)

(** Push elements of the sequence into the queue *)
let to_queue q seq = iter (fun x -> Queue.push x q) seq

(** Sequence of elements contained in the queue, FIFO order *)
let of_queue q = from_iter (fun k -> Queue.iter k q)

let hashtbl_add h seq =
  iter (fun (k,v) -> Hashtbl.add h k v) seq

let hashtbl_replace h seq =
  iter (fun (k,v) -> Hashtbl.replace h k v) seq

let to_hashtbl seq =
  let h = Hashtbl.create 3 in
  hashtbl_replace h seq;
  h

let of_hashtbl h =
  from_iter (fun k -> Hashtbl.iter (fun a b -> k (a, b)) h)

let hashtbl_keys h =
  from_iter (fun k -> Hashtbl.iter (fun a b -> k a) h)

let hashtbl_values h =
  from_iter (fun k -> Hashtbl.iter (fun a b -> k b) h)

let of_str s = from_iter (fun k -> String.iter k s)
    
let to_str seq =
  let b = Buffer.create 64 in
  iter (fun c -> Buffer.add_char b c) seq;
  Buffer.contents b

let of_in_channel ic =
  from_iter (fun k ->
    try while true do
      let c = input_char ic in k c
    done with End_of_file -> ())

(** Copy content of the sequence into the buffer *)
let to_buffer seq buf =
  iter (fun c -> Buffer.add_char buf c) seq

(** Iterator on integers in [start...stop] by steps 1 *)
let int_range ~start ~stop =
  fun k ->
    for i = start to stop do k i done

(** Convert the given set to a sequence. The set module must be provided. *)
let of_set (type s) (type v) m set =
  let module S = (val m : Set.S with type t = s and type elt = v) in
  from_iter
    (fun k -> S.iter k set)

(** Convert the sequence to a set, given the proper set module *)
let to_set (type s) (type v) m seq =
  let module S = (val m : Set.S with type t = s and type elt = v) in
  fold
    (fun set x -> S.add x set)
    S.empty seq

(** {2 Functorial conversions between sets and sequences} *)

module Set = struct
  module type S = sig
    type set
    include Set.S with type t := set
    val of_seq : elt t -> set
    val to_seq : set -> elt t
  end

  (** Create an enriched Set module from the given one *)
  module Adapt(X : Set.S) : S with type elt = X.elt and type set = X.t = struct
    type set = X.t

    let to_seq set = from_iter (fun k -> X.iter k set)

    let of_seq seq = fold (fun set x -> X.add x set) X.empty seq

    include X
  end
    
  (** Functor to build an extended Set module from an ordered type *)
  module Make(X : Set.OrderedType) : S with type elt = X.t = struct
    module MySet = Set.Make(X)
    include Adapt(MySet)
  end
end

(** {2 Conversion between maps and sequences.} *)

module Map = struct
  module type S = sig
    type +'a map
    include Map.S with type 'a t := 'a map
    val to_seq : 'a map -> (key * 'a) t
    val of_seq : (key * 'a) t -> 'a map
    val keys : 'a map -> key t
    val values : 'a map -> 'a t
  end

  (** Adapt a pre-existing Map module to make it sequence-aware *)
  module Adapt(M : Map.S) : S with type key = M.key and type 'a map = 'a M.t = struct
    type 'a map = 'a M.t

    let to_seq m = from_iter (fun k -> M.iter (fun x y -> k (x,y)) m)

    let of_seq seq = fold (fun m (k,v) -> M.add k v m) M.empty seq

    let keys m = from_iter (fun k -> M.iter (fun x _ -> k x) m)

    let values m = from_iter (fun k -> M.iter (fun _ y -> k y) m)

    include M
  end

  (** Create an enriched Map module, with sequence-aware functions *)
  module Make(V : Map.OrderedType) : S with type key = V.t = struct
    module M = Map.Make(V)
    include Adapt(M)
  end
end

(** {2 Infinite sequences of random values} *)

let random_int bound = forever (fun () -> Random.int bound)

let random_bool = forever Random.bool

let random_float bound = forever (fun () -> Random.float bound)

(** Sequence of choices of an element in the array *)
let random_array a =
  assert (Array.length a > 0);
  let seq k =
    while true do
      let i = Random.int (Array.length a) in
      k a.(i);
    done in
  from_iter seq

let random_list l = random_array (Array.of_list l)

(** {2 Type-classes} *)

module TypeClass = struct
  (** {3 Classes} *)
  type ('a,'b) sequenceable = {
    to_seq : 'b -> 'a t;
    of_seq : 'a t -> 'b;
  }

  type ('a,'b) addable = {
    empty : 'b;
    add : 'b -> 'a -> 'b;
  }

  type 'a monoid = ('a,'a) addable

  type ('a,'b) iterable = {
    iter : ('a -> unit) -> 'b -> unit;
  }

  (** {3 Instances} *)

  let (sequenceable : ('a,'a t) sequenceable) = {
    to_seq = (fun seq -> seq);
    of_seq = (fun seq -> seq);
  }

  let (iterable : ('a, 'a t) iterable) = {
    iter = (fun f seq -> iter f seq);
  }

  let (monoid : 'a t monoid) = {
    empty = empty;
    add = (fun s1 s2 -> append s1 s2);
  }

  (** {3 Conversions} *)

  let of_iterable iterable x =
    from_iter (fun k -> iterable.iter k x)


  let to_addable addable seq =
    fold addable.add addable.empty seq
end


(** {2 Pretty printing of sequences} *)

(** Pretty print a sequence of ['a], using the given pretty printer
    to print each elements. An optional separator string can be provided. *)
let pp_seq ?(sep=", ") pp_elt formatter seq =
  let first = ref true in
  iter
    (fun x -> 
      (if !first then first := false else Format.pp_print_string formatter sep);
      pp_elt formatter x;
      Format.pp_print_cut formatter ())
    seq
