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

(** {1 Transient iterators, that abstract on a finite sequence of elements.} *)

(** Sequence abstract iterator type *)
type 'a t = ('a -> unit) -> unit

type 'a sequence = 'a t

type (+'a, +'b) t2 = ('a -> 'b -> unit) -> unit
  (** Sequence of pairs of values of type ['a] and ['b]. *)

(** Build a sequence from a iter function *)
let from_iter f = f

let rec from_fun f k = match f () with
  | None -> ()
  | Some x -> k x; from_fun f k

let empty k = ()

let singleton x k = k x
let return x k = k x
let pure f k = k f

let doubleton x y k = k x; k y

let cons x l k = k x; l k
let snoc l x k = l k; k x

let repeat x k = while true do k x done

let rec iterate f x k =
  k x;
  iterate f (f x) k

let rec forever f k =
  k (f ());
  forever f k

let cycle s k = while true do s k; done

let iter f seq = seq f

let iteri f seq =
  let r = ref 0 in
  seq
    (fun x ->
      f !r x;
      incr r)

let fold f init seq =
  let r = ref init in
  seq (fun elt -> r := f !r elt);
  !r

let foldi f init seq =
  let i = ref 0 in
  let r = ref init in
  seq
    (fun elt ->
      r := f !r !i elt;
      incr i);
  !r

let map f seq k = seq (fun x -> k (f x))

let mapi f seq k =
  let i = ref 0 in
  seq (fun x -> k (f !i x); incr i)

let filter p seq k = seq (fun x -> if p x then k x)

let append s1 s2 k = s1 k; s2 k

let concat s k = s (fun s' -> s' k)

let flatten s = concat s

let flatMap f seq k = seq (fun x -> f x k)

let flat_map = flatMap

let fmap f seq k =
  seq (fun x -> match f x with
      | None -> ()
      | Some y -> k y
      )

let filter_map = fmap

let intersperse elem seq k =
  let first = ref true in
  seq (fun x -> (if !first then first := false else k elem); k x)

(** Mutable unrolled list to serve as intermediate storage *)
module MList = struct
  type 'a node =
    | Nil
    | Cons of 'a array * int ref * 'a node ref

  (* build and call callback on every element *)
  let of_seq_with seq k =
    let start = ref Nil in
    let chunk_size = ref 8 in
    (* fill the list. prev: tail-reference from previous node *)
    let prev, cur = ref start, ref Nil in
    seq
      (fun x ->
        k x;  (* callback *)
        match !cur with
        | Nil ->
          let n = !chunk_size in
          if n < 4096 then chunk_size := 2 * !chunk_size;
          cur := Cons (Array.make n x, ref 1, ref Nil)
        | Cons (a,n,next) ->
          assert (!n < Array.length a);
          a.(!n) <- x;
          incr n;
          if !n = Array.length a then begin
            !prev := !cur;
            prev := next;
            cur := Nil
          end
      );
    !prev := !cur;
    !start

  let of_seq seq =
    of_seq_with seq (fun _ -> ())

  let is_empty = function
    | Nil -> true
    | Cons _ -> false

  let rec iter f l = match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
        for i=0 to !n - 1 do f a.(i) done;
        iter f !tl

  let iteri f l =
    let rec iteri i f l = match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
        for j=0 to !n - 1 do f (i+j) a.(j) done;
        iteri (i+ !n) f !tl
    in iteri 0 f l

  let rec iter_rev f l = match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
        iter_rev f !tl;
        for i = !n-1 downto 0 do f a.(i) done

  let length l =
    let rec len acc l = match l with
      | Nil -> acc
      | Cons (_, n, tl) -> len (acc+ !n) !tl
    in len 0 l

  (** Get element by index *)
  let rec get l i = match l with
    | Nil -> raise (Invalid_argument "MList.get")
    | Cons (a, n, _) when i < !n -> a.(i)
    | Cons (_, n, tl) -> get !tl (i- !n)

  let to_seq l k = iter k l

  let _to_next arg l =
    let cur = ref l in
    let i = ref 0 in (* offset in cons *)
    let rec get_next _ = match !cur with
      | Nil -> None
      | Cons (_, n, tl) when !i = !n ->
          cur := !tl;
          i := 0;
          get_next arg
      | Cons (a, n, _) ->
          let x = a.(!i) in
          incr i;
          Some x
    in get_next

  let to_gen l = _to_next () l

  let to_stream l =
    Stream.from (_to_next 42 l)  (* 42=magic cookiiiiiie *)

  let to_klist l =
    let rec make (l,i) () = match l with
      | Nil -> `Nil
      | Cons (_, n, tl) when i = !n -> make (!tl,0) ()
      | Cons (a, n, _) -> `Cons (a.(i), make (l,i+1))
    in make (l,0)
end

let persistent seq =
  let l = MList.of_seq seq in
  MList.to_seq l

type 'a lazy_state =
  | LazySuspend
  | LazyCached of 'a t

let persistent_lazy (seq:'a t) =
  let r = ref LazySuspend in
  fun k ->
    match !r with
    | LazyCached seq' -> seq' k
    | LazySuspend ->
        (* here if this traversal is interruted, no caching occurs *)
        let seq' = MList.of_seq_with seq k in
        r := LazyCached (MList.to_seq seq')

let sort ?(cmp=Pervasives.compare) seq =
  (* use an intermediate list, then sort the list *)
  let l = fold (fun l x -> x::l) [] seq in
  let l = List.fast_sort cmp l in
  fun k -> List.iter k l

let group ?(eq=fun x y -> x = y) seq k =
  let cur = ref [] in
  seq (fun x ->
    match !cur with
    | [] -> cur := [x]
    | (y::_) as l when eq x y ->
      cur := x::l  (* [x] belongs to the group *)
    | (_::_) as l ->
      k l; (* yield group, and start another one *)
      cur := [x]);
  (* last list *)
  if !cur <> [] then k !cur

let uniq ?(eq=fun x y -> x = y) seq k =
  let has_prev = ref false
  and prev = ref (Obj.magic 0) in  (* avoid option type, costly *)
  seq (fun x ->
    if !has_prev && eq !prev x
      then ()  (* duplicate *)
      else begin
        has_prev := true;
        prev := x;
        k x
      end)

let sort_uniq (type elt) ?(cmp=Pervasives.compare) seq =
  let module S = Set.Make(struct
    type t = elt
    let compare = cmp
  end) in
  let set = fold (fun acc x -> S.add x acc) S.empty seq in
  fun k -> S.iter k set

let product outer inner k =
  outer (fun x ->
    inner (fun y -> k (x,y))
  )

let product2 outer inner k =
  outer (fun x ->
    inner (fun y -> k x y)
  )

let join ~join_row s1 s2 k =
  s1 (fun a ->
    s2 (fun b ->
      match join_row a b with
      | None -> ()
      | Some c -> k c
      )
    )  (* yield the combination of [a] and [b] *)

let rec unfoldr f b k = match f b with
  | None -> ()
  | Some (x, b') ->
      k x;
      unfoldr f b' k

let scan f acc seq k =
  k acc;
  let acc = ref acc in
  seq (fun elt -> let acc' = f !acc elt in k acc'; acc := acc')

let max ?(lt=fun x y -> x < y) seq =
  let ret = ref None in
  seq (fun x -> match !ret with
    | None -> ret := Some x
    | Some y -> if lt y x then ret := Some x);
  !ret

let min ?(lt=fun x y -> x < y) seq =
  let ret = ref None in
  seq (fun x -> match !ret with
    | None -> ret := Some x
    | Some y -> if lt x y then ret := Some x);
  !ret

exception ExitSequence

let head seq =
  let r = ref None in
  try
    seq (fun x -> r := Some x; raise ExitSequence); None
  with ExitSequence -> !r

let head_exn seq =
  match head seq with
  | None -> invalid_arg "Sequence.head_exn"
  | Some x -> x

let take n seq k =
  let count = ref 0 in
  try
    seq (fun x ->
      incr count;
      k x;
      if !count = n then raise ExitSequence
    )
  with ExitSequence -> ()

let take_while p seq k =
  try
    seq (fun x -> if p x then k x else raise ExitSequence)
  with ExitSequence -> ()

let drop n seq k =
  let count = ref 0 in
  seq (fun x -> if !count >= n then k x else incr count)

let drop_while p seq k =
  let drop = ref true in
  seq (fun x ->
    if !drop
    then if p x then () else (drop := false; k x)
    else k x)

let rev seq =
  let l = MList.of_seq seq in
  fun k -> MList.iter_rev k l

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

let mem ?(eq=(=)) x seq = exists (eq x) seq

let find f seq =
  let r = ref None in
  begin try
    seq (fun x -> match f x with
      | None -> ()
      | Some _ as res -> r := res
    );
  with ExitSequence -> ()
  end;
  !r

let length seq =
  let r = ref 0 in
  seq (fun _ -> incr r);
  !r

let is_empty seq =
  try seq (fun _ -> raise ExitSequence); true
  with ExitSequence -> false

(** {2 Transform a sequence} *)

let empty2 k = ()

let is_empty2 seq2 =
  try ignore (seq2 (fun _ _ -> raise ExitSequence)); true
  with ExitSequence -> false

let length2 seq2 =
  let r = ref 0 in
  seq2 (fun _ _ -> incr r);
  !r

let zip seq2 k = seq2 (fun x y -> k (x,y))

let unzip seq k = seq (fun (x,y) -> k x y)

let zip_i seq k =
  let r = ref 0 in
  seq (fun x -> let n = !r in incr r; k n x)

let fold2 f acc seq2 =
  let acc = ref acc in
  seq2 (fun x y -> acc := f !acc x y);
  !acc

let iter2 f seq2 = seq2 f

let map2 f seq2 k = seq2 (fun x y -> k (f x y))

let map2_2 f g seq2 k =
  seq2 (fun x y -> k (f x y) (g x y))

(** {2 Basic data structures converters} *)

let to_list seq = List.rev (fold (fun y x -> x::y) [] seq)

let to_rev_list seq = fold (fun y x -> x :: y) [] seq

let to_opt = head

let of_opt o k = match o with
  | None -> ()
  | Some x -> k x

let of_list l k = List.iter k l

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

let of_array a k =
  for i = 0 to Array.length a - 1 do
    k (Array.unsafe_get a i)
  done

let of_array_i a k =
  for i = 0 to Array.length a - 1 do
    k (i, Array.unsafe_get a i)
  done

let of_array2 a k =
  for i = 0 to Array.length a - 1 do
    k i (Array.unsafe_get a i)
  done

let array_slice a i j k =
  assert (i >= 0 && j < Array.length a);
  for idx = i to j do
    k a.(idx);  (* iterate on sub-array *)
  done

let of_stream s k = Stream.iter k s

let to_stream seq =
  let l = MList.of_seq seq in
  MList.to_stream l

let to_stack s seq = iter (fun x -> Stack.push x s) seq

let of_stack s k = Stack.iter k s

let to_queue q seq = seq (fun x -> Queue.push x q)

let of_queue q k = Queue.iter k q

let hashtbl_add h seq =
  seq (fun (k,v) -> Hashtbl.add h k v)

let hashtbl_replace h seq =
  seq (fun (k,v) -> Hashtbl.replace h k v)

let to_hashtbl seq =
  let h = Hashtbl.create 3 in
  hashtbl_replace h seq;
  h

let to_hashtbl2 seq2 =
  let h = Hashtbl.create 3 in
  seq2 (fun k v -> Hashtbl.replace h k v);
  h

let of_hashtbl h k = Hashtbl.iter (fun a b -> k (a, b)) h

let of_hashtbl2 h k = Hashtbl.iter k h

let hashtbl_keys h k = Hashtbl.iter (fun a b -> k a) h

let hashtbl_values h k = Hashtbl.iter (fun a b -> k b) h

let of_str s k = String.iter k s

let to_str seq =
  let b = Buffer.create 64 in
  iter (fun c -> Buffer.add_char b c) seq;
  Buffer.contents b

let concat_str seq =
  let b = Buffer.create 64 in
  iter (Buffer.add_string b) seq;
  Buffer.contents b

exception OneShotSequence

let of_in_channel ic =
  let first = ref true in
  fun k ->
    if not !first
    then raise OneShotSequence
    else (
      first := false;
      try
        while true do
          let c = input_char ic in k c
        done
      with End_of_file -> ()
    )

let to_buffer seq buf =
  seq (fun c -> Buffer.add_char buf c)

(** Iterator on integers in [start...stop] by steps 1 *)
let int_range ~start ~stop k =
  for i = start to stop do k i done

let int_range_dec ~start ~stop k =
  for i = start downto stop do k i done

let of_set (type s) (type v) m set =
  let module S = (val m : Set.S with type t = s and type elt = v) in
  fun k -> S.iter k set

let to_set (type s) (type v) m seq =
  let module S = (val m : Set.S with type t = s and type elt = v) in
  fold
    (fun set x -> S.add x set)
    S.empty seq

type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

let of_gen g =
  (* consume the generator to build a MList *)
  let rec iter1 k = match g () with
    | None -> ()
    | Some x -> k x; iter1 k
  in
  let l = MList.of_seq iter1 in
  MList.to_seq l

let to_gen seq =
  let l = MList.of_seq seq in
  MList.to_gen l

let rec of_klist l k = match l() with
  | `Nil -> ()
  | `Cons (x,tl) -> k x; of_klist tl k

let to_klist seq =
  let l = MList.of_seq seq in
  MList.to_klist l

(** {2 Functorial conversions between sets and sequences} *)

module Set = struct
  module type S = sig
    include Set.S
    val of_seq : elt sequence -> t
    val to_seq : t -> elt sequence
    val to_list : t -> elt list
    val of_list : elt list -> t
  end

  (** Create an enriched Set module from the given one *)
  module Adapt(X : Set.S) = struct
    let to_seq set k = X.iter k set

    let of_seq seq = fold (fun set x -> X.add x set) X.empty seq

    let of_list l = of_seq (of_list l)

    let to_list set = to_list (to_seq set)

    include X
  end

  (** Functor to build an extended Set module from an ordered type *)
  module Make(X : Set.OrderedType) = struct
    module MySet = Set.Make(X)
    include Adapt(MySet)
  end
end

(** {2 Conversion between maps and sequences.} *)

module Map = struct
  module type S = sig
    include Map.S
    val to_seq : 'a t -> (key * 'a) sequence
    val of_seq : (key * 'a) sequence -> 'a t
    val keys : 'a t -> key sequence
    val values : 'a t -> 'a sequence
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
  end

  (** Adapt a pre-existing Map module to make it sequence-aware *)
  module Adapt(M : Map.S) = struct
    let to_seq m = from_iter (fun k -> M.iter (fun x y -> k (x,y)) m)

    let of_seq seq = fold (fun m (k,v) -> M.add k v m) M.empty seq

    let keys m = from_iter (fun k -> M.iter (fun x _ -> k x) m)

    let values m = from_iter (fun k -> M.iter (fun _ y -> k y) m)

    let of_list l = of_seq (of_list l)

    let to_list x = to_list (to_seq x)

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

let random_array a k =
  assert (Array.length a > 0);
  while true do
    let i = Random.int (Array.length a) in
    k a.(i);
  done

let random_list l = random_array (Array.of_list l)

(** {2 Infix functions} *)

module Infix = struct
  let (--) i j = int_range ~start:i ~stop:j

  let (--^) i j = int_range_dec ~start:i ~stop:j

  let (>>=) x f = flat_map f x

  let (>|=) x f = map f x

  let (<*>) funs args k =
    funs (fun f -> args (fun x -> k (f x)))

  let (<+>) = append
end

include Infix

(** {2 Pretty printing of sequences} *)

(** Pretty print a sequence of ['a], using the given pretty printer
    to print each elements. An optional separator string can be provided. *)
let pp_seq ?(sep=", ") pp_elt formatter seq =
  let first = ref true in
  seq
    (fun x ->
      (if !first then first := false
        else begin
          Format.pp_print_string formatter sep;
          Format.pp_print_cut formatter ();
        end);
      pp_elt formatter x)

let pp_buf ?(sep=", ") pp_elt buf seq =
  let first = ref true in
  seq
    (fun x ->
      if !first then first := false else Buffer.add_string buf sep;
      pp_elt buf x)

let to_string ?sep pp_elt seq =
  let buf = Buffer.create 25 in
  pp_buf ?sep (fun buf x -> Buffer.add_string buf (pp_elt x)) buf seq;
  Buffer.contents buf

(** {2 Basic IO} *)

module IO = struct
  let lines_of ?(mode=0o644) ?(flags=[Open_rdonly]) filename =
    fun k ->
      let ic = open_in_gen flags mode filename in
      try
        while true do
          let line = input_line ic in
          k line
        done
      with
      | End_of_file -> close_in ic
      | e -> close_in_noerr ic; raise e

  let chunks_of ?(mode=0o644) ?(flags=[]) ?(size=1024) filename =
    fun k ->
      let ic = open_in_gen flags mode filename in
      try
        let buf = String.create size in
        let n = ref 0 in
        let stop = ref false in
        while not !stop do
          n := 0;
          (* try to read [size] chars. If [input] returns [0] it means
              the end of file, so we stop, but first we yield the current chunk *)
          while !n < size && not !stop do
            let n' = input ic buf !n (size - !n) in
            if n' = 0 then stop := true else n := !n + n';
          done;
          if !n > 0
            then k (String.sub buf 0 !n)
        done;
        close_in ic
      with e ->
        close_in_noerr ic;
        raise e

  let write_to ?(mode=0o644) ?(flags=[Open_creat;Open_wronly]) filename seq =
    let oc = open_out_gen flags mode filename in
    try
      seq (fun s -> output oc s 0 (String.length s));
      close_out oc
    with e ->
      close_out oc;
      raise e

  let write_lines ?mode ?flags filename seq =
    write_to ?mode ?flags filename (snoc (intersperse "\n" seq) "\n")
end


