
(* This file is free software, part of containers. See file "license" for more details. *)

(* Copyright (C) 2015 Simon Cruanes, Carmelo Piccione *)

(** Generic Circular Buffer for IO, with bulk operations.
    The bulk operations (e.g. based on {!Array.blit} or {!Bytes.blit})
    are more efficient than item-by-item copy.

    See https://en.wikipedia.org/wiki/Circular_buffer for an overview. *)

module Array = struct
  (** The abstract type for arrays *)
  module type S = sig
    (** The element type *)
    type elt

    (** The type of an array instance *)
    type t

    val dummy : elt
    (** A dummy element used for empty slots in the array
        @since 2.4 *)

    val create : int -> t
    (** Make an array of the given size, filled with dummy elements *)

    val length: t -> int
    (** [length t] gets the total number of elements currently in [t] *)

    val get: t -> int -> elt
    (** [get t i] gets the element at position [i] *)

    val set: t -> int -> elt -> unit
    (** [set t i e] sets the element at position [i] to [e] *)

    val sub: t -> int -> int -> t
    (** [sub t i len] gets the subarray of [t] from
        position [i] to [i + len] *)

    val copy : t -> t
    (** [copy t] makes a fresh copy of the array [t] *)

    val blit : t -> int -> t -> int -> int -> unit
    (** [blit t s arr i len] copies [len] elements from [arr] starting at [i]
        to position [s] from [t] *)

    val iter : (elt -> unit) -> t -> unit
    (** [iter f t] iterates over the array [t] invoking [f] with
        the current element, in array order *)
  end

  module Byte :
    S with type elt = char and type t = Bytes.t = struct
    type elt = char
    let dummy = '\x00'
    include Bytes
  end

  module Make(Elt:sig type t val dummy : t end) :
    S with type elt = Elt.t and type t = Elt.t array = struct
    type elt = Elt.t
    type t = Elt.t array
    let dummy = Elt.dummy
    let create size = Array.make size Elt.dummy
    let length = Array.length
    let get = Array.get
    let set = Array.set
    let copy = Array.copy
    let blit = Array.blit
    let iter = Array.iter
    let sub = Array.sub
  end
end

module type S = sig
  (** The module type of Array for this ring buffer *)
  module Array : Array.S

  (** Defines the bounded ring buffer type *)
  type t

  (** Raised in querying functions when the buffer is empty *)
  exception Empty

  val create : int -> t
  (** [create size] creates a new bounded buffer with given size.
      The underlying array is allocated immediately and no further (large)
      allocation will happen from now on.
      @raise Invalid_argument if the arguments is [< 1] *)

  val copy : t -> t
  (** Make a fresh copy of the buffer. *)

  val capacity : t -> int
  (** Length of the inner buffer. *)

  val length : t -> int
  (** Number of elements currently stored in the buffer. *)

  val is_full : t -> bool
  (** true if pushing an element would erase another element.
      @since 1.3 *)

  val blit_from : t -> Array.t -> int -> int -> unit
  (** [blit_from buf from_buf o len] copies the slice [o, ... o + len - 1] from
      a input buffer [from_buf] to the end of the buffer.
      If the slice is too large for the buffer, only the last part of the array
      will be copied.
      @raise Invalid_argument if [o,len] is not a valid slice of [s] *)

  val blit_into : t -> Array.t -> int -> int -> int
  (** [blit_into buf to_buf o len] copies at most [len] elements from [buf]
      into [to_buf] starting at offset [o] in [s].
      @return the number of elements actually copied ([min len (length buf)]).
      @raise Invalid_argument if [o,len] is not a valid slice of [s]. *)

  val append : t -> into:t -> unit
  (** [append b ~into] copies all data from [b] and adds it at the
      end of [into]. Erases data of [into] if there is not enough room. *)

  val to_list : t -> Array.elt list
  (** Extract the current content into a list *)

  val clear : t -> unit
  (** Clear the content of the buffer. Doesn't actually destroy the content. *)

  val is_empty :t -> bool
  (** Is the buffer empty (i.e. contains no elements)? *)

  val junk_front : t -> unit
  (** Drop the front element from [t].
      @raise Empty if the buffer is already empty. *)

  val junk_back : t -> unit
  (** Drop the back element from [t].
      @raise Empty if the buffer is already empty. *)

  val skip : t -> int -> unit
  (** [skip b len] removes [len] elements from the front of [b].
      @raise Invalid_argument if [len > length b]. *)

  val iter : t -> f:(Array.elt -> unit) -> unit
  (** [iter b ~f] calls [f i t] for each element [t] in [buf] *)

  val iteri : t -> f:(int -> Array.elt -> unit) -> unit
  (** [iteri b ~f] calls [f i t] for each element [t] in [buf], with [i]
      being its relative index within [buf]. *)

  val get_front : t -> int -> Array.elt
  (** [get_front buf i] returns the [i]-th element of [buf] from the front, ie
      the one returned by [take_front buf] after [i-1] calls to [junk_front buf].
      @raise Invalid_argument if the index is invalid (> [length buf]) *)

  val get_back : t -> int -> Array.elt
  (** [get_back buf i] returns the [i]-th element of [buf] from the back, ie
      the one returned by [take_back buf] after [i-1] calls to [junk_back buf].
      @raise Invalid_argument if the index is invalid (> [length buf]) *)

  val push_back : t -> Array.elt -> unit
  (** Push value at the back of [t].
      If [t.bounded=false], the buffer will grow as needed,
      otherwise the oldest elements are replaced first. *)

  val peek_front : t -> Array.elt option
  (** First value from front of [t], without modification. *)

  val peek_front_exn : t -> Array.elt
  (** First value from front of [t], without modification.
      @raise Empty if buffer is empty.
      @since 1.3 *)

  val peek_back : t -> Array.elt option
  (** Get the last value from back of [t], without modification. *)

  val peek_back_exn : t -> Array.elt
  (** Get the last value from back of [t], without modification.
      @raise Empty if buffer is empty.
      @since 1.3 *)

  val take_back : t -> Array.elt option
  (** Take and remove the last value from back of [t], if any *)

  val take_back_exn : t -> Array.elt
  (** Take and remove the last value from back of [t].
      @raise Empty if buffer is already empty. *)

  val take_front : t -> Array.elt option
  (** Take and remove the first value from front of [t], if any *)

  val take_front_exn : t -> Array.elt
  (** Take and remove the first value from front of [t].
      @raise Empty if buffer is already empty. *)

  val of_array : Array.t -> t
  (** Create a buffer from an initial array, but doesn't take ownership
      of it (stills allocates a new internal array)
      @since 0.11 *)

  val to_array : t -> Array.t
  (** Create an array from the elements, in order.
      @since 0.11 *)
end

(*$inject
  open Q.Gen
  let g_char = map Char.chr (Char.code 'A' -- Char.code 'z')
  let g_str = string_size ~gen:g_char (0--10)
  let a_str = Q.set_gen g_str Q.string
*)

module MakeFromArray(A:Array.S) : S with module Array = A = struct
  module Array = A

  type t = {
    mutable start : int;
    mutable stop : int; (* excluded *)
    buf : Array.t;
  }

  exception Empty

  let create size =
    if size < 1 then invalid_arg "CCRingBuffer.create";
    { start=0;
      stop=0;
      buf = A.create (size+1);  (* keep room for extra slot *)
    }

  let copy b =
    { b with buf=A.copy b.buf; }

  (*$T
    let b = Byte.of_array (Bytes.of_string "abc") in \
    let b' = Byte.copy b in \
    Byte.clear b; \
    Byte.to_array b' = (Bytes.of_string "abc") && Byte.to_array b = Bytes.empty
  *)

  let capacity b =
    let len = A.length b.buf in
    match len with 0 -> 0 | l -> l - 1

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    Byte.capacity b >= s_len)
  *)

  let length b =
    if b.stop >= b.start
    then b.stop - b.start
    else (A.length b.buf - b.start) + b.stop

  let is_full b = length b + 1 = Array.length b.buf

  let next_ b i =
    let j = i+1 in
    if j = A.length b.buf then 0 else j

  let incr_start_ b = b.start <- next_ b b.start
  let incr_stop_ b = b.stop <- next_ b b.stop

  let push_back b e =
    A.set b.buf b.stop e;
    incr_stop_ b;
    if b.start = b.stop then incr_start_ b; (* overwritten one element *)
    ()

  let blit_from b from_buf o len =
    if len = 0 then ()
    else if o + len > A.length from_buf then invalid_arg "CCRingBuffer.blit_from"
    else (
      for i=o to o+len-1 do
        push_back b (A.get from_buf i)
      done
    )

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    let b' = Byte.copy b in \
    try Byte.iteri b ~f:(fun i c -> if Byte.get_front b' i <> c then raise Exit); true with Exit -> false)
  *)

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    Byte.push_back b 'X'; \
    Byte.peek_back_exn b = 'X')
  *)

  (*$Q
    (Q.pair a_str a_str) (fun (s,s') -> \
    let b = Byte.create (max (String.length s+String.length s') 64) in \
    let s = Bytes.of_string s in let s' = Bytes.of_string s' in  \
    Byte.blit_from b s 0 (Bytes.length s); \
    Byte.blit_from b s' 0 (Bytes.length s'); \
    Byte.length b = Bytes.length s + Bytes.length s')
  *)


  (*$Q
    (Q.pair a_str a_str) (fun (s,s') -> \
    let s = Bytes.of_string s in let s' = Bytes.of_string s' in  \
    let b = Byte.create (max (Bytes.length s + Bytes.length s') 64) in \
    Byte.blit_from b s 0 (Bytes.length s); \
    Byte.blit_from b s' 0 (Bytes.length s'); \
    Byte.length b = Bytes.length s + Bytes.length s')
  *)


  let blit_into b to_buf o len =
    if o+len > A.length to_buf then (
      invalid_arg "CCRingBuffer.blit_into";
    );
    if b.stop >= b.start then (
      let n = min (b.stop - b.start) len in
      A.blit b.buf b.start to_buf o n;
      n
    ) else (
      let len_end = A.length b.buf - b.start in
      A.blit b.buf b.start to_buf o (min len_end len);
      if len_end >= len
      then len  (* done *)
      else (
        let n = min b.stop (len - len_end) in
        A.blit b.buf 0 to_buf (o+len_end) n;
        n + len_end
      )
    )

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let b = Byte.create (max 64 (Bytes.length s)) in \
    Byte.blit_from b s 0 (Bytes.length s); \
    let to_buf = Bytes.create (Bytes.length s) in \
    let len = Byte.blit_into b to_buf 0 (Bytes.length s) in \
    to_buf = s && len = Bytes.length s)
  *)

  let is_empty b = b.start = b.stop

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    Byte.skip b s_len; \
    Byte.is_empty b)
  *)

  let take_front_exn b =
    if b.start = b.stop then raise Empty;
    let c = A.get b.buf b.start in
    A.set b.buf b.start A.dummy;
    b.start <- next_ b b.start;
    c

  let take_front b = try Some (take_front_exn b) with Empty -> None

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    try let front = Byte.take_front_exn b in \
    front = Bytes.get s 0 with Byte.Empty -> s_len = 0)
  *)

  let take_back_exn b =
    if b.start = b.stop then raise Empty;
    if b.stop = 0
    then b.stop <- A.length b.buf - 1
    else b.stop <- b.stop - 1;
    let c = A.get b.buf b.stop in
    A.set b.buf b.stop A.dummy;
    c

  let take_back b = try Some (take_back_exn b) with Empty -> None

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    try let back = Byte.take_back_exn b in \
        back = Bytes.get s (Bytes.length s - 1) \
     with Byte.Empty -> s_len = 0)
  *)

  let junk_front b =
    if b.start = b.stop then raise Empty;
    A.set b.buf b.start A.dummy;
    if b.start + 1 = A.length b.buf
    then b.start <- 0
    else b.start <- b.start + 1

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    try let () = Byte.junk_front b in \
    s_len - 1 = Byte.length b with Byte.Empty -> s_len = 0)
  *)

  let junk_back b =
    if b.start = b.stop then raise Empty;
    if b.stop = 0
      then b.stop <- A.length b.buf - 1
      else b.stop <- b.stop - 1;
    A.set b.buf b.stop A.dummy

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    try let () = Byte.junk_back b in \
    s_len - 1 = Byte.length b with Byte.Empty -> s_len = 0)
  *)

  let skip b len =
    if len > length b then (
      invalid_arg "CCRingBuffer.skip";
    );
    for _ = 1 to len do
      junk_front b
    done

  (*$Q
    (Q.pair a_str a_str) (fun (s,s') -> \
    let s = Bytes.of_string s in let s' = Bytes.of_string s' in  \
    let b = Byte.create (max (Bytes.length s+Bytes.length s') 64) in \
    Byte.blit_from b s 0 (Bytes.length s); \
    Byte.blit_from b s' 0 (Bytes.length s'); \
    let h = Bytes.of_string "hello world" in \
    Byte.blit_from b h 0 (Bytes.length h); (* big enough *) \
    let l = Byte.length b in let l' = l/2 in Byte.skip b l'; \
    Byte.length b + l' = l)
  *)

  let clear b =
    skip b (length b)

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    Byte.clear b; \
    Byte.length b = 0)
  *)


  let iter b ~f =
    if b.stop >= b.start
    then for i = b.start to b.stop - 1 do f (A.get b.buf i) done
    else (
      for i = b.start to A.length b.buf -1 do f (A.get b.buf i) done;
      for i = 0 to b.stop - 1 do f (A.get b.buf i) done;
    )

  let iteri b ~f =
    if b.stop >= b.start
    then for i = b.start to b.stop - 1 do f i (A.get b.buf i) done
    else (
      for i = b.start to A.length b.buf -1 do f i (A.get b.buf i) done;
      for i = 0 to b.stop - 1 do f i (A.get b.buf i) done;
    )

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    try Byte.iteri b ~f:(fun i c -> if Byte.get_front b i <> c then raise Exit); \
      true with Exit -> false)
  *)

  let get b i =
    if b.stop >= b.start
    then (
      if i >= b.stop - b.start then (
        invalid_arg "CCRingBuffer.get"
      ) else A.get b.buf (b.start + i)
    ) else (
      let len_end = A.length b.buf - b.start in
      if i < len_end then A.get b.buf (b.start + i)
      else if i - len_end > b.stop then (
        invalid_arg "CCRingBuffer.get"
      ) else A.get b.buf (i - len_end)
    )

  let get_front b i =
    if is_empty b then (
      invalid_arg "CCRingBuffer.get_front"
    ) else get b i

  (*$Q
    (Q.pair Q.small_int a_str) (fun (i, s) -> \
    let s = Bytes.of_string (s ^ " ") in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    let index = abs (i mod Byte.length b) in \
    let front = Byte.get_front b index in \
    front = Bytes.get s index)
  *)

  let get_back b i =
    let offset = ((length b) - i - 1) in
    if offset < 0 then (
      invalid_arg "CCRingBuffer.get_back"
    ) else get b offset

  (*$Q
    (Q.pair Q.small_int a_str) (fun (i, s) -> \
    let s = Bytes.of_string (s ^ " ") in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    let index = abs (i mod Byte.length b) in \
    let back = Byte.get_back b index in \
    back = Bytes.get s (s_len - index - 1))
  *)


  let to_list b =
    let len = length b in
    let rec build l i =
      if i < 0 then l else build ((get_front b i)::l) (i-1)
    in
    build [] (len-1)

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    let l = Byte.to_list b in \
    let explode s = let rec exp i l = \
     if i < 0 then l else exp (i - 1) (Bytes.get s i :: l) in \
     exp (Bytes.length s - 1) [] in \
    explode s = l)
  *)

  (* TODO: more efficient version, with one or two blit *)
  let append b ~into =
    iter b ~f:(push_back into)

  let peek_front_exn b =
    if is_empty b then raise Empty
    else A.get b.buf b.start

  let peek_front b = try Some (peek_front_exn b) with Empty -> None

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    try let back = Byte.peek_front_exn b in \
    back = Bytes.get s 0 with Byte.Empty -> s_len = 0)
  *)

  let peek_back_exn b = if is_empty b
    then raise Empty
    else (
      let i = if b.stop = 0 then A.length b.buf - 1 else b.stop-1 in
      A.get b.buf i
    )

  let peek_back b = try Some (peek_back_exn b) with Empty -> None

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
    let s_len = Bytes.length s in \
    let b = Byte.create (max s_len 64) in \
    Byte.blit_from b s 0 s_len; \
    try let back = Byte.peek_back_exn b in \
    back = Bytes.get s (s_len - 1) with Byte.Empty -> s_len = 0)
  *)

  let of_array a =
    let b = create (max (A.length a) 16) in
    blit_from b a 0 (A.length a);
    b

  let to_array b =
    let a = A.create (length b) in
    let n = blit_into b a 0 (length b) in
    assert (n = length b);
    a

  (*$Q
    a_str (fun s -> let s = Bytes.of_string s in \
      let b = Byte.of_array s in let s' = Byte.to_array b in \
      s = s')
  *)
end

module Byte = MakeFromArray(Array.Byte)

module Make(Elt:sig
    type t
    val dummy : t
  end) = MakeFromArray(Array.Make(Elt))


(*$inject
  module BI = CCRingBuffer.Make(struct type t = int let dummy=0 end)
*)

(* try to trigger an error on resize
   see issue #126 *)
(*$R
  let b = BI.create 50 in
  let st = Random.State.make [| 0 |] in
  for _i = 1 to 100_000 do
    if Random.State.float st 1.0 < 0.5 then
      BI.push_back b 0
    else
      let _ = BI.take_front b in ()
  done
*)

(* Test against reference implementation (lists) on a succession of
   operations.

   Remarks on semantics:

   JUNK_FRONT/JUNK_BACK: try to remove if not empty
   SKIP: if at least n elements, skip; else nop
*)

(*$inject
  module BS = CCRingBuffer.Byte

  type op =
  | Push_back of char
  | Take_front
  | Take_back
  | Peek_front
  | Peek_back
  | Junk_front
  | Junk_back
  | Skip of int
  | Blit of string * int * int
  | Z_if_full

  let str_of_op = function
  | Push_back c -> Printf.sprintf "push_back(%C)" c
  | Take_front -> Printf.sprintf "take_front"
  | Take_back -> Printf.sprintf "take_back"
  | Peek_front -> Printf.sprintf "peek_front"
  | Peek_back -> Printf.sprintf "peek_back"
  | Junk_front -> Printf.sprintf "junk_front"
  | Junk_back -> Printf.sprintf "junk_back"
  | Skip n -> Printf.sprintf "skip(%d)" n
  | Blit (s,i,len) -> Printf.sprintf "blit(%S,%d,%d)" s i len
  | Z_if_full -> "zero_if_full"

  let push_back c = Push_back c
  let skip n = assert (n>=0); Skip n
  let blit s i len =
  if i<0 || len<0 || i+len > String.length s then (
    failwith ("wrong blit: " ^ str_of_op (Blit (s,i,len)));
  );
  Blit (s,i,len)

  let shrink_op =
  let open Q.Iter in
  function
    | Push_back c -> Q.Shrink.char c >|= push_back
    | Take_front | Take_back | Junk_back | Junk_front
    | Z_if_full | Peek_front | Peek_back
    -> empty
    | Skip n -> Q.Shrink.int n >|= skip
    | Blit (s,i,len) ->
      let s_i =
        Q.Shrink.int i >>= fun i' ->
        assert (i' <= i && i' + len <= String.length s);
        if i' <= 0 then empty else return (blit s i' len)
      and s_len =
        Q.Shrink.int len >>= fun len'->
        assert (len' <= len && i + len' <= String.length s);
        if len' <= 0 then empty else return (blit s i len')
      and s_s =
        Q.Shrink.string s >>= fun s' ->
        if i+len > String.length s' then empty else return (blit s' i len)
      in
      append s_i (append s_len s_s)

  let rec len_op size acc = function
  | Push_back _ -> min size (acc + 1)
  | Take_front | Take_back | Junk_front | Junk_back -> max (acc-1) 0
  | Skip n -> if acc >= n then acc-n else acc
  | Z_if_full | Peek_front | Peek_back -> acc
  | Blit (_,_,len) -> min size (acc + len)

  let apply_op b = function
  | Push_back c -> BS.push_back b c; None
  | Take_front -> BS.take_front b
  | Take_back -> BS.take_back b
  | Junk_front -> (try BS.junk_front b with BS.Empty -> ()); None
  | Junk_back -> (try BS.junk_back b with BS.Empty -> ()); None
  | Peek_front -> BS.peek_front b
  | Peek_back -> BS.peek_back b
  | Skip n -> if n <= BS.length b then BS.skip b n; None
  | Blit (s,i,len) ->
    assert(i+len <= String.length s);
    BS.blit_from b (Bytes.unsafe_of_string s) i len; None
  | Z_if_full -> if BS.is_full b then Some '0' else None

  let gen_op =
  let open Q.Gen in
  let g_blit =
    string_size ~gen:g_char (5--20) >>= fun s ->
    (0 -- String.length s) >>= fun len ->
    assert (len >= 0 && len <= String.length s);
    (0--(String.length s-len)) >|= fun i ->
    blit s i len
  in
  frequency
    [ 3, return Take_back;
      3, return Take_front;
      1, return Junk_back;
      1, return Junk_front;
      1, return Peek_front;
      1, return Peek_back;
      2, g_blit;
      1, (0--5 >|= skip);
      2, map push_back g_char;
      1, return Z_if_full;
    ]

  let arb_op =
  Q.make
    ~shrink:shrink_op
    ~print:str_of_op
    gen_op

  let arb_ops = Q.list_of_size Q.Gen.(0 -- 20) arb_op

  module L_impl = struct
  type t = {
    size: int;
    mutable l: char list;
  }

  let create size = {size; l=[]}

  let normalize_ b =
    let n = List.length b.l in
    if n>b.size then b.l <- CCList.drop (n-b.size) b.l

  let push_back b c = b.l <- b.l @ [c]; normalize_ b
  let take_front b = match b.l with
    | [] -> None
    | c :: l -> b.l <- l; Some c
  let peek_front b = match b.l with [] -> None | x::_ -> Some x
  let take_back b =
    let n = List.length b.l in
    if n=0 then None
    else (
      let init, last = CCList.take_drop (n-1) b.l in
      let x = List.hd last in
      b.l <- init;
      Some x
    )
  let peek_back b = match b.l with [] -> None | l -> Some (List.hd (List.rev l))
  let junk_front b = ignore (take_front b)
  let junk_back b = ignore (take_back b)
  let skip b n =
    if n <= List.length b.l then (
      CCInt.range' 0 n (fun _ -> junk_front b)
    )

  let blit b s i len =
    for j=i to i+len-1 do push_back b (String.get s j) done

  let apply_op b = function
    | Push_back c -> push_back b c; None
    | Take_front -> take_front b
    | Take_back -> take_back b
    | Peek_front -> peek_front b
    | Peek_back -> peek_back b
    | Junk_back -> junk_back b; None
    | Junk_front -> junk_front b; None
    | Skip n -> skip b n; None
    | Blit (s,i,len) -> blit b s i len; None
    | Z_if_full -> if b.size = List.length b.l then Some '0' else None

  let to_list b = b.l
  end

*)

(* check that a lot of operations can be applied without failure,
   and that the result has correct length *)
(*$QR & ~count:3_000
  arb_ops (fun ops ->
    let size = 64 in
    let b = BS.create size in
    List.iter (fun o-> ignore (apply_op b o)) ops;
    BS.length b = List.fold_left (len_op size) 0 ops)
*)

(* check identical behavior with list implem *)
(*$QR & ~count:3_000
  arb_ops (fun ops ->
    let size = 64 in
    let b = BS.create size in
    let l = L_impl.create size in
    let l1 = CCList.filter_map (apply_op b) ops in
    let l2 = CCList.filter_map (L_impl.apply_op l) ops in
    l1=l2 && BS.to_list b = L_impl.to_list l)
*)

(* check that deleted elements can be GCed *)
(*$inject
  module BO = CCRingBuffer.Make(struct type t = int option let dummy=None end)
  let make_bo () =
    let b = BO.create 1000 in
    for i = 1 to BO.capacity b do
      BO.push_back b (Some i)
    done;
    b
  let test_no_major_blocks clear =
    Gc.full_major ();
    let live_blocks_before = (Gc.stat ()).live_blocks in
    let b = make_bo () in
    clear b;
    Gc.full_major ();
    let live_blocks_after = (Gc.stat ()).live_blocks in
    assert (BO.length b = 0);
    let diff = live_blocks_after - live_blocks_before in
    diff < BO.capacity b / 2
*)

(*$T
  test_no_major_blocks (fun b -> for _ = 1 to BO.length b do BO.junk_front b; done)
  test_no_major_blocks (fun b -> for _ = 1 to BO.length b do BO.junk_back b; done)
  test_no_major_blocks (fun b -> for _ = 1 to BO.length b do ignore (BO.take_front b); done)
  test_no_major_blocks (fun b -> for _ = 1 to BO.length b do ignore (BO.take_back b); done)
  test_no_major_blocks (fun b -> BO.skip b (BO.length b))
  test_no_major_blocks (fun b -> BO.clear b)
*)
