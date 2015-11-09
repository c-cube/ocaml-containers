(*
copyright (c) 2013-2015, simon cruanes
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

(** {1 Bigarrays of dimension 1 *)

module A = Bigarray.Array1

type 'a printer = Format.formatter -> 'a -> unit
type 'a sequence = ('a -> unit) -> unit
type 'a or_error = [`Ok of 'a | `Error of string]
type random = Random.State.t

type json = [ `Assoc of (string * json) list
            | `Bool of bool
            | `Float of float
            | `Int of int
            | `List of json list
            | `Null
            | `String of string ]
type 'a to_json = 'a -> json
type 'a of_json = json -> 'a or_error

type ('a, 'b, 'perm) t =
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
  constraint 'perm = [< `R | `W]

type ('a, 'b, 'perm) array_ = ('a, 'b, 'perm) t

exception WrongDimension

let make ?x ~kind n =
  let a = A.create kind Bigarray.c_layout n in
  begin match x with
    | None -> ()
    | Some x -> A.fill a x
  end;
  a

let make_int n = make ~kind:Bigarray.int n
let make_char n = make ~kind:Bigarray.char n
let make_int8s n = make ~kind:Bigarray.int8_signed n
let make_int8u n = make ~kind:Bigarray.int8_unsigned n
let make_int16s n = make ~kind:Bigarray.int16_signed n
let make_int16u n = make ~kind:Bigarray.int16_unsigned n
let make_int32 n = make ~kind:Bigarray.int32 n
let make_int64 n = make ~kind:Bigarray.int64 n
let make_native n = make ~kind:Bigarray.nativeint n
let make_float32 n = make ~kind:Bigarray.float32 n
let make_float64 n = make ~kind:Bigarray.float64 n
let make_complex32 n = make ~kind:Bigarray.complex32 n
let make_complex64 n = make ~kind:Bigarray.complex64 n

let init ~kind ~f n =
  let a = A.create kind Bigarray.c_layout n in
  for i = 0 to n-1 do
    A.unsafe_set a i (f i)
  done;
  a

(*$T
  let a = init ~kind:Bigarray.int 10 ~f:(fun x->x) in \
  CCList.(0 -- 9) |> List.for_all (fun i -> get a i = i)
*)

let of_bigarray a = a
let to_bigarray a = a

let ro (t : ('a,'b,[>`R]) t) : ('a,'b,[`R]) t = t
let wo (t : ('a,'b,[>`W]) t) : ('a,'b,[`W]) t = t

let fill = A.fill

let copy a =
  let b = make ~kind:(A.kind a) (A.dim a) in
  A.blit a b;
  b

let length a = A.dim a

(*$T
  length (make_int 42) = 42
*)

let set = A.set

let get = A.get

let blit = A.blit

let sub = A.sub

let iter ~f a =
  for i = 0 to A.dim a - 1 do
    f (A.unsafe_get a i)
  done

exception LocalExit

let for_all ~f a =
  try
    for i = 0 to A.dim a - 1 do
      if not (f (A.unsafe_get a i)) then raise LocalExit
    done;
    true
  with LocalExit -> false

let exists ~f a =
  try
    for i = 0 to A.dim a - 1 do
      if f (A.unsafe_get a i) then raise LocalExit
    done;
    false
  with LocalExit -> true

(*$T
  init ~kind:Bigarray.int 10 ~f:(fun x->x) |> for_all ~f:(fun x -> x<10)
  init ~kind:Bigarray.int 10 ~f:(fun x->x) |> exists ~f:(fun x -> x=5)
*)

let iteri ~f a =
  for i = 0 to A.dim a - 1 do
    f i (A.unsafe_get a i)
  done

let foldi f acc a =
  let rec fold' f acc a i =
    if i = A.dim a then acc
    else
      let acc = f acc i (A.unsafe_get a i) in
      fold' f acc a (i+1)
  in
  fold' f acc a 0

let pp pp_x out a =
  Format.pp_print_char out '[';
  iteri a
    ~f:(fun i x ->
        if i > 0 then Format.fprintf out ",@ ";
        pp_x out x
      );
  Format.pp_print_char out ']';
  ()

module Bool = struct
  type ('a, 'perm) t = (int, 'a, 'perm) array_

  let set a i x = A.set a i (if x then 1 else 0)

  let get a i = A.get a i <> 0

  let zeroes n = make ~x:0 ~kind:Bigarray.int8_unsigned n
  let ones n = make ~x:1 ~kind:Bigarray.int8_unsigned n

  let iter_zeroes ~f a =
    for i = 0 to A.dim a - 1 do
      if A.unsafe_get a i = 0 then f i
    done

  let iter_ones ~f a =
    for i = 0 to A.dim a - 1 do
      if A.unsafe_get a i > 0 then f i
    done

  let cardinal a =
    let rec fold a i acc =
      if i = A.dim a then acc
      else
        let acc = if A.get a i <> 0 then acc+1 else acc in
        fold a (i+1) acc
    in
    fold a 0 0

  let or_ ?res a b =
    let res = match res with
      | Some r ->
        if A.dim r <> max (A.dim a) (A.dim b) then raise WrongDimension;
        A.fill r 0;
        r
      | None -> make ~x:0 ~kind:(A.kind a) (max (A.dim a) (A.dim b))
    in
    (* ensure [a] is no longer than [b] *)
    let a, b = if A.dim a < A.dim b then a, b else b, a in
    for i = 0 to A.dim a - 1 do
      if A.unsafe_get a i > 0 || A.unsafe_get b i > 0
      then set b i true
    done;
    res

  let and_ ?res a b =
    let res = match res with
      | Some r ->
        if A.dim r <> max (A.dim a) (A.dim b) then raise WrongDimension;
        A.fill r 0;
        r
      | None -> make ~x:0 ~kind:(A.kind a) (max (A.dim a) (A.dim b))
    in
    (* ensure [a] is no longer than [b] *)
    let a, b = if A.dim a < A.dim b then a, b else b, a in
    for i=0 to A.dim a - 1 do
      if A.unsafe_get a i > 0 && A.unsafe_get b i > 0
      then set res i true
    done;
    res

  let not_ ?res a =
    let res = match res with
      | Some r ->
        if A.dim r <> A.dim a then raise WrongDimension;
        A.fill r 0;
        r
      | None -> make ~x:0 ~kind:(A.kind a) (A.dim a)
    in
    for i=0 to A.dim a - 1 do
      if A.unsafe_get a i = 0 then set res i true
    done;
    res

  (* assumes dimensions are ok and [A.dim a >= A.dim b] *)
  let mix_ a b ~res =
    let na = A.dim a
    and nb = A.dim b in
    assert (nb <= na);
    (* a has more bits, and we group them in successive chunks of size [d] *)
    let step = 1 + (na + nb) / nb in
    for i = 0 to na + nb - 1 do
      let q, r = i / step, i mod step in
      if r = 0
      then set res i (get b q)
      else set res i (get a (q + r - 1))
    done

  let mix ?res a b =
    let res = match res with
      | Some r ->
        if A.dim a + A.dim b <> A.dim r then raise WrongDimension;
        r
      | None -> make ~kind:(A.kind a) (A.dim a + A.dim b)
    in
    if A.dim a < A.dim b then mix_ b a ~res else mix_ a b ~res;
    res

  let rec big_or_ a b i j acc =
    if j = A.dim b then acc
    else (* acc xor (a[i+j] and b[j]) *)
      let acc = acc <> (get a ((i+j) mod A.dim a) && get b j) in
      big_or_ a b i (j+1) acc

  (* [into[i] = big_or_{j in [0...nb-1]} (a[i+j-1 mod na] and b[j]) *)
  let convolution ?res a ~by:b =
    let res = match res with
      | Some r ->
        if A.dim a < A.dim b || A.dim a <> A.dim r then raise WrongDimension;
        r
      | None -> make ~kind:(A.kind a) (A.dim a)
    in
    for i = 0 to A.dim res - 1 do
      if big_or_ a b i 0 false then set res i true
    done;
    res

  let pp out a = pp
      (fun oc b ->
         Format.pp_print_char oc (if b>0 then '1' else '0')
      ) out a
end

let append ?res a b =
  let res = match res with
    | Some r ->
      if A.dim a + A.dim b <> A.dim r then raise WrongDimension;
      r
    | None -> make ~kind:(A.kind a) (A.dim a + A.dim b)
  in
  let n = A.dim a in
  A.blit a (A.sub res 0 n);
  A.blit b (A.sub res n (A.dim b));
  res

let map ?res ~f a =
  let res = match res with
    | Some r ->
      if A.dim a <> A.dim r then raise WrongDimension;
      r
    | None -> make ~kind:(A.kind a) (A.dim a)
  in
  for i=0 to A.dim a - 1 do
    A.set res i (f (A.unsafe_get a i))
  done;
  res

let map2 ?res ~f a b =
  if A.dim a <> A.dim b then raise WrongDimension;
  let res = match res with
    | Some r ->
      if A.dim r <> A.dim a then raise WrongDimension;
      r
    | None -> make ~kind:(A.kind a) (A.dim a)
  in
  for i=0 to A.dim a - 1 do
    A.set res i (f (A.unsafe_get a i) (A.unsafe_get b i))
  done;
  res

let filter ?res ~f a =
  let res = match res with
    | Some r ->
      if A.dim a <> A.dim r then raise WrongDimension;
      r
    | None -> make ~x:0 ~kind:Bigarray.int8_unsigned (A.dim a)
  in
  for i=0 to A.dim a - 1 do
    if f (A.unsafe_get a i)
    then Bool.set res i true
  done;
  res

module type S = sig
  type elt
  type ('a, 'perm) t = (elt, 'a, 'perm) array_

  val add :
    ?res:('a, [>`W] as 'perm) t ->
    ('a, [>`R]) t ->
    ('a, [>`R]) t ->
    ('a, 'perm) t
  (** Elementwise sum
      @raise WrongDimension if dimensions do not fit *)

  val mult :
    ?res:('a, [>`W] as 'perm) t ->
    ('a, [>`R]) t ->
    ('a, [>`R]) t ->
    ('a, 'perm) t
  (** Elementwise product *)

  val scalar_add :
    ?res:('a, [>`W] as 'perm) t ->
    ('a, [>`R]) t ->
    x:elt ->
    ('a, 'perm) t
    (** @raise WrongDimension if dimensions do not fit *)

  val scalar_mult :
    ?res:('a, [>`W] as 'perm) t ->
    ('a, [>`R]) t ->
    x:elt ->
    ('a, 'perm) t
    (** @raise WrongDimension if dimensions do not fit *)

  val sum_elt : (_, [>`R]) t -> elt
  (** Efficient sum of elements *)

  val product_elt : (_, [>`R]) t -> elt
  (** Efficient product of elements *)

  val dot_product : (_, [>`R]) t -> (_, [>`R]) t -> elt
  (** [dot_product a b] returns [sum_i a(i)*b(i)] with the given
      sum and product, on [elt].
      [dot_product a b = sum_elt (product a b)]
      @raise WrongDimension if [a] and [b] do not have the same size *)

  module Infix : sig
    val ( * ) : ('a, [>`R]) t -> ('a, [>`R]) t -> ('a, 'perm) t
    (** Alias to {!mult} *)

    val ( + ) : ('a, [>`R]) t -> (_, [>`R]) t -> ('a, 'perm) t
    (** Alias to {!add} *)

    val ( *! ) : ('a, [>`R]) t -> elt -> ('a, 'perm) t
    (** Alias to {!scalar_mult} *)

    val ( +! ) : ('a, [>`R]) t -> elt -> ('a, 'perm) t
    (** Alias to {!scalar_add} *)
  end

  include module type of Infix
end

module Int = struct
  type elt = int
  type ('a, 'perm) t = (elt, 'a, 'perm) array_

  let add ?res a b =
    if A.dim a <> A.dim b then raise WrongDimension;
    let res = match res with
      | Some r ->
        if A.dim a <> A.dim r then raise WrongDimension;
        r
      | None -> make ~x:0 ~kind:(A.kind a) (A.dim a)
    in
    for i = 0 to A.dim a - 1 do
      A.set res i (A.unsafe_get a i + A.unsafe_get b i)
    done;
    res

  let mult ?res a b =
    if A.dim a <> A.dim b then raise WrongDimension;
    let res = match res with
      | Some r ->
        if A.dim a <> A.dim r then raise WrongDimension;
        r
      | None -> make ~x:0 ~kind:(A.kind a) (A.dim a)
    in
    for i = 0 to A.dim a - 1 do
      A.set res i (A.unsafe_get a i * A.unsafe_get b i)
    done;
    res

  let scalar_add ?res a ~x =
    let res = match res with
      | Some r ->
        if A.dim a <> A.dim r then raise WrongDimension;
        r
      | None -> make ~x:0 ~kind:(A.kind a) (A.dim a)
    in
    for i = 0 to A.dim a - 1 do
      A.set res i (A.unsafe_get a i + x)
    done;
    res

  let scalar_mult ?res a ~x =
    let res = match res with
      | Some r ->
        if A.dim a <> A.dim r then raise WrongDimension;
        r
      | None -> make ~x:0 ~kind:(A.kind a) (A.dim a)
    in
    for i = 0 to A.dim a - 1 do
      A.set res i (A.unsafe_get a i * x)
    done;
    res

  let dot_product a b =
    if A.dim a <> A.dim b then raise WrongDimension;
    let r = ref 0 in
    for i = 0 to A.dim a - 1 do
      r := !r + (A.unsafe_get a i * A.unsafe_get b i)
    done;
    !r

  let sum_elt a =
    let r = ref 0 in
    for i = 0 to A.dim a - 1 do
      r := !r + A.unsafe_get a i
    done;
    !r

  let product_elt a =
    let r = ref 1 in
    for i = 0 to A.dim a - 1 do
      r := !r * A.unsafe_get a i
    done;
    !r

  module Infix = struct
    let ( + ) a b = add a b
    let ( * ) a b = mult a b

    let ( +! ) a x = scalar_add a ~x
    let ( *! ) a x = scalar_mult a ~x
  end

  include Infix
end

module Float = struct
  type elt = float
  type ('a, 'perm) t = (elt, 'a, 'perm) array_

  let add ?res a b =
    if A.dim a <> A.dim b then raise WrongDimension;
    let res = match res with
      | Some r ->
        if A.dim a <> A.dim r then raise WrongDimension;
        r
      | None -> make ~x:0. ~kind:(A.kind a) (A.dim a)
    in
    for i = 0 to A.dim a - 1 do
      A.set res i (A.unsafe_get a i +. A.unsafe_get b i)
    done;
    res

  let mult ?res a b =
    if A.dim a <> A.dim b then raise WrongDimension;
    let res = match res with
      | Some r ->
        if A.dim a <> A.dim r then raise WrongDimension;
        r
      | None -> make ~x:0. ~kind:(A.kind a) (A.dim a)
    in
    for i = 0 to A.dim a - 1 do
      A.set res i (A.unsafe_get a i *. A.unsafe_get b i)
    done;
    res

  let scalar_add ?res a ~x =
    let res = match res with
      | Some r ->
        if A.dim a <> A.dim r then raise WrongDimension;
        r
      | None -> make ~x:0. ~kind:(A.kind a) (A.dim a)
    in
    for i = 0 to A.dim a - 1 do
      A.set res i (A.unsafe_get a i +. x)
    done;
    res

  let scalar_mult ?res a ~x =
    let res = match res with
      | Some r ->
        if A.dim a <> A.dim r then raise WrongDimension;
        r
      | None -> make ~x:0. ~kind:(A.kind a) (A.dim a)
    in
    for i = 0 to A.dim a - 1 do
      A.set res i (A.unsafe_get a i *. x)
    done;
    res

  let dot_product a b =
    if A.dim a <> A.dim b then raise WrongDimension;
    let r = ref 0. in
    for i = 0 to A.dim a - 1 do
      r := !r +. (A.unsafe_get a i *. A.unsafe_get b i)
    done;
    !r

  let sum_elt a =
    let r = ref 0. in
    for i = 0 to A.dim a - 1 do
      r := !r +. A.unsafe_get a i
    done;
    !r

  let product_elt a =
    let r = ref 1. in
    for i = 0 to A.dim a - 1 do
      r := !r *. A.unsafe_get a i
    done;
    !r

  module Infix = struct
    let ( + ) a b = add a b
    let ( * ) a b = mult a b

    let ( +! ) a x = scalar_add a ~x
    let ( *! ) a x = scalar_mult a ~x
  end

  include Infix
end

let to_list a =
  let l = foldi (fun acc _ x -> x::acc) [] a in
  List.rev l

let to_array a =
  if A.dim a = 0 then [||]
  else (
    let b = Array.make (A.dim a) (A.get a 0) in
    for i = 1 to A.dim a - 1 do
      Array.unsafe_set b i (A.unsafe_get a i)
    done;
    b
  )

let to_seq a yield = iter a ~f:yield

let of_array ~kind a = A.of_array kind Bigarray.c_layout a

exception OfYojsonError of string

let to_yojson (f:'a -> json) a : json =
  let l = foldi (fun l _ x -> f x :: l) [] a in
  `List (List.rev l)

let int_to_yojson i = `Int i
let int_of_yojson = function
  | `Int i -> `Ok i
  | `Float f -> `Ok (int_of_float f)
  | `String s -> (try `Ok (int_of_string s) with _ -> `Error "expected int")
  | _ -> `Error "expected int"

let float_to_yojson f = `Float f
let float_of_yojson = function
  | `Float f -> `Ok f
  | `Int i -> `Ok (float_of_int i)
  | _ -> `Error "expected float"

let of_yojson
    ~(kind:('a,'b) Bigarray.kind)
    (f: json -> 'a or_error)
    (j : json) : ('a,'b,'perm) t or_error
=
  let unwrap_ = function
    | `Ok x -> x
    | `Error msg -> raise (OfYojsonError msg)
  in
  let map_l l = List.map (fun x -> unwrap_ (f x)) l
  and of_list l =
    let a = make ~kind (List.length l) in
    List.iteri (fun i b -> set a i b) l;
    a
  in
  try
    match j with
    | `List l -> `Ok (of_list (map_l l))
    | _ -> raise (OfYojsonError "invalid json (expected list)")
  with OfYojsonError msg ->
    `Error msg


module View = struct
  type 'a t = {
    len : int;
    view : 'a view
  }
  and _ view =
    | Arr : ('a, _, _) array_ -> 'a view
    | Map : ('a -> 'b) * 'a t -> 'b view
    | Map2 : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c view
    | Select : (int, _, _) array_ * 'a t -> 'a view
    | SelectA : int array * 'a t -> 'a view
    | SelectV : int t * 'a t -> 'a view
    | Raw :
        ('a, 'b, [>`R]) array_ *
        (('a, 'b, [>`R]) array_ -> int) *
        (('a, 'b, [>`R]) array_ -> int -> 'a) ->
        'a view

  let length t = t.len

  let rec get
    : type a. a t -> int -> a
    = fun v i -> match v.view with
    | Arr a -> A.get a i
    | Map (f, a) -> f (get a i)
    | Map2 (f, a1, a2) -> f (get a1 i) (get a2 i)
    | Select (idx, a) -> get a (A.get idx i)
    | SelectA (idx, a) -> get a (Array.get idx i)
    | SelectV (idx, a) -> get a (get idx i)
    | Raw (a, _, f) -> f a i

  let rec iteri
    : type a. f:(int -> a -> unit) -> a t -> unit
    = fun ~f v -> match v.view with
    | Arr a ->
      for i = 0 to A.dim a - 1 do
        f i (A.unsafe_get a i)
      done
    | Map (g, a') ->
      iteri a' ~f:(fun i x -> f i (g x))
    | Map2 (g, a1, a2) ->
      iteri a1 ~f:(fun i x -> let y = get a2 i in f i (g x y))
    | Select (idx, a) ->
      for i = 0 to A.dim idx - 1 do
        let j = A.unsafe_get idx i in
        f i (get a j)
      done
    | SelectA (idx, a) ->
      Array.iteri (fun i j -> f i (get a j)) idx
    | SelectV (idx, a) ->
      for i=0 to length idx - 1 do
        let j = get idx i in
        f i (get a j)
      done
    | Raw (a, len, g) ->
      for i=0 to len a - 1 do
        f i (g a i)
      done

  let of_array a = {len=A.dim a; view=Arr a}

  let map ~f a = {len=length a; view=Map(f, a)}
  let map2 ~f a b =
    if length a <> length b then raise WrongDimension;
    {len=length a; view=Map2(f, a, b)}

  let select ~idx a = {len=A.dim idx; view=Select(idx,a)}
  let select_a ~idx a = {len=Array.length idx; view=SelectA(idx,a)}
  let select_view ~idx a = {len=length idx; view=SelectV(idx,a)}

  let foldi f acc a =
    let acc = ref acc in
    iteri a ~f:(fun i x -> acc := f !acc i x);
    !acc

  let raw ~length ~get a = {len=length a; view=Raw (a, length, get) }

  module type S = sig
    type elt
    val mult : elt t -> elt t -> elt t
    val add : elt t -> elt t -> elt t
    val sum : elt t -> elt
    val prod : elt t -> elt
    val add_scalar : elt t -> x:elt -> elt t
    val mult_scalar : elt t -> x:elt -> elt t
  end

  module Int = struct
    type elt = int
    let add a b = map2 ~f:(+) a b
    let mult a b = map2 ~f:( * ) a b
    let sum a = foldi (fun acc _ x -> acc+x) 0 a
    let prod a = foldi (fun acc _ x -> acc*x) 1 a
    let add_scalar a ~x = map ~f:(fun y -> x+y) a
    let mult_scalar a ~x = map ~f:(fun y -> x*y) a
  end

  module Float = struct
    type elt = float
    let add a b = map2 ~f:(+.) a b
    let mult a b = map2 ~f:( *. ) a b
    let sum a = foldi (fun acc _ x -> acc+.x) 0. a
    let prod a = foldi (fun acc _ x -> acc*.x) 1. a
    let add_scalar a ~x = map ~f:(fun y -> x+.y) a
    let mult_scalar a ~x = map ~f:(fun y -> x*.y) a
  end

  let to_array ?res ?kind a =
    let res = match res, kind with
      | Some r, None ->
        if A.dim r <> length a then raise WrongDimension;
        r
      | None, Some kind -> A.create kind Bigarray.c_layout (length a)
      | None, None
      | Some _, Some _ -> invalid_arg "View.to_array"
    in
    iteri a ~f:(fun i x -> A.unsafe_set res i x);
    res
end
