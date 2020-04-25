
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Random Generators} *)

open CCShims_
include Random

type state = Random.State.t

type 'a t = state -> 'a
type 'a random_gen = 'a t

let return x _st = x

let flat_map f g st = f (g st) st

let (>>=) g f st = flat_map f g st

let map f g st = f (g st)

let (>|=) g f st = map f g st

let delay f st = f () st

let _choose_array a st =
  if Array.length a = 0 then invalid_arg "CCRandom.choose_array";
  a.(Random.State.int st (Array.length a))

let choose_array a st =
  try Some (_choose_array a st st) with Invalid_argument _ -> None

let choose l =
  let a = Array.of_list l in
  choose_array a

let choose_exn l =
  let a = Array.of_list l in
  fun st -> _choose_array a st st

let choose_return l = _choose_array (Array.of_list l)

exception Pick_from_empty

let pick_list l =
  let n = List.length l in
  if n=0 then raise Pick_from_empty;
  fun st ->
    List.nth l (Random.State.int st n)

(*$Q
  Q.(list small_int) (fun l -> \
    l=[] || List.mem (run (pick_list l)) l)
*)

let pick_array a =
  let n = Array.length a in
  if n=0 then raise Pick_from_empty;
  fun st -> Array.get a (Random.State.int st n)

let int i st = Random.State.int st i

let small_int = int 100

let int_range i j st = i + Random.State.int st (j-i+1)

let float f st = Random.State.float st f

let small_float = float 100.0

let float_range i j st = i +. Random.State.float st (j-.i)

(* TODO: sample functions *)

let replicate n g st =
  let rec aux acc n =
    if n = 0 then acc else aux (g st :: acc) (n-1)
  in aux [] n

(* Sample without replacement using rejection sampling. *)
let sample_without_duplicates (type elt) ~cmp k (rng:elt t) st=
  let module S = Set.Make(struct type t=elt let compare = cmp end) in
  let rec aux s k =
    if k <= 0 then
      S.elements s
    else
      let x = rng st in
      if S.mem x s then
        aux s k
      else
        aux (S.add x s) (k-1)
  in
  if k<=0 then invalid_arg "sample_without_duplicates";
  aux S.empty k

let list_seq l st = List.map (fun f -> f st) l

let split i st =
  if i < 2 then None
  else
    let j = 1 + Random.State.int st (i-1) in
    Some (j, i-j)

let _diff_list ~last l =
  let rec diff_list acc = function
    | [a] -> Some ( (last - a)::acc )
    | a::( b::_ as r ) -> diff_list ( (b-a)::acc ) r
    | [] -> None
  in
  diff_list [] l


(* Partition of an int into [len] integers uniformly.
   We first sample (len-1) points from the set {1,..i-1} without replacement.
   We sort these points and add back 0 and i, we have thus
   x_0 = 0 < x_1 < x_2 < ... < x_{len-1} < i = x_{len}.
   If we define, y_k = x_{k+1} - x_{k} for k in 0..(len-1), then by construction
   ∑_k y_k = ∑_k (x_{k+1} - x_k ) = x_{len} - x_0 = i. *)
let split_list i ~len st =
  if len <= 1 then invalid_arg "Random.split_list";
  if i >= len then (
    let xs = sample_without_duplicates ~cmp:compare (len-1) (int_range 1 (i-1)) st in
    _diff_list ~last:i (0::xs)
  ) else
    None

(*$Q
  Q.(pair small_int small_int) (fun (i,j) -> \
    let len, n = 2+min i j, max i j in \
    let l = QCheck.Gen.generate1 (split_list n ~len) in \
    match l with None -> true | Some l -> l<> [] && List.for_all (fun x->x>0) l)
*)

let retry ?(max=10) g st =
  let rec aux n =
    match g st with
      | None when n=0 -> None
      | None -> aux (n-1)  (* retry *)
      | Some _ as res -> res
  in
  aux max

let rec try_successively l st = match l with
  | [] -> None
  | g :: l' ->
    begin match g st with
      | None -> try_successively l' st
      | Some _ as res -> res
    end

let (<?>) a b = try_successively [a;b]

exception Backtrack

let _choose_array_call a f st =
  try
    f (_choose_array a st)
  with Invalid_argument _ -> raise Backtrack

let fix ?(sub1=[]) ?(sub2=[]) ?(subn=[]) ~base fuel st =
  let sub1 = Array.of_list sub1
  and sub2 = Array.of_list sub2
  and subn = Array.of_list subn in
  (* recursive function with fuel *)
  let rec make fuel st =
    if fuel=0 then raise Backtrack
    else if fuel=1 then base st
    else
      _try_otherwise 0
        [| _choose_array_call sub1 (fun f -> f (make (fuel-1)) st)
         ;  _choose_array_call sub2
             (fun f ->
                match split fuel st with
                  | None -> raise Backtrack
                  | Some (i,j) -> f (make i) (make j) st
             )
         ; _choose_array_call subn
             (fun (len,f) ->
                let len = len st in
                match split_list fuel ~len st with
                  | None -> raise Backtrack
                  | Some l' ->
                    f (fun st -> List.map (fun x -> make x st) l') st
             )
         ; base (* base case then *)
        |]
  and _try_otherwise i a =
    if i=Array.length a then raise Backtrack
    else try
        a.(i) st
      with Backtrack ->
        _try_otherwise (i+1) a
  in
  make (fuel st) st

let pure x _st = x

let (<*>) f g st = f st (g st)

include CCShimsMkLet_.Make(struct
    type nonrec 'a t = 'a t
    let (>>=) = (>>=)
    let (>|=) = (>|=)
    let monoid_product a1 a2 st = a1 st, a2 st
  end)

let __default_state = Random.State.make_self_init ()

let run ?(st=__default_state) g = g st

let uniformity_test ?(size_hint=10) k rng st =
  let histogram = Hashtbl.create size_hint in
  let add x = let n = try Hashtbl.find histogram x with Not_found -> 0 in
    Hashtbl.replace histogram x (n + 1) in
  let () =
    for _i = 0 to ( k - 1 ) do
      add (rng st)
    done in
  let cardinal = float_of_int (Hashtbl.length histogram) in
  let kf = float_of_int k in
  (* average number of points assuming an uniform distribution *)
  let average = kf /. cardinal in
  (* The number of points is a sum of random variables with binomial distribution *)
  let p = 1. /. cardinal in
  (* The variance of a binomial distribution with average p is *)
  let variance = p *. (1. -. p ) in
  (* Central limit theorem: a confidence interval of 4σ provides a false positive rate
     of 0.00634% *)
  let confidence = 4. in
  let std = confidence *. (sqrt (kf *. variance)) in
  let predicate _key n acc =
    let (<) (a : float) b = Stdlib.(<) a b in
    acc && abs_float (average -. float_of_int n) < std in
  Hashtbl.fold predicate histogram true

(*$T split_list
  run ~st:(QCheck_runner.random_state()) ( uniformity_test 50_000 (split_list 10 ~len:3) )
*)

(*$R
  let open Containers in
  ignore @@ List.random_choose [1;2;3] (Random.get_state())
*)
