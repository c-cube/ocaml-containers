
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Immutable Arrays} *)

(* TODO: transient API? for batch modifications *)

(*$inject let print_array f a = to_list a |> Array.of_list |> Q.Print.(array f)
*)

type 'a t = 'a array

let empty = [| |]

let length = Array.length

let singleton x = [| x |]

let doubleton x y = [| x; y |]

let make n x = Array.make n x

let init n f = Array.init n f

let get = Array.get

let set a n x =
  let a' = Array.copy a in
  a'.(n) <- x;
  a'

(*$= set & ~printer:(print_array Q.Print.int)
  (of_list [0]) (set (of_list [5]) 0 0)
  (of_list [1; 3; 4; 5]) (set (of_list [1; 2; 4; 5]) 1 3)
*)

let sub = Array.sub (* Would this not be better implemented with CCArray_slice *)

let map = Array.map

let mapi = Array.mapi

let append a b =
  let na = length a in
  Array.init (na + length b)
    (fun i -> if i < na then a.(i) else b.(i-na))

(*$= append & ~printer:(print_array Q.Print.int)
  empty (append empty empty)
  (of_list [1; 2; 3]) (append empty (of_list [1; 2; 3]))
  (of_list [1; 2; 3]) (append (of_list [1; 2; 3]) empty)
  (of_list [3; 1; 4; 1; 5]) (append (of_list [3; 1]) (of_list [4; 1; 5]))
*)

let iter = Array.iter

let iteri = Array.iteri

let fold = Array.fold_left

let foldi f acc a =
  let n = ref 0 in
  Array.fold_left
    (fun acc x ->
       let acc = f acc !n x in
       incr n;
       acc)
    acc a

(*$= foldi & ~printer:Q.Print.(list (pair int string))
  ([2, "baz"; 1, "bar"; 0, "foo"]) (foldi (fun l i a -> (i, a) :: l) [] (of_list ["foo"; "bar"; "baz"]))
*)

exception ExitNow

let for_all p a =
  try
    Array.iter (fun x -> if not (p x) then raise ExitNow) a;
    true
  with ExitNow -> false

(*$= for_all & ~printer:Q.Print.bool
  true (for_all (fun _ -> false) empty)
  false (for_all (fun _ -> false) (singleton 3))
  true (for_all (fun n -> n mod 2 = 0) (of_list [2; 4; 8]))
  false (for_all (fun n -> n mod 2 = 0) (of_list [2; 4; 5; 8]))
*)

let exists p a =
  try
    Array.iter (fun x -> if p x then raise ExitNow) a;
    false
  with ExitNow -> true

(*$= exists & ~printer:Q.Print.bool
  false (exists (fun _ -> true) empty)
  true (exists (fun _ -> true) (singleton 3))
  false (exists (fun _ -> false) (singleton 3))
  false (exists (fun n -> n mod 2 = 1) (of_list [2; 4; 8]))
  true (exists (fun n -> n mod 2 = 1) (of_list [2; 4; 5; 8]))
*)

(*$Q
  Q.(list bool) (fun l -> let a = of_list l in not @@ exists (fun b -> b) a = for_all not a)
  Q.(list bool) (fun l -> let a = of_list l in not @@ for_all (fun b -> b) a = exists not a)
*)

(*$Q
 Q.(list bool) (fun l -> exists (fun b -> b) (of_list l) = List.fold_left (||) false l)
 Q.(list bool) (fun l -> for_all (fun b -> b) (of_list l) = List.fold_left (&&) true l)
 *)

(** {2 Conversions} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let of_list = Array.of_list

let to_list = Array.to_list

let of_array_unsafe a = a (* careful with that axe, Eugene *)

let to_iter a k = iter k a

let of_iter s =
  let l = ref [] in
  s (fun x -> l := x :: !l);
  Array.of_list (List.rev !l)

(*$Q
  Q.(list int) (fun l -> \
    let g = Iter.of_list l in \
    of_iter g |> to_iter |> Iter.to_list = l)
*)

let rec gen_to_list_ acc g = match g() with
  | None -> List.rev acc
  | Some x -> gen_to_list_ (x::acc) g

let of_gen g =
  let l = gen_to_list_ [] g in
  Array.of_list l

let to_gen a =
  let i = ref 0 in
  fun () ->
    if !i < Array.length a then (
      let x = a.(!i) in
      incr i;
      Some x
    ) else None

(*$Q
  Q.(list int) (fun l -> \
    let g = Gen.of_list l in \
    of_gen g |> to_gen |> Gen.to_list = l)
*)

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

let pp ?(pp_start=fun _ () -> ()) ?(pp_stop=fun _ () -> ())
    ?(pp_sep=fun out () -> Format.fprintf out ",@ ") pp_item out a =
  pp_start out ();
  for k = 0 to Array.length a - 1 do
    if k > 0 then pp_sep out ();
    pp_item out a.(k)
  done;
  pp_stop out ()
