
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Functional queues (fifo)} *)

type 'a iter = ('a -> unit) -> unit
type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option

type 'a t = {
  hd : 'a list;
  tl : 'a list;
} (** Queue containing elements of type 'a *)

let empty = {
  hd = [];
  tl = [];
}

(* invariant: if hd=[], then tl=[] *)
let make_ hd tl = match hd with
  | [] -> {hd=List.rev tl; tl=[] }
  | _::_ -> {hd; tl; }

let list_is_empty = function
  | [] -> true
  | _::_ -> false

let is_empty q = list_is_empty q.hd

let push x q = make_ q.hd (x :: q.tl)

let snoc q x = push x q

let peek_exn q =
  match q.hd with
    | [] -> assert (list_is_empty q.tl); invalid_arg "Queue.peek"
    | x::_ -> x

let peek q = match q.hd with
  | [] -> None
  | x::_ -> Some x

let pop_exn q =
  match q.hd with
    | [] -> assert (list_is_empty q.tl); invalid_arg "Queue.peek"
    | x::hd' ->
      let q' = make_ hd' q.tl in
      x, q'

let pop q =
  try Some (pop_exn q)
  with Invalid_argument _ -> None

(*$Q
  Q.(list small_int) (fun l -> \
    let q = of_list l in \
    equal CCInt.equal (Gen.unfold pop q |> of_gen) q)
*)

let junk q =
  try
    let _, q' = pop_exn q in
    q'
  with Invalid_argument _ -> q

let map f q = { hd=List.map f q.hd; tl=List.map f q.tl; }

let rev q = make_ q.tl q.hd

(*$Q
  Q.(list small_int) (fun l -> \
    equal CCInt.equal (of_list l |> rev) (of_list (List.rev l)))
  Q.(list small_int) (fun l -> \
    let q = of_list l in \
    equal CCInt.equal q (q |> rev |> rev))
*)

let length q = List.length q.hd + List.length q.tl

(*$Q
  Q.(list small_int)(fun l -> \
    length (of_list l) = List.length l)
*)

(*$Q
  Q.(list small_int)(fun l -> \
    equal CCInt.equal (of_list l) (List.fold_left snoc empty l))
*)

let fold f acc q =
  let acc' = List.fold_left f acc q.hd in
  List.fold_right (fun x acc -> f acc x) q.tl acc'

(* iterate on a list in reverse order *)
let rec rev_iter_ f l = match l with
  | [] -> ()
  | x :: tl -> rev_iter_ f tl; f x

let iter f q =
  List.iter f q.hd;
  rev_iter_ f q.tl

let to_list q = fold (fun acc x->x::acc) [] q |> List.rev

let add_list q l = List.fold_left snoc q l
let of_list l = add_list empty l

let to_iter q = fun k -> iter k q

let add_iter q seq =
  let q = ref q in
  seq (fun x -> q := push x !q);
  !q

let of_iter s = add_iter empty s

let of_seq = of_iter
let to_seq = to_iter
let add_seq = add_iter

(*$Q
  Q.(list small_int) (fun l -> \
    equal CCInt.equal \
      (of_seq (Iter.of_list l)) \
      (of_list l))
  Q.(list small_int) (fun l -> \
    l = (of_list l |> to_seq |> Iter.to_list))
*)

let add_std_seq q l = add_iter q (fun k -> Seq.iter k l)
let of_std_seq l = add_std_seq empty l

let to_std_seq q =
  let rec aux1 l () = match l with
    | [] -> aux2 (List.rev q.tl) ()
    | x :: tl -> Seq.Cons (x, aux1 tl)
  and aux2 l () = match l with
    | [] -> Seq.Nil
    | x :: tl -> Seq.Cons (x, aux2 tl)
  in
  aux1 q.hd

let rec klist_iter_ k f = match k() with
  | `Nil -> ()
  | `Cons (x,tl) -> f x; klist_iter_ tl f

let add_klist q l = add_seq q (klist_iter_ l)
let of_klist l = add_klist empty l

let to_klist q =
  let rec aux1 l () = match l with
    | [] -> aux2 (List.rev q.tl) ()
    | x :: tl -> `Cons (x, aux1 tl)
  and aux2 l () = match l with
    | [] -> `Nil
    | x :: tl -> `Cons (x, aux2 tl)
  in
  aux1 q.hd

let rec gen_iter g f = match g() with
  | None -> ()
  | Some x -> f x; gen_iter g f

let add_gen q g = add_seq q (gen_iter g)
let of_gen g = add_gen empty g

let to_gen q =
  let st = ref (`Left q.hd) in
  let rec aux () = match !st with
    | `Stop -> None
    | `Left [] -> st := `Right q.tl; aux()
    | `Left (x::tl) -> st := `Left tl; Some x
    | `Right [] -> st := `Stop; None
    | `Right (x::tl) -> st := `Right tl; Some x
  in
  aux

let rec klist_equal eq l1 l2 = match l1(), l2() with
  | `Nil, `Nil -> true
  | `Nil, _
  | _, `Nil -> false
  | `Cons (x1,l1'), `Cons (x2,l2') ->
    eq x1 x2 && klist_equal eq l1' l2'

let equal eq q1 q2 = klist_equal eq (to_klist q1) (to_klist q2)

(*$Q
  Q.(pair (list small_int)(list small_int)) (fun (l1,l2) -> \
    equal CCInt.equal (of_list l1)(of_list l2) = (l1=l2))
*)

let append q1 q2 =
  add_seq q1
    (fun yield ->
       to_seq q2 yield)

(*$Q
  Q.(pair (list small_int)(list small_int)) (fun (l1,l2) -> \
    equal CCInt.equal \
      (append (of_list l1)(of_list l2)) \
      (of_list (List.append l1 l2)))
*)

module Infix = struct
  let (>|=) q f = map f q
  let (<::) = snoc
  let (@) = append
end

include Infix

(** {2 IO} *)

let pp ?(sep=fun out () -> Format.fprintf out ",@ ") pp_item out l =
  let first = ref true in
  iter
    (fun x ->
       if !first then first := false else sep out ();
       pp_item out x)
    l
