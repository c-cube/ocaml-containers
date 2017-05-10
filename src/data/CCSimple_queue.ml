
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Functional queues (fifo)} *)

type 'a t = {
  hd : 'a list;
  tl : 'a list;
} (** Queue containing elements of type 'a *)

let empty = {
  hd = [];
  tl = [];
}

(* invariant: if hd=[], then tl=[] *)
let _make hd tl = match hd with
  | [] -> {hd=List.rev tl; tl=[] }
  | _::_ -> {hd; tl; }

let is_empty q = q.hd = []

let push x q = {q with tl = x :: q.tl; }

let snoc q x = push x q

let peek_exn q =
  match q.hd with
    | [] -> assert (q.tl = []); raise (Invalid_argument "Queue.peek")
    | x::_ -> x

let peek q = match q.hd with
  | [] -> None
  | x::_ -> Some x

let pop_exn q =
  match q.hd with
    | [] -> assert (q.tl = []); raise (Invalid_argument "Queue.peek")
    | x::hd' ->
      let q' = _make hd' q.tl in
      x, q'

let pop q =
  try Some (pop_exn q)
  with Invalid_argument _ -> None

let junk q =
  try
    let _, q' = pop_exn q in
    q'
  with Invalid_argument _ -> q

(** Append two queues. Elements from the second one come
    after elements of the first one *)
let append q1 q2 =
  { hd=q1.hd;
    tl=q2.tl @ (List.rev_append q2.hd q1.tl);
  }

let map f q = { hd=List.map f q.hd; tl=List.map f q.tl; }

let size q = List.length q.hd + List.length q.tl

let (>|=) q f = map f q

let fold f acc q =
  let acc' = List.fold_left f acc q.hd in
  List.fold_right (fun x acc -> f acc x) q.tl acc'

let iter f q = fold (fun () x -> f x) () q

type 'a sequence = ('a -> unit) -> unit

let to_seq q = fun k -> iter k q

let of_seq seq =
  let q = ref empty in
  seq (fun x -> q := push x !q);
  !q
