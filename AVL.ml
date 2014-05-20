
(*
copyright (c) 2013-2014, simon cruanes
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

(** {1 AVL trees}

See https://en.wikipedia.org/wiki/AVL_tree *)

type ('a,'b) t =
  | Empty
  | Node of ('a,'b) t * 'a * 'b * ('a,'b) t * int

type 'a comparator = 'a -> 'a -> int

let empty = Empty

let _height = function
  | Empty -> 0
  | Node (_, _, _, _, h) -> h

let _balance l r = _height l - _height r

(* build the tree *)
let _make l x y r =
  Node (l, x, y, r, 1 + max (_height l) (_height r))

let singleton k v = _make empty k v empty

(* balance tree [t] *)
let _rebalance t = match t with
  | Empty -> t
  | Node (l, k1, v1, r, _) ->
      let b = _balance l r in
      if b = 2
      then (* left cases: left tree is too deep *)
        match l with
        | Empty -> assert false
        | Node (ll, k2, v2, lr, _) ->
          if _balance ll lr = -1
            then (* left-right *)
              match lr with
              | Empty -> assert false
              | Node (lrl, k3, v3, lrr, _) ->
                  _make
                    (_make ll k2 v2 lrl)
                    k3 v3
                    (_make lrr k1 v1 r)
            else (* left-left *)
              _make
                ll k2 v2
                (_make lr k1 v1 r)
      else if b = -2 (* right cases: symetric *)
      then match r with
        | Empty -> assert false
        | Node (rl, k2, v2, rr, _) ->
          if _balance rl rr = 1
            then (* right-left *)
              match rl with
              | Empty -> assert false
              | Node (rll, k3, v3, rlr, _) ->
                  _make
                    (_make l k1 v1 rll)
                    k3 v3
                    (_make rll k2 v2 rlr)
            else (* right-right *)
              _make
                (_make l k1 v1 rl)
                k2 v2 rr
      else t

let _make_balance l k v r =
  _rebalance (_make l k v r)

let rec fold f acc t = match t with
  | Empty -> acc
  | Node (l, x, y, r, _) ->
      let acc = fold f acc l in
      let acc = f acc x y in
      fold f acc r

let rec for_all p t = match t with
  | Empty -> true
  | Node (l, x, y, r, _) ->
      p x y && for_all p l && for_all p r

let rec exists p t = match t with
  | Empty -> false
  | Node (l, x, y, r, _) ->
      p x y || exists p l || exists p r

let rec insert ~cmp t k v = match t with
  | Empty -> _make empty k v empty
  | Node (l, k1, v1, r, _) ->
      let c = cmp k k1 in
      if c < 0
        then _make_balance (insert ~cmp l k v) k1 v1 r
      else if c = 0
        then _make l k v r
      else _make_balance l k1 v1 (insert ~cmp r k v)

(* remove the maximal value in the given tree (the only which only has a left
  child), and return its key/value pair *)
let rec _remove_max t = match t with
  | Empty -> assert false
  | Node (l, k, v, Empty, _) ->
      l, k, v
  | Node (l, k, v, r, _) ->
      let r', k', v' = _remove_max r in
      _make_balance l k v r', k', v'

exception NoSuchElement

let remove ~cmp t key =
  let rec _remove t = match t with
    | Empty -> raise NoSuchElement
    | Node (l, k, v, r, _) ->
        let c = cmp key k in
        if c < 0
          then _make_balance (_remove l) k v r
        else if c > 0
          then _make_balance l k v (_remove r)
        else
          (* interesting case: the node to remove is this one. We need
            to find a replacing node, unless [l] is empty  *)
          match l with
          | Empty -> r
          | Node _ ->
              let l', k', v' = _remove_max l in
              _make_balance l' k' v' r
  in
  try _remove t
  with NoSuchElement -> t  (* element not found *)

let update ~cmp t key f = failwith "update: not implemented"

let rec find_exn ~cmp t key = match t with
  | Empty -> raise Not_found
  | Node (l, k, v, r, _) ->
      let c = cmp key k in
      if c < 0 then find_exn ~cmp l key
      else if c > 0 then find_exn ~cmp r key
      else v

let find ~cmp t key =
  try Some (find_exn ~cmp t key)
  with Not_found -> None

(* add k,v as strictly maximal element to t. [t] must not contain
  any key >= k *)
let rec _add_max k v t = match t with
  | Empty -> singleton k v
  | Node (l, k', v', r, _) ->
      _make_balance l k' v' (_add_max k v r)
and
(* same for minimal value *)
_add_min k v t = match t with
  | Empty -> singleton k v
  | Node (l, k', v', r, _) ->
      _make_balance (_add_min k v l) k' v' r

(* same as [_make] but doesn't assume anything about balance *)
let rec _join l k v r =
  match l, r with
  | Empty, _ -> _add_min k v r
  | _, Empty -> _add_max k v l
  | Node (ll, k1, v1, lr, h1), Node (rl, k2, v2, rr, h2) ->
      if h1 + 1 < h2
        then (* r is much bigger. join l with rl *)
          _make_balance (_join l k v rl) k2 v2 rr
      else if h1 > h2 + 1
        then
          _make_balance ll k1 v1 (_join lr k v r)
      else (* balance uneeded *)
        _make l k v r

(* concat t1 and t2, where all keys of [t1] are smaller than
  those of [t2] *)
let _concat t1 t2 = match t1, t2 with
  | Empty, t
  | t, Empty -> t
  | _ ->
      let t1', k, v = _remove_max t1 in
      _join t1' k v t2

let rec split ~cmp t key = match t with
  | Empty -> empty, None, empty
  | Node (l, k, v, r, _) ->
      let c = cmp key k in
      if c < 0
        then
          let ll, result, lr = split ~cmp l key in
          ll, result, _join lr k v r
      else if c > 0
        then
          let rl, result, rr = split ~cmp r key in
          _join l k v rl, result, rr
      else
        l, Some v, r

(* if k = Some v, join l k v r, else concat l v *)
let _concat_or_join l k result r = match result with
  | None -> _concat l r
  | Some v -> _join l k v r

let rec merge ~cmp f t1 t2 = match t1, t2 with
  | Empty, Empty -> empty
  | Node (l1, k1, v1, r1, h1), _ when h1 >= _height t2 ->
      let l2, result2, r2 = split ~cmp t2 k1 in
      let result = f k1 (Some v1) result2 in
      let l = merge ~cmp f l1 l2 in
      let r = merge ~cmp f r1 r2 in
      _concat_or_join l k1 result r
  | _, Node (l2, k2, v2, r2, _) ->
      let l1, result1, r1 = split ~cmp t1 k2 in
      let result = f k2 result1 (Some v2) in
      let l = merge ~cmp f l1 l2 in
      let r = merge ~cmp f r1 r2 in
      _concat_or_join l k2 result r
  | _, Empty -> assert false  (* h1 < heigth h2?? *)

(* invariant: balanced *)
let rec invariant_balance t = match t with
  | Empty -> true
  | Node (l, _, _, r, _) ->
      abs (_balance l r) < 2
      && invariant_balance l && invariant_balance r

(* invariant: search tree *)
let rec invariant_search ~cmp t = match t with
  | Empty -> true
  | Node (l, x, _, r, _) ->
      invariant_search ~cmp l &&
      invariant_search ~cmp r &&
      for_all (fun x' _ -> cmp x' x < 0) l &&
      for_all (fun x' _ -> cmp x' x > 0) r

let of_list ~cmp l =
  List.fold_left (fun acc (x,y) -> insert ~cmp acc x y) empty l

let to_list t =
  let rec aux acc t = match t with
  | Empty -> acc
  | Node (l, k, v, r, _) ->
      let acc = aux acc r in
      let acc = (k,v)::acc in
      aux acc l
  in aux [] t

(** {2 Iterators} *)

module type ITERATOR = sig
  type 'a iter

  val after : cmp:'a comparator -> ('a,'b) t -> 'a -> ('a * 'b) iter
  val before : cmp:'a comparator -> ('a,'b) t -> 'a -> ('a * 'b) iter
  val iter : ('a,'b) t -> ('a * 'b) iter
  val add : cmp:'a comparator -> ('a,'b) t -> ('a * 'b) iter -> ('a,'b) t
end

type ('a,'b) explore =
  | Yield of 'a * 'b
  | Explore of ('a, 'b) t

exception EndOfIter

(* push the tree [t] on the stack [s] *)
let _push t s = match t with
  | Empty -> s
  | Node _ -> Explore t :: s

(* push [t] on [s] with swapped children *)
let _push_swap t s = match t with
  | Empty -> s
  | Node (l, k, v, r,h) ->
      Explore (Node(r,k,v,l,h)) :: s

let _yield k v l = Yield (k,v) :: l

let _has_next = function
  | [] -> false
  | _::_ -> true

(* next key,value to yield *)
let rec _pop l = match l with
  | [] -> raise EndOfIter
  | (Yield (k,v))::l' -> k, v, l'
  | (Explore Empty) :: _ -> assert false
  | (Explore Node(l, k, v, r, _)::l') ->
      _pop (_push l (_yield k v (_push r l')))

(* return the initial stack of trees to explore, that
  are all "after" key (included) *)
let rec _after ~cmp stack t key = match t with
  | Empty -> stack
  | Node (l, k, v, r, _) ->
      let c = cmp key k in
      if c = 0 then _yield k v stack
      else if c < 0 then _yield k v (_push r stack)
      else _after ~cmp stack r key

(* same as [_after] but for the range before *)
let rec _before~cmp stack t key = match t with
  | Empty -> stack
  | Node (l, k, v, r, _) ->
      let c = cmp key k in
      if c = 0 then _yield k v stack
      else if c < 0 then _before ~cmp stack l key
      else _yield k v (_push_swap l stack)

module KList = struct
  type 'a t = [ `Nil | `Cons of 'a * (unit -> 'a t) ]

  let rec _next (l:('a,'b) explore list) () : ('a*'b) t = match l with
    | [] -> `Nil
    | _::_ ->
        let k, v, l' = _pop l in
        `Cons ((k,v), _next l')

  let iter t = _next (_push t []) ()

  let rec add ~cmp t (l:'a t) = match l with
    | `Nil -> t
    | `Cons ((k,v), l') ->
        add ~cmp (insert ~cmp t k v) (l' ())

  let after ~cmp t key = _next (_after ~cmp [] t key) ()

  let before ~cmp t key = _next (_before ~cmp [] t key) ()
end

module Gen = struct
  type 'a t = unit -> 'a option

  let _gen stack =
    let stack = ref stack in
    let rec next () =
      match !stack with
      | [] -> None
      | l ->
          let k, v, stack' = _pop l in
          stack := stack';
          Some (k, v)
    in next

  let iter t = _gen (_push t [])

  let rec add ~cmp t gen =
    match gen() with
    | None -> t
    | Some (k,v) -> add ~cmp (insert ~cmp t k v) gen

  let after ~cmp t key = _gen (_after ~cmp [] t key)
  let before ~cmp t key = _gen (_before ~cmp [] t key)
end
