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

(** {1 Imperative skip-list} *)

type 'a gen = unit -> 'a option

(** Most functions are inspired from
    "A skip list cookbook", William Pugh, 1989. *)

type ('a, 'b) t = {
  mutable data : ('a, 'b) bucket;
  cmp : ('a -> 'a -> int);  (* comparison function *)
  mutable size : int;
} (** A skip list that maps elements of type 'a to elements of type 'b *)
and ('a, 'b) bucket =
  | Init of int * ('a, 'b) bucket array  (* level + first array *)
  | Node of 'a * 'b ref * ('a, 'b) bucket array
  | Nil

(* give a random level between 0 and [maxLevel] *)
let random_level maxLevel =
  let rec iter level =
    if level = maxLevel then level
    else if Random.bool () then iter (level+1)
    else level
  in iter 1

let create ?(maxLevel=4) cmp =
  { data = Init (1, Array.make maxLevel Nil);
    cmp;
    size = 0;
  }

(* level of the list node *)
let level node = match node with
  | Init (n, _) -> n
  | Node (_, _, a) -> Array.length a
  | _ -> assert false

(* check whether the element is lower than k *)
let lower ~cmp node k = match node with
  | Init _ -> assert false
  | Node (k', _, _) -> cmp k' k < 0
  | Nil -> false

let eq ~cmp node k = match node with
  | Init _ -> assert false
  | Node (k', _, _) -> cmp k' k = 0
  | Nil -> false

(** Is the list empty? *)
let is_empty l =
  l.size = 0

let maxLevel l =
  match l.data with
  | Init (_, a) -> Array.length a
  | _ -> assert false

let array_of node =
  match node with
  | Init (_, a) | Node (_, _, a) -> a
  | Nil -> assert false

let clear l =
  l.size <- 0;
  let a = array_of l.data in
  Array.fill a 0 (Array.length a) Nil;
  l.data <- Init (1, a)

(* next element of node, at level [n] *)
let next node n =
  (array_of node).(n)

(** Find given key in the list, or Not_found *)
let find l k =
  let cmp = l.cmp in
  let rec search x n =
    if n < 0 then peek_last x
    else
      let x' = next x n in
      match x' with
      | Nil -> search x (n-1)
      | Node (k', v, _) ->
        let c = cmp k' k in
        if c = 0 then !v
        else if c < 0 then search x' n
        else search x (n-1)
      | Init _ -> assert false
  and peek_last x =
    match next x 0 with
    | Node (k', v, _) when cmp k k' = 0 -> !v
    | _ -> raise Not_found
  in
  search l.data (level l.data - 1)

let mem l k =
  try ignore (find l k); true
  with Not_found -> false

(** Add [k -> v] to the list [l] *)
let add l k v =
  let cmp = l.cmp in
  let x = ref l.data in
  let update = Array.make (maxLevel l) (array_of l.data) in
  (* find which pointers to update *)
  for i = level l.data - 1 downto 0 do
    while lower ~cmp (next !x i) k do x := next !x i done;
    update.(i) <- array_of !x;
  done;
  x := next !x 0;
  match !x with
  | Node (k', v', _) when cmp k k' = 0 ->
    v' := v  (* replace mapping of [k] *)
  | _ ->
    let new_level = random_level (maxLevel l) in
    l.size <- l.size + 1;
    (* update level of the list *)
    (if new_level > level l.data then
      begin
        for i = level l.data to new_level - 1 do
          update.(i) <- array_of l.data
        done;
        l.data <- Init (new_level, array_of l.data)
      end);
    (* create node and insert it *)
    let a = Array.make new_level Nil in
    x := Node (k, ref v, a);
    for i = 0 to new_level - 1 do
      a.(i) <- update.(i).(i);
      update.(i).(i) <- !x
    done

(** Removal of the given key *)
let remove l k =
  let cmp = l.cmp in
  let x = ref l.data in
  let update = Array.make (maxLevel l) (array_of l.data) in
  (* find which pointers to update *)
  for i = level l.data - 1 downto 0 do
    while lower ~cmp (next !x i) k do x := next !x i done;
    update.(i) <- array_of !x;
  done;
  x := next !x 0;
  if eq ~cmp !x k then begin
    (* found the node containing [k] *)
    for i = 0 to level l.data - 1 do
      if update.(i).(i) == !x then update.(i).(i) <- next !x i
    done;
    (* update level of list *)
    l.size <- l.size - 1;
    while level l.data > 1 && next l.data (level l.data - 1) = Nil
      do l.data <- Init (level l.data - 1, array_of l.data) done
  end

let length l = l.size

(** Iterator on the skip list *)
let gen l =
  let x = ref (next l.data 0) in
  fun () ->
    match !x with
    | Nil -> None
    | Init _ -> assert false
    | Node (k, v, a) ->
      x := a.(0);
      Some (k, !v)

let rec gen_iter f g = match g() with
  | None -> ()
  | Some x -> f x; gen_iter f g

(** Add content of the iterator to the list *)
let of_gen l gen =
  gen_iter (fun (k,v) -> add l k v) gen
