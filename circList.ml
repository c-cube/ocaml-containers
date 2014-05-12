
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

(** {1 Functional Circular List}

Those are infinite lists that are built from a finite list of
elements, and cycles through them. *)

type 'a t = {
  front : 'a list;
  f_len : int;
  rear : 'a list;
  r_len : int;
}
(* invariant: if front=[] then rear=[] *)

let make f f_len r r_len = match f with
  | [] ->
      assert (f_len = 0);
      { front=List.rev r; f_len=r_len; rear=[]; r_len=0; }
  | _::_ -> {front=f; f_len; rear=r; r_len; }

let singleton x = make [x] 1 [] 0

let of_list l = make l (List.length l) [] 0

let length l = l.f_len + l.r_len

let cons x l = make (x::l.front) (l.f_len+1) l.rear l.r_len

let snoc l x = make l.front l.f_len (x::l.rear) (l.r_len+1)

let next l = match l.front with
  | [] -> assert false
  | x::l' ->
      x, make l' (l.f_len-1) l.rear l.r_len

let rev l = make l.rear l.r_len l.front l.f_len

let find p l =
  let rec _find p i l =
    if i = 0 then None
    else
      let x, l' = next l in
      if p x then Some x else _find p (i-1) l'
  in
  _find p (length l) l

let mem ?(eq=fun x y -> x=y) x l =
  match find (eq x) l with
  | None -> false
  | Some _ -> true

let exists p l = match find p l with
  | None -> false
  | Some _ -> true

let for_all p l =
  let rec _check i l =
    i = 0 ||
    ( let x, l' = next l in
      p x && _check (i-1) l')
  in
  _check (length l) l

let fold f acc l =
  let rec _fold acc i l =
    if i=0 then acc
    else
      let x, l' = next l in
      let acc = f acc x in
      _fold acc (i-1) l'
  in
  _fold acc (length l) l

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit

let gen l =
  let l = ref l in
  fun () ->
    let x, l' = next !l in
    l := l';
    Some x

let seq l k =
  let r' = lazy (List.rev l.rear) in
  while true do
    List.iter k l.front;
    List.iter k (Lazy.force r')
  done
