
(*
copyright (c) 2013, simon cruanes
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

(** {1 Bidirectional Iterators}

Iterators that can be traversed in both directions *)

type 'a t =
  | Nil
  | Cons of (unit -> 'a t) * 'a * (unit -> 'a t)

let nil = Nil

let ret_nil () = Nil

let insert_before x = function
  | Nil -> Cons (ret_nil, x, ret_nil)
  | Cons (l, y, r) ->
      let rec cur() = 
        Cons (l, x, (fun () -> Cons (cur, y, r)))
      in cur()

let insert_after x = function
  | Nil -> Cons (ret_nil, x, ret_nil)
  | Cons (l, y, r) ->
      let rec cur() = 
        Cons (l, y, (fun () -> Cons (cur, x, r)))
      in cur()

let left = function
  | Nil -> Nil
  | Cons (l, _, _) -> l()

let right = function
  | Nil -> Nil
  | Cons (_, _, r) -> r()

let graft_before ~inner outer =
  match outer with
  | Nil -> inner
  | Cons (l_out, x_out, r_out) ->
      let rec right ret_left inner () = match inner () with
        | Nil -> Cons(ret_left, x_out, r_out)  (* yield x_out *)
        | Cons (_, x_in, r_in) ->
            let rec cur() =
              Cons (ret_left, x_in, right cur r_in)
            in cur()
      and left ret_right inner () = match inner () with
        | Nil -> l_out()  (* yield same as l_out *)
        | Cons (l_in, x_in, _) ->
            let rec cur() =
              Cons (left cur l_in, x_in, ret_right)
            in cur()
      and start() = match inner with
      | Nil -> outer
      | Cons (l, x, r) -> Cons (left start l, x, right start r)
      in
      start()

let graft_after ~inner outer =
  graft_before ~inner (right outer)

let rev = function
  | Nil -> Nil
  | Cons (l, x, r) ->
      Cons (r, x, l)

(** {2 Right-iteration} *)

let rec fold f acc = function
  | Nil -> acc
  | Cons (_, x, l) ->
      let acc = f acc x in
      fold f acc (l ())

let to_rev_list l =
  fold (fun acc x -> x::acc) [] l

let to_list l =
  List.rev (to_rev_list l)

let rec __of_list prev l () = match l with
  | [] -> Nil
  | x::l ->
    let rec cur() =
      Cons (prev, x, __of_list cur l)
    in cur()

let of_list l = __of_list ret_nil l ()

(** {2 Full constructor} *)

let of_lists l x r =
  let rec cur() =
    Cons (__of_list cur l, x, __of_list cur r)
  in cur()

(** {2 Moves} *)

let left_n n b =
  let rec traverse acc n b = match n, b with
  | 0, _
  | _, Nil -> acc, b
  | _, Cons (l, x, _) -> traverse (x::acc) (n-1) (l())
  in traverse [] n b

let right_n n b =
  let rec traverse acc n b = match n, b with
  | 0, _
  | _, Nil -> acc, b
  | _, Cons (_, x, r) -> traverse (x::acc) (n-1) (r())
  in traverse [] n b
