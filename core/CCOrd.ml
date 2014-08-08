
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

(** {1 Comparisons} *)

type 'a t = 'a -> 'a -> int
(** Comparison (total ordering) between two elements, that returns an int *)

let compare = Pervasives.compare

let opp f x y = - (f x y)

let equiv i j =
  if i<0 then j<0
  else if i>0 then j>0
  else j=0

let int_ (x:int) y = Pervasives.compare x y
let string_ (x:string) y = Pervasives.compare x y
let bool_ (x:bool) y = Pervasives.compare x y
let float_ (x:float) y = Pervasives.compare x y

(** {2 Lexicographic Combination} *)

let (<?>) c (ord,x,y) =
  if c = 0
    then ord x y
    else c

let pair o_x o_y (x1,y1) (x2,y2) =
  let c = o_x x1 x2 in
  if c = 0
    then o_y y1 y2
    else c

let triple o_x o_y o_z (x1,y1,z1) (x2,y2,z2) =
  let c = o_x x1 x2 in
  if c = 0
    then
      let c' = o_y y1 y2 in
      if c' = 0
        then o_z z1 z2
        else c'
    else c

let rec list_ ord l1 l2 = match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x1::l1', x2::l2' ->
      let c = ord x1 x2 in
      if c = 0
        then list_ ord l1' l2'
        else c

let array_ ord a1 a2 =
  let rec aux i =
    if i = Array.length a1
      then if Array.length a1 = Array.length a2 then 0
      else -1
    else if i = Array.length a2
      then 1
      else
        let c = ord a1.(i) a2.(i) in
        if c = 0
          then aux (i+1) else c
  in
  aux 0

let map f ord a b = ord (f a) (f b)
