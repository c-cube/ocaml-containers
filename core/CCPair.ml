
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

(** {1 Tuple Functions} *)

type ('a,'b) t = ('a * 'b)

let map1 f (x,y) = f x,y

let map2 f (x,y) = x,f y

let map f g (x,y) = f x, g y

let map_same f (x,y) = f x, f y

let swap (x,y) = y, x

let (<<<) = map1

let (>>>) = map2

let ( *** ) = map

let ( &&& ) f g x = f x, g x

let merge f (x,y) = f x y

let equal f g (x1,y1) (x2,y2) = f x1 x2 && g y1 y2

let compare f g (x1,y1) (x2,y2) =
  let c = f x1 x2 in
  if c <> 0 then c else g y1 y2

type 'a printer = Buffer.t -> 'a -> unit

let pp pp_x pp_y buf (x,y) =
  Printf.bprintf buf "(%a, %a)" pp_x x pp_y y
