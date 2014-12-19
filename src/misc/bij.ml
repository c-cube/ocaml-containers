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

(** {1 Bijective Serializer/Deserializer} *)

type _ t =
  | Unit : unit t
  | String : string t
  | Int : int t
  | Bool : bool t
  | Float : float t
  | List : 'a t -> 'a list t
  | Many : 'a t -> 'a list t
  | Opt : 'a t -> 'a option t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Quad : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Quint : 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) t
  | Guard : ('a -> bool) * 'a t -> 'a t
  | Map : ('a -> 'b) * ('b -> 'a) * 'b t -> 'a t
  | Switch :  ('a -> string * 'a inject_branch) *
              (string-> 'a extract_branch) -> 'a t
and _ inject_branch =
  | BranchTo : 'b t * 'b -> 'a inject_branch
and _ extract_branch =
  | BranchFrom : 'b t * ('b -> 'a) -> 'a extract_branch

type 'a bij = 'a t

(** {2 Bijection description} *)

let unit_ = Unit
let string_ = String
let int_ = Int
let bool_ = Bool
let float_ = Float
let list_ l = List l
let many l = Many l
let opt t = Opt t
let pair a b = Pair(a,b)
let triple a b c = Triple (a,b,c)
let quad a b c d = Quad (a, b, c, d)
let quint a b c d e = Quint (a, b, c, d, e)
let guard f t = Guard (f, t)

let map ~inject ~extract b = Map (inject, extract, b)
let switch ~inject ~extract = Switch (inject, extract)

(** {2 Exceptions} *)

exception EncodingError of string
  (** Raised when encoding is impossible *)

exception DecodingError of string
  (** Raised when decoding is impossible *)

(** {2 Helpers} *)

let fix f =
  let rec bij = lazy (f bij) in
  Lazy.force bij

let with_version v t =
  map
    ~inject:(fun x -> v, x)
    ~extract:(fun (v', x) ->
      if v = v'
        then x
        else raise (DecodingError ("expected version " ^ v)))
    (pair string_ t)

let array_ m =
  map
    ~inject:(fun a -> Array.to_list a)
    ~extract:(fun l -> Array.of_list l)
    (list_ m)

let hashtbl ma mb =
  map
    ~inject:(fun h -> Hashtbl.fold (fun k v l -> (k,v)::l) h [])
    ~extract:(fun l ->
      let h = Hashtbl.create 5 in
      List.iter (fun (k,v) -> Hashtbl.add h k v) l;
      h)
    (list_ (pair ma mb))
