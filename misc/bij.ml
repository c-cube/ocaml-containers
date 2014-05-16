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

(** {2 Translations} *)

module TrBencode = struct
  module B = Bencode

  let rec encode: type a. bij:a t -> a -> B.t =
  fun ~bij x -> match bij, x with
  | Unit, () -> B.I 0
  | String, s -> B.S s
  | Int, i -> B.I i
  | Float, f -> B.S (string_of_float f)
  | Bool, b -> B.I (if b then 1 else 0)
  | List bij', l ->
    let l' = List.map (fun x -> encode ~bij:bij' x) l in
    B.L l'
  | Many bij', [] -> raise (EncodingError "many: got empty list")
  | Many bij', l -> 
    let l' = List.map (fun x -> encode ~bij:bij' x) l in
    B.L l'
  | Opt bij', None -> B.L []
  | Opt bij', Some x -> B.L [encode ~bij:bij' x]
  | Pair (bija, bijb), (a, b) ->
    B.L [encode ~bij:bija a; encode ~bij:bijb b]
  | Triple (bija, bijb, bijc), (a, b, c) ->
    B.L [encode ~bij:bija a; encode ~bij:bijb b; encode ~bij:bijc c]
  | Quad (bija, bijb, bijc, bijd), (a, b, c, d) ->
    B.L [encode ~bij:bija a; encode ~bij:bijb b;
         encode ~bij:bijc c; encode ~bij:bijd d]
  | Quint (bija, bijb, bijc, bijd, bije), (a, b, c, d, e) ->
    B.L [encode ~bij:bija a; encode ~bij:bijb b;
         encode ~bij:bijc c; encode ~bij:bijd d;
         encode ~bij:bije e]
  | Guard (check, bij'), x ->
    if not (check x) then raise (EncodingError "check failed");
    encode ~bij:bij' x
  | Map (inject, _, bij'), x ->
    encode ~bij:bij' (inject x)
  | Switch (inject, _), x ->
    let key, BranchTo (bij',y) = inject x in
    B.D (B.SMap.singleton key (encode ~bij:bij' y))
  
  let rec decode: type a. bij:a t -> B.t -> a
  = fun ~bij b -> match bij, b with
  | Unit, B.I 0 -> ()
  | String, B.S s -> s
  | Int, B.I i -> i
  | Float, B.S s ->
    begin try
      let f = float_of_string s in
      f
    with Failure _ ->
      raise (DecodingError "expected float")
    end
  | Bool, B.I 0 -> false
  | Bool, B.I _ -> true
  | List bij', B.L l ->
    List.map (fun b -> decode ~bij:bij' b) l
  | Many bij', B.L [] ->
    raise (DecodingError "expected nonempty list")
  | Many bij', B.L l ->
    List.map (fun b -> decode ~bij:bij' b) l
  | Opt bij', B.L [] -> None
  | Opt bij', B.L [x] -> Some (decode ~bij:bij' x)
  | Opt bij', B.L _ ->
    raise (DecodingError "expected [] or [_]")
  | Pair (bija, bijb), B.L [a; b] ->
    decode ~bij:bija a, decode ~bij:bijb b
  | Triple (bija, bijb, bijc), B.L [a; b; c] ->
    decode ~bij:bija a, decode ~bij:bijb b, decode ~bij:bijc c
  | Quad (bija, bijb, bijc, bijd), B.L [a; b; c; d] ->
    decode ~bij:bija a, decode ~bij:bijb b,
    decode ~bij:bijc c, decode ~bij:bijd d
  | Quint (bija, bijb, bijc, bijd, bije), B.L [a; b; c; d; e] ->
    decode ~bij:bija a, decode ~bij:bijb b,
    decode ~bij:bijc c, decode ~bij:bijd d,
    decode ~bij:bije e
  | Guard (check, bij'), x ->
    let y = decode ~bij:bij' x in
    if not (check y) then raise (DecodingError "check failed");
    y
  | Map (_, extract, bij'), b ->
    let x = decode ~bij:bij' b in
    extract x
  | Switch (_, extract), B.D d when B.SMap.cardinal d = 1 ->
    let key, value = B.SMap.choose d in
    let BranchFrom (bij', convert) = extract key in
    convert (decode ~bij:bij' value)
  | _ -> raise (DecodingError "bad case")

  let to_string ~bij x = B.to_string (encode ~bij x)

  let of_string ~bij s =
    let b = B.of_string s in
    decode ~bij b

  let read ~bij ic =
    let d = B.mk_decoder () in
    let buf = String.create 256 in
    let rec read_chunk() =
      let n = input ic buf 0 (String.length buf) in
      if n = 0
        then raise (DecodingError "unexpected EOF")
        else match B.parse d buf 0 n with
          | B.ParsePartial -> read_chunk()
          | B.ParseError s -> raise (DecodingError s)
          | B.ParseOk b -> decode ~bij b
    in
    read_chunk()

  let read_stream ~bij ic =
    let d = B.mk_decoder () in
    let buf = String.create 256 in
    let rec try_parse n = match B.parse d buf 0 n with
      | B.ParsePartial -> read_chunk()
      | B.ParseError s -> raise (DecodingError s)
      | B.ParseOk b -> Some (decode ~bij b)
    and read_chunk() =
      let n = input ic buf 0 (String.length buf) in
      if n = 0
        then match B.parse_resume d with
        | B.ParsePartial -> None
        | B.ParseError s -> raise (DecodingError s)
        | B.ParseOk b -> Some (decode ~bij b)
        else try_parse n
    in
    Stream.from (fun _ -> read_chunk())

  let write ~bij oc x =
    let b = encode ~bij x in
    B.to_chan oc b;
    flush oc

  let write_stream ~bij oc str =
    Stream.iter (fun x -> write ~bij oc x) str
end
