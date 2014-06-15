
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

(** {1 Options} *)

type 'a t = 'a option

let map f = function
  | None -> None
  | Some x -> Some (f x)

let maybe f d = function
  | None -> d
  | Some x -> f x

let is_some = function
  | None -> false
  | Some _ -> true

let compare f o1 o2 = match o1, o2 with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some x, Some y -> f x y

let equal f o1 o2 = match o1, o2 with
  | None, None -> true
  | Some _, None
  | None, Some _ -> false
  | Some x, Some y -> f x y

let return x = Some x

let (>|=) x f = map f x

let (>>=) o f = match o with
  | None -> None
  | Some x -> f x

let flat_map f o = match o with
  | None -> None
  | Some x -> f x

let (<*>) f x = match f, x with
  | None, _
  | _, None -> None
  | Some f, Some x -> Some (f x)

let (<$>) = map

let map2 f o1 o2 = match o1, o2 with
  | None, _
  | _, None -> None
  | Some x, Some y -> Some (f x y)

let iter f o = match o with
  | None -> ()
  | Some x -> f x

let fold f acc o = match o with
  | None -> acc
  | Some x -> f acc x

let to_list o = match o with
  | None -> []
  | Some x -> [x]

let of_list = function
  | x::_ -> Some x
  | [] -> None

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Buffer.t -> 'a -> unit

let to_gen o =
  match o with
  | None -> (fun () -> None)
  | Some _ ->
    let first = ref true in
    fun () -> if !first then (first:=false; o) else None

let to_seq o k = match o with
  | None -> ()
  | Some x -> k x

let pp ppx buf o = match o with
  | None -> Buffer.add_string buf "None"
  | Some x -> Buffer.add_string buf "Some "; ppx buf x
