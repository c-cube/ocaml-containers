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

(** {1 Consumable generators} *)

exception EOG
  (** End of Generation *)

type 'a t = unit -> 'a generator
  (** An enum is a generator of generators *)
and 'a generator = unit -> 'a
  (** A generator may be called several times, yielding the next value
      each time. It raises EOG when it reaches the end. *)

let empty () = fun () -> raise EOG

let singleton x =
  fun () ->
    let stop = ref false in
    fun () ->
      if !stop
        then raise EOG
        else begin stop := true; x end

let start enum = enum ()

let next gen = gen ()

let junk gen = ignore (gen ())

let is_empty enum =
  try ignore ((enum ()) ()); false
  with EOG -> true

let fold f acc enum =
  let rec fold acc gen =
    let acc', stop =
      try f acc (gen ()), true
      with EOG -> acc, false in
    if stop then acc' else fold acc' gen
  in
  fold acc (enum ())

let iter f enum =
  let rec iter gen =
    let stop =
      try f (gen ()); true
      with EOG -> false in
    if stop then () else iter gen
  in
  iter (enum ())

let length enum =
  fold (fun acc _ -> acc + 1) 0 enum
              
let map f enum =
  (* another enum *)
  fun () ->
    let gen = enum () in
    (* the mapped generator *)
    fun () ->
      try f (gen ())
      with EOG -> raise EOG
    
let of_list l =
  fun () ->
    let l = ref l in
    fun () ->
      match !l with
      | [] -> raise EOG
      | x::l' -> l := l'; x

let to_list enum =
  let rec fold gen =
    try (gen ()) :: fold gen
    with EOG -> []
  in fold (enum ())

let to_rev_list enum =
  let rec fold acc gen =
    let acc', stop =
      try gen () :: acc, true
      with EOG -> acc, false
    in if stop then acc' else fold acc' gen
  in
  fold [] (enum ())
