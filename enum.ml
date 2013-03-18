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
      try f acc (gen ()), false
      with EOG -> acc, true in
    if stop then acc' else fold acc' gen
  in
  fold acc (enum ())

let iter f enum =
  let rec iter gen =
    let stop =
      try f (gen ()); false
      with EOG -> true in
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

let append e1 e2 =
  fun () ->
    let gen = ref (e1 ()) in
    let first = ref true in
    (* get next element *)
    let rec next () =
      try !gen ()
      with EOG ->
        if !first then begin
          first := false;
          gen := e2 ();  (* switch to the second generator *)
          next ()
        end else raise EOG  (* done *)
    in next

let flatten enum =
  fun () ->
    let next_gen = enum () in
    let gen = ref (fun () -> raise EOG) in
    (* get next element *)
    let rec next () =
      try !gen ()
      with EOG ->
        (* jump to next sub-enum *)
        let stop =
          try gen := (next_gen () ()); false
          with EOG -> true in
        if stop then raise EOG else next ()
    in next
      
let flatMap f enum =
  fun () ->
    let next_elem = enum () in
    let gen = ref (fun () -> raise EOG) in
    (* get next element *)
    let rec next () =
      try !gen ()
      with EOG ->
        (* enumerate f (next element) *)
        let stop =
          try
            let x = next_elem () in
            gen := (f x) (); false
          with EOG -> true in
        if stop then raise EOG else next ()
    in next

let take n enum =
  assert (n >= 0);
  fun () ->
    let gen = enum () in
    let count = ref 0 in  (* how many yielded elements *)
    fun () ->
      if !count = n then raise EOG
      else begin incr count; gen () end

let drop n enum =
  assert (n >= 0);
  fun () ->
    let gen = enum () in
    let count = ref 0 in  (* how many droped elements? *)
    let rec next () =
      if !count < n
        then begin incr count; ignore (gen ()); next () end
        else gen ()
    in next
    
let of_list l =
  fun () ->
    let l = ref l in
    fun () ->
      match !l with
      | [] -> raise EOG
      | x::l' -> l := l'; x

let to_list enum =
  let rec fold gen =
    try
      let x = gen () in
      x :: fold gen
    with EOG -> []
  in fold (enum ())

let to_rev_list enum =
  let rec fold acc gen =
    let acc', stop =
      try let x = gen () in x :: acc, true
      with EOG -> acc, false
    in if stop then acc' else fold acc' gen
  in
  fold [] (enum ())

let int_range i j =
  fun () ->
    let r = ref i in
    fun () ->
      let x = !r in
      if x > j then raise EOG
        else begin
          incr r;
          x
        end

module Infix = struct
  let (@@) = append

  let (>>=) e f = flatMap f e

  let (--) = int_range

  let (|>) x f = f x
end
