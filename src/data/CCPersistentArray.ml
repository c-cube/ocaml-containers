(*
copyright (c) 2013-2015, Guillaume Bury
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


(* Persistent arrays *)

type 'a t = 'a data ref
and 'a data =
  | Array of 'a array
  | Diff of int * 'a * 'a t

let make n a = ref (Array (Array.make n a))
let init n f = ref (Array (Array.init n f))

let rec _reroot t k = match !t with
  | Array a -> k a
  | Diff (i, v, t') ->
    _reroot t' (fun a ->
        let v' = a.(i) in
        a.(i) <- v;
        t := Array a;
        t' := Diff(i, v', t);
        k a
      )

let reroot t = match !t with
  | Array a -> a
  | _ -> _reroot t (fun x -> x)

let copy t = ref (Array(Array.copy (reroot t)))

let get t i = match !t with
  | Array a -> a.(i)
  |  _ -> (reroot t).(i)

let set t i v =
  let a = reroot t in
  let old = a.(i) in
  a.(i) <- v;
  let t' = ref (Array a) in
  t := Diff (i, old, t');
  t'

let length t = Array.length (reroot t)

let map f t = ref (Array (Array.map f (reroot t)))
let mapi f t = ref (Array (Array.mapi f (reroot t)))

let iter f t = Array.iter f (reroot t)
let iteri f t = Array.iteri f (reroot t)

let fold_left f acc t = Array.fold_left f acc (reroot t)
let fold_right f t acc = Array.fold_right f (reroot t) acc

let to_array t = Array.copy (reroot t)
let of_array a = init (Array.length a) (fun i -> a.(i))

let to_list t = Array.to_list (reroot t)
let of_list l = ref (Array (Array.of_list l))

