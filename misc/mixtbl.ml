
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

(** {1 Hash Table with Heterogeneous Keys} *)

type 'b injection = {
  get : (unit -> unit) -> 'b option;
  set : 'b -> (unit -> unit);
}

type 'a t = ('a, unit -> unit) Hashtbl.t

let create n = Hashtbl.create n

let create_inj () =
  let r = ref None in
  let get f =
    r := None;
    f ();
    !r
  and set v =
    (fun () -> r := Some v)
  in
  {get;set}

let get ~inj tbl x =
  try inj.get (Hashtbl.find tbl x)
  with Not_found -> None

let set ~inj tbl x y =
  Hashtbl.replace tbl x (inj.set y)

let length tbl = Hashtbl.length tbl

let clear tbl = Hashtbl.clear tbl

let remove tbl x = Hashtbl.remove tbl x

let copy tbl = Hashtbl.copy tbl

let mem ~inj tbl x =
  try
    inj.get (Hashtbl.find tbl x) <> None
  with Not_found -> false

let find ~inj tbl x =
  match inj.get (Hashtbl.find tbl x) with
    | None -> raise Not_found
    | Some v -> v

let iter_keys tbl f =
  Hashtbl.iter (fun x _ -> f x) tbl

let fold_keys tbl acc f =
  Hashtbl.fold (fun x _ acc -> f acc x) tbl acc

(** {2 Iterators} *)

type 'a sequence = ('a -> unit) -> unit

let keys_seq tbl yield =
  Hashtbl.iter
    (fun x _ -> yield x)
    tbl

let bindings_of ~inj tbl yield =
  Hashtbl.iter
    (fun k value ->
      match inj.get value with
      | None -> ()
      | Some v -> yield (k, v)
    ) tbl

type value =
  | Value : ('b injection -> 'b option) -> value

let bindings tbl yield =
  Hashtbl.iter
    (fun x y -> yield (x, Value (fun inj -> inj.get y)))
    tbl
