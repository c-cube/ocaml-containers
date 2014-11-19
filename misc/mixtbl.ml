
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

type 'a t = ('a, (unit -> unit)) Hashtbl.t

type ('a, 'b) injection = {
  getter : 'a t -> 'a -> 'b option;
  setter : 'a t -> 'a -> 'b -> unit;
}

let create n = Hashtbl.create n

let access () =
  let r = ref None in
  let getter tbl k =
    r := None; (* reset state in case last operation was not a get *)
    try
      (Hashtbl.find tbl k) ();
      let result = !r in
      r := None; (* clean up here in order to avoid memory leak *)
      result
    with Not_found -> None
  in
  let setter tbl k v =
    let v_opt = Some v in
    Hashtbl.replace tbl k (fun () -> r := v_opt)
  in
  { getter; setter; }

let get ~inj tbl x = inj.getter tbl x

let set ~inj tbl x y = inj.setter tbl x y

let length tbl = Hashtbl.length tbl

let clear tbl = Hashtbl.clear tbl

let remove tbl x = Hashtbl.remove tbl x

let copy tbl = Hashtbl.copy tbl

let mem ~inj tbl x =
  match inj.getter tbl x with
  | None -> false
  | Some _ -> true

let find ~inj tbl x =
  match inj.getter tbl x with
  | None -> raise Not_found
  | Some y -> y

let iter_keys tbl f =
  Hashtbl.iter (fun x _ -> f x) tbl

let fold_keys tbl acc f =
  Hashtbl.fold (fun x _ acc -> f acc x) tbl acc

let keys tbl =
  Hashtbl.fold (fun x _ acc -> x :: acc) tbl []

let bindings ~inj tbl =
  fold_keys tbl []
    (fun acc k ->
      match inj.getter tbl k with
      | None -> acc
      | Some v -> (k, v) :: acc)
