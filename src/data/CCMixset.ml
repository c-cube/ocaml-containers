
(*
copyright (c) 2013-2015, simon cruanes
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

(** {1 Set of Heterogeneous Values} *)

module IMap = Map.Make(struct
  type t = int
  let compare : int -> int -> int = compare
end)

(*$R
  let k1 : int key = newkey () in
  let k2 : int key = newkey () in
  let k3 : string key = newkey () in
  let set =
    empty
    |> set ~key:k1 1
    |> set ~key:k2 2
    |> set ~key:k3 "3"
  in
  assert (get ~key:k1 set = Some 1);
  assert (get ~key:k2 set = Some 2);
  assert (get ~key:k3 set = Some "3");
  ()
*)

type t = (unit -> unit) IMap.t
and 'a key = {
  id: int;
  mutable opt : 'a option;
};;

let newkey_n_ = ref 0

let newkey () =
  let id = !newkey_n_ in
  incr newkey_n_;
  { id; opt=None; }

let empty = IMap.empty

let get ~key set =
  key.opt <- None;
  try
    (IMap.find key.id set) ();
    key.opt
  with Not_found -> None

let get_exn ~key set = match get ~key set with
  | None -> raise Not_found
  | Some v -> v

let set ~key v set =
  IMap.add key.id (fun () -> key.opt <- Some v) set

let cardinal set = IMap.cardinal set
