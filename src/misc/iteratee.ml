
(*
copyright (c) 2013, simon cruanes
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

type 'a t = {
  fold: 'b. ('b -> 'a -> [`Continue | `Stop] * 'b) -> 'b -> 'b
}

exception StopNow

let of_iter i = {
  fold = (fun f acc ->
      let r = ref acc in
      begin try i (fun x ->
        let cont, acc' = f !r x in
        r := acc';
        match cont with
        | `Stop -> raise StopNow
        | `Continue -> ());
      with StopNow -> ()
      end;
      !r
  );
}

let fold f acc i =
  i.fold (fun acc x -> `Continue, f acc x) acc

let iter f i =
  i.fold (fun () x -> f x; `Continue, ()) ()

let map f i = {
  fold=(fun g acc ->
      i.fold (fun acc x -> g acc (f x)) acc
  )
}

let of_list l =
  let rec next f acc l = match l with
    | [] -> acc
    | x::l' ->
        match f acc x with
        | `Continue, acc' -> next f acc' l'
        | `Stop, res -> res
  in
  {fold=(fun f acc -> next f acc l) }

let to_rev_list i =
  i.fold (fun acc x -> `Continue, x::acc) []

let to_list i = List.rev (to_rev_list i)
