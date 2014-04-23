
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

(** {1 Causal Graph} for Debugging *)

(** {2 Basic Causal Description} *)

type t = {
  id : int;
  descr : string;
  attrs : string list;
  mutable within : t list;
  mutable after : t list;
}

type cause = t

let _count = ref 0

let make ?(attrs=[]) ?(within=[]) ?(after=[]) descr =
  let id = !_count in
  incr _count;
  { id; descr; attrs; within; after; }

let root = make ~within:[] ~after:[] "root cause"

let make_b ?attrs ?within ?after fmt =
  let buf = Buffer.create 24 in
  Printf.kbprintf
    (fun buf -> make ?attrs ?within ?after (Buffer.contents buf))
    buf fmt

let add_within a b = a.within <- b :: a.within
let add_after a b = a.after <- b :: a.after

let id c = c.id

let level c = assert false  (* TODO *)

let pp buf c =
  let rec pp_id_list buf l = match l with
    | [] -> ()
    | [x] -> Printf.bprintf buf "%d" x.id
    | x::l' -> Printf.bprintf buf "%d, " x.id; pp_id_list buf l'
  in
  Printf.bprintf buf "cause_%d{%s, within{%a}, after{%a}}" c.id
    c.descr pp_id_list c.within pp_id_list c.after

let fmt fmt c =
  let buf = Buffer.create 15 in
  pp buf c;
  Format.pp_print_string fmt (Buffer.contents buf)

(** {2 Encoding to/from B-Encode} *)

type 'a sequence = ('a -> unit) -> unit

module Bencode = struct
  type token =
    [ `I of int
    | `S of string
    | `BeginDict
    | `BeginList
    | `End
    ]

  let to_seq c k =
    k `BeginDict;
    k (`S "after");
    k `BeginList;
    List.iter (fun c' -> k (`I c'.id)) c.after;
    k `End;
    k (`S "attrs");
    k `BeginList;
    List.iter (fun s -> k (`S s)) c.attrs;
    k `End;
    k (`S "descr");
    k (`S c.descr);
    k (`S "id");
    k (`I c.id);
    k (`S "within");
    k `BeginList;
    List.iter (fun c' -> k (`I c'.id)) c.within;
    k `End;
    k `End

  module ITbl = Hashtbl.Make(struct
    type t = int
    let equal i j = i=j
    let hash i = i land max_int
  end)

  module Sink = struct
    type t = {
      send : token -> unit;
      ids : unit ITbl.t; (* printed IDs *)
    }

    let make send = { send; ids = ITbl.create 32; }

    let mem sink id = ITbl.mem sink.ids id

    let print sink c =
      let s = Stack.create () in
      Stack.push (`Enter c) s;
      (* DFS in postfix order *)
      while not (Stack.is_empty s) do
        match Stack.pop s with
        | `Enter c when mem sink c.id -> ()  (* already done *)
        | `Enter c ->
            ITbl.add sink.ids c.id ();
            (* explore sub-causes *)
            List.iter (fun c' -> Stack.push (`Enter c') s) c.within;
            List.iter (fun c' -> Stack.push (`Enter c') s) c.after;
            Stack.push (`Exit c) s;
        | `Exit c ->
            (* print the cause *)
            to_seq c sink.send
      done
  end

  module Source = struct
    type t = {
      tbl : cause ITbl.t;
      mutable roots : cause list;
    }

    let make seq =
      let tbl = ITbl.create 128 in
      let _roots = ref [] in
      seq
        (function
        | _ -> assert false (* TODO parse back *)
        );
        { tbl; roots= !_roots; }

    let roots src k = List.iter k src.roots

    let by_id_exn src id = ITbl.find src.tbl id

    let by_id src id =
      try Some (by_id_exn src id)
      with Not_found -> None
  end
end
