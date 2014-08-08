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

(** {1 Persistent hash-table on top of OCaml's hashtables} *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a equal = 'a -> 'a -> bool

module type HashedType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** {2 Signature of such a hashtable} *)

module type S = sig
  type key
  type 'a t

  val empty : unit -> 'a t
  (** Empty table. The table will be allocated at the first binding *)

  val create : int -> 'a t
  (** Create a new hashtable, with the given initial capacity *)

  val is_empty : 'a t -> bool
  (** Is the table empty? *)

  val find : 'a t -> key -> 'a
  (** Find the value for this key, or fails
      @raise Not_found if the key is not present in the table *)

  val get_exn : key -> 'a t -> 'a
  (** Synonym to {!find} with flipped arguments *)

  val get : key -> 'a t -> 'a option
  (** Safe version of !{get_exn} *)

  val mem : 'a t -> key -> bool
  (** Is the key bound? *)

  val length : _ t -> int
  (** Number of bindings *)

  val replace : 'a t -> key -> 'a -> 'a t
  (** Add the binding to the table, returning a new table. This erases
      the current binding for [key], if any. *)

  val update : 'a t -> key -> ('a option -> 'a option) -> 'a t
  (** [update tbl key f] calls [f None] if [key] doesn't belong in [tbl],
      [f (Some v)] if [key -> v] otherwise; If [f] returns [None] then
      [key] is removed, else it returns [Some v'] and [key -> v'] is added. *)

  val remove : 'a t -> key -> 'a t
  (** Remove the key *)

  val copy : 'a t -> 'a t
  (** Fresh copy of the table; the underlying structure is not shared
      anymore, so using both tables alternatively will be efficient *)

  val merge : (key -> 'a option -> 'a option -> 'a option) ->
              'a t -> 'a t -> 'a t
  (** Merge two tables together into a new table. The function's argument
      correspond to values associated with the key (if present); if the
      function returns [None] the key will not appear in the result. *)

  val iter : 'a t -> (key -> 'a -> unit) -> unit
  (** Iterate over bindings *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold over bindings *)

  val map : (key -> 'a -> 'b) -> 'a t -> 'b t
  (** Map all values *)

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  (** {3 Conversions} *)

  val of_seq : (key * 'a) sequence -> 'a t
  (** Add (replace) bindings from the sequence to the table *)

  val of_list : (key * 'a) list -> 'a t

  val add_seq : 'a t -> (key * 'a) sequence -> 'a t

  val add_list : 'a t -> (key  * 'a) list -> 'a t

  val to_seq : 'a t -> (key * 'a) sequence
  (** Sequence of the bindings of the table *)

  val to_list : 'a t -> (key * 'a) list

  (** {3 Misc} *)

  val equal : 'a equal -> 'a t equal

  val pp : key printer -> 'a printer -> 'a t printer  

  val print : key formatter -> 'a formatter -> 'a t formatter
end

(** {2 Implementation} *)

module Make(H : HashedType) : S with type key = H.t = struct
  module Table = Hashtbl.Make(H)
    (** Imperative hashtable *)

  type key = H.t
  type 'a t = 'a zipper ref
  and 'a zipper =
    | Table of 'a Table.t         (** Concrete table *)
    | Add of key * 'a * 'a t      (** Add key *)
    | Replace of key * 'a * 'a t  (** Replace key by value *)
    | Remove of key * 'a t        (** As the table, but without given key *)

  let create i =
    ref (Table (Table.create i))

  let empty () = create 11

  (* pass continuation to get a tailrec rerooting *)
  let rec _reroot t k = match !t with
  | Table tbl -> k tbl  (* done *)
  | Add (key, v, t') ->
    _reroot t'
      (fun tbl ->
        t' := Remove (key, t);
        Table.add tbl key v;
        t := Table tbl;
        k tbl)
  | Replace (key, v, t') ->
    _reroot t'
      (fun tbl ->
        let v' = Table.find tbl key in
        t' := Replace (key, v', t);
        t := Table tbl;
        Table.replace tbl key v;
        k tbl)
  | Remove (key, t') ->
    _reroot t'
      (fun tbl ->
        let v = Table.find tbl key in
        t' := Add (key, v, t);
        t := Table tbl;
        Table.remove tbl key;
        k tbl)

  (* Reroot: modify the zipper so that the current node is a proper
     hashtable, and return the hashtable *)
  let reroot t = match !t with
    | Table tbl -> tbl
    | _ -> _reroot t (fun x -> x)

  let is_empty t = Table.length (reroot t) = 0

  let find t k = Table.find (reroot t) k

  let get_exn k t = find t k

  let get k t =
    try Some (find t k)
    with Not_found -> None

  let mem t k = Table.mem (reroot t) k

  let length t = Table.length (reroot t)

  let replace t k v =
    let tbl = reroot t in
    (* create the new hashtable *)
    let t' = ref (Table tbl) in
    (* update [t] to point to the new hashtable *)
    (try
      let v' = Table.find tbl k in
      t := Replace (k, v', t')
    with Not_found ->
      t := Remove (k, t')
    );
    (* modify the underlying hashtable *)
    Table.replace tbl k v;
    t'

  let remove t k =
    let tbl = reroot t in
    try
      let v' = Table.find tbl k in
      (* value present, make a new hashtable without this value *)
      let t' = ref (Table tbl) in
      t := Add (k, v', t');
      Table.remove tbl k;
      t'
    with Not_found ->
      (* not member, nothing to do *)
      t

  let update t k f =
    let v = get k t in
    match v, f v with
    | None, None -> t  (* no change *)
    | Some _, None -> remove t k
    | _, Some v' -> replace t k v'

  let copy t =
    let tbl = reroot t in
    (* no one will point to the new [t] *)
    let t = ref (Table (Table.copy tbl)) in
    t
  
  let iter t f =
    let tbl = reroot t in
    Table.iter f tbl

  let fold f acc t =
    let tbl = reroot t in
    Table.fold (fun k v acc -> f acc k v) tbl acc

  let map f t =
    let tbl = reroot t in
    let res = Table.create (Table.length tbl) in
    Table.iter (fun k v -> Table.replace res k (f k v)) tbl;
    ref (Table res)

  let filter p t =
    let tbl = reroot t in
    let res = Table.create (Table.length tbl) in
    Table.iter (fun k v -> if p k v then Table.replace res k v) tbl;
    ref (Table res)

  let filter_map f t =
    let tbl = reroot t in
    let res = Table.create (Table.length tbl) in
    Table.iter
      (fun k v -> match f k v with
        | None -> ()
        | Some v' -> Table.replace res k v'
      ) tbl;
    ref (Table res)

  exception ExitPTbl

  let for_all p t =
    try
      iter t (fun k v -> if not (p k v) then raise ExitPTbl);
      true
    with ExitPTbl -> false

  let exists p t =
    try
      iter t (fun k v -> if p k v then raise ExitPTbl);
      false
    with ExitPTbl -> true

  let merge f t1 t2 =
    let tbl = Table.create (max (length t1) (length t2)) in
    iter t1
      (fun k v1 ->
        let v2 = try Some (find t2 k) with Not_found -> None in
        match f k (Some v1) v2 with
        | None -> ()
        | Some v' -> Table.replace tbl k v');
    iter t2
      (fun k v2 ->
        if not (mem t1 k) then match f k None (Some v2) with
          | None -> ()
          | Some v' -> Table.replace tbl k v2);
    ref (Table tbl)

  let add_seq init seq =
    let tbl = ref init in
    seq (fun (k,v) -> tbl := replace !tbl k v);
    !tbl

  let of_seq seq = add_seq (empty ()) seq

  let add_list init l =
    add_seq init (fun k -> List.iter k l)

  let of_list l = add_list (empty ()) l

  let to_list t =
    let tbl = reroot t in
    let bindings = Table.fold (fun k v acc -> (k,v)::acc) tbl [] in
    bindings

  let to_seq t =
    fun k ->
      let tbl = reroot t in
      Table.iter (fun x y -> k (x,y)) tbl

  let equal eq t1 t2 =
    length t1 = length t2
    &&
    for_all
      (fun k v -> match get k t2 with
        | None -> false
        | Some v' -> eq v v'
      ) t1

  let pp pp_k pp_v buf t =
    Buffer.add_string buf "{";
    let first = ref true in
    iter t
      (fun k v ->
        if !first then first:=false else Buffer.add_string buf ", ";
        Printf.bprintf buf "%a -> %a" pp_k k pp_v v
      );
    Buffer.add_string buf "}"
 
  let print pp_k pp_v fmt t =
    Format.pp_print_string fmt "{";
    let first = ref true in
    iter t
      (fun k v ->
        if !first then first:=false
        else (Format.pp_print_string fmt ", "; Format.pp_print_cut fmt ());
        Format.fprintf fmt "%a -> %a" pp_k k pp_v v
      );
    Format.pp_print_string fmt "}"
end

