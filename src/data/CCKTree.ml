(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Lazy Tree Structure}
    This structure can be used to represent trees and directed
    graphs (as infinite trees) in a lazy fashion. Like {!CCKList}, it
    is a structural type. *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a printer = Format.formatter -> 'a -> unit

type +'a t = unit -> [`Nil | `Node of 'a * 'a t list]

let empty () = `Nil

let is_empty t = match t() with
  | `Nil -> true
  | `Node _ -> false

let singleton x () = `Node (x, [])

let node x l () = `Node(x,l)

let node1 x t () = `Node(x,[t])
let node2 x t1 t2 () = `Node(x,[t1;t2])

let rec fold f acc t = match t() with
  | `Nil -> acc
  | `Node (x,l) ->
    let acc = f acc x in
    List.fold_left (fold f) acc l

let rec iter f t = match t() with
  | `Nil -> ()
  | `Node(x,l) -> f x; List.iter (iter f) l

let size t = fold (fun n _ -> n+1) 0 t

let height t =
  let rec aux t k = match t() with
    | `Nil -> k 0
    | `Node (_, l) -> aux_l 0 l k
  and aux_l acc l k = match l with
    | [] -> k acc
    | t'::l' ->
      aux t' (fun n -> aux_l (max acc n) l' k)
  in aux t (fun x->x)

let rec map f t () = match t() with
  | `Nil -> `Nil
  | `Node(x,l) ->
    `Node (f x, List.map (map f) l)

let (>|=) t f = map f t

let rec cut_depth n t () = match t() with
  | `Nil -> `Nil
  | `Node _ when n=0 -> `Nil
  | `Node(x,l) ->
    `Node(x, List.map (cut_depth (n-1)) l)

(** {2 Graph Traversals} *)

(** Abstract Set structure *)
class type ['a] pset = object
  method add : 'a -> 'a pset
  method mem : 'a -> bool
end

let set_of_cmp (type elt) ~cmp () =
  let module S = Set.Make(struct
      type t = elt
      let compare = cmp
    end) in
  object
    val s = S.empty
    method add x = {< s = S.add x s >}
    method mem x = S.mem x s
  end

let _nil () = `Nil
let _cons x l = `Cons (x, l)

let dfs ~pset t =
  let rec dfs pset stack () = match stack with
    | [] -> `Nil
    | `Explore t :: stack' ->
      begin match t() with
        | `Nil -> dfs pset stack' ()
        | `Node (x, _) when pset#mem x ->
          dfs pset stack' ()  (* loop *)
        | `Node (x, l) ->
          let pset' = pset#add x in
          let stack' =
            List.rev_append (List.rev_map (fun x -> `Explore x) l) (`Exit x :: stack')
          in
          _cons (`Enter x) (dfs pset' stack')
      end
    | `Exit x :: stack' ->
      _cons (`Exit x) (dfs pset stack')
  in
  dfs pset [`Explore t]

(** Functional queues for BFS *)
module FQ = struct
  type 'a t = {
    hd : 'a list;
    tl : 'a list;
  }

  exception Empty

  (* invariant: if hd=[], then tl=[] *)
  let _make hd tl = match hd with
    | [] -> {hd=List.rev tl; tl=[] }
    | _::_ -> {hd; tl; }

  let empty = _make [] []

  let list_is_empty = function
    | [] -> true
    | _::_ -> false

  let is_empty q = list_is_empty q.hd

  let push q x = _make q.hd (x::q.tl)

  let pop_exn q =
    match q.hd with
      | [] -> assert (list_is_empty q.tl); raise Empty
      | x::hd' ->
        let q' = _make hd' q.tl in
        x, q'
end

let bfs ~pset t =
  let rec bfs pset q () =
    if FQ.is_empty q then `Nil
    else
      let t, q' = FQ.pop_exn q in
      match t() with
        | `Nil -> bfs pset q' ()
        | `Node(x,_) when pset#mem x ->
          bfs pset q' () (* loop *)
        | `Node(x,l) ->
          let q' = List.fold_left FQ.push q' l in
          let pset' = pset#add x in
          _cons x (bfs pset' q')
  in
  bfs pset (FQ.push FQ.empty t)

let rec force t : ([`Nil | `Node of 'a * 'b list] as 'b) = match t() with
  | `Nil -> `Nil
  | `Node (x, l) -> `Node (x, List.map force l)

let find ~pset f t =
  let rec _find_kl f l = match l() with
    | `Nil -> None
    | `Cons (x, l') ->
      match f x with
        | None -> _find_kl f l'
        | Some _ as res -> res
  in
  _find_kl f (bfs ~pset t)

(** {2 Pretty-printing} *)

let pp pp_x fmt t =
  (* at depth [lvl] *)
  let rec pp fmt t = match t with
    | `Nil -> ()
    | `Node (x, children) ->
      let children = filter children in
      match children with
        | [] -> pp_x fmt x
        | _::_ ->
          Format.fprintf fmt "@[<v2>(@[<hov0>%a@]%a)@]"
            pp_x x pp_children children
  and filter l  =
    let l = List.fold_left
        (fun acc c -> match c() with
           | `Nil -> acc
           | `Node _ as sub -> sub :: acc
        ) [] l
    in
    List.rev l
  and pp_children fmt children =
    (* remove empty children *)
    List.iter
      (fun c ->
         Format.fprintf fmt "@,";
         pp fmt c
      ) children
  in
  pp fmt (t ());
  ()

(** {2 Pretty printing in the DOT (graphviz) format} *)

module Dot = struct
  type attribute = [
    | `Color of string
    | `Shape of string
    | `Weight of int
    | `Style of string
    | `Label of string
    | `Id of string
    | `Other of string * string
  ] (** Dot attributes for nodes *)

  type graph = (string * attribute list t list)
  (** A dot graph is a name, plus a list of trees labelled with attributes *)

  let mk_id format =
    let buf = Buffer.create 64 in
    Printf.kbprintf
      (fun _ -> `Id (Buffer.contents buf))
      buf
      format

  let mk_label format =
    let buf = Buffer.create 64 in
    Printf.kbprintf
      (fun _ -> `Label(Buffer.contents buf))
      buf
      format

  let make ~name l = (name,l)

  let singleton ~name t = (name, [t])

  (* find and remove the `Id attribute, if any *)
  let rec _find_id acc l = match l with
    | [] -> raise Not_found
    | `Id n :: l' -> n, List.rev_append acc l'
    | x :: l' -> _find_id (x::acc) l'

  let _pp_attr fmt attr = match attr with
    | `Color c -> Format.fprintf fmt "color=%s" c
    | `Shape s -> Format.fprintf fmt "shape=%s" s
    | `Weight w -> Format.fprintf fmt "weight=%d" w
    | `Style s -> Format.fprintf fmt "style=%s" s
    | `Label l -> Format.fprintf fmt "label=\"%s\"" l
    | `Other (name, value) -> Format.fprintf fmt "%s=\"%s\"" name value
    | `Id _ -> ()  (* should not be here *)

  let rec _pp_attrs fmt l = match l with
    | [] -> ()
    | [x] -> _pp_attr fmt x
    | x::l' ->
      _pp_attr fmt x;
      Format.pp_print_char fmt ',';
      _pp_attrs fmt l'

  let pp out (name,l) =
    (* nodes already printed *)
    let tbl = Hashtbl.create 32 in
    (* fresh name generator *)
    let new_name =
      let n = ref 0 in
      fun () ->
        let s = Printf.sprintf "node_%d" !n in
        incr n; s
    in
    (* the name for some node is either defined, either a fresh random
        name *)
    let get_name x =
      try _find_id [] x
      with Not_found -> new_name (), x
    in
    (* recursive printing (bfs) *)
    let rec aux q =
      if FQ.is_empty q
      then ()
      else
        let (parent,x), q' = FQ.pop_exn q in
        let q' = pp_node q' ?parent x in
        aux q'
    and pp_node q ?parent t = match t() with
      | `Nil -> q
      | `Node (x,l) ->
        let name, attrs = get_name x in
        begin match parent with
          | None -> ()
          | Some n -> Format.fprintf out "  %s -> %s;@," n name
        end;
        if not (Hashtbl.mem tbl name) then (
          Hashtbl.add tbl name ();
          Format.fprintf out "@[%s [%a];@]@," name _pp_attrs attrs;
          List.fold_left
            (fun q y -> FQ.push q (Some name, y)) q l
        ) else q
    in
    let q =
      List.fold_left
        (fun q y -> FQ.push q (None, y))
        FQ.empty l
    in
    (* preamble *)
    Format.fprintf out "@[<hv 2>digraph \"%s\" {@," name;
    aux q;
    Format.fprintf out "}@]@.";
    ()

  let pp_single name out t = pp out (singleton ~name t)

  let print_to_file filename g =
    let oc = open_out filename in
    let fmt = Format.formatter_of_out_channel oc in
    try
      pp fmt g;
      Format.pp_print_flush fmt ();
      close_out oc
    with e ->
      close_out oc;
      raise e

  let to_file ?(name="graph") filename trees =
    let g = make ~name trees in
    print_to_file filename g
end
