
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

(** {1 Rational Terms} *)

module type SYMBOL = sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
end

module type S = sig
  module Symbol : SYMBOL

  type t = private
    | Var of int
    | Ref of int
    | App of Symbol.t * t list

  type term = t

  type 'a env = 'a RAL.t
  
  (** Structural equality and comparisons. Two terms being different
      for {!eq} may still be equal, but with distinct representations.
      For instance [r:f(f(r))] and [r:f(r)] are the same term but they
      are not equal structurally. *)

  val eq : t -> t -> bool
  val cmp : t -> t -> int

  val eq_set : t -> t -> bool
  (** Proper equality on terms. This returns [true] if the two terms represent
      the same infinite tree, not only if they have the same shape. *)

  val var : unit -> t
    (** free variable, with a fresh name *)

  val mk_ref : int -> t
    (** Back-ref of [n] levels down (see De Bruijn indices) *)

  val app : Symbol.t -> t list -> t
    (** Application of a symbol to a list, possibly with a unique label *)

  val const : Symbol.t -> t
    (** Shortcut for [app s []] *)

  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
  val to_string : t -> string

  val rename : t -> t
    (** Rename all variables and references to fresh ones *)

  module Subst : sig
    type t
    val empty : t
    val bind : t -> int -> term -> t
    val deref : t -> term -> term
    val apply : ?depth:int -> t -> term -> term

    val pp : Buffer.t -> t -> unit
    val fmt : Format.formatter -> t -> unit
    val to_string : t -> string
  end

  val matching : ?subst:Subst.t -> term -> term -> Subst.t option
  val unify : ?subst:Subst.t -> term -> term -> Subst.t option
end

module Make(Symbol : SYMBOL) = struct
  module Symbol = Symbol

  type t =
    | Var of int
    | Ref of int
    | App of Symbol.t * t list

  type term = t

  module IMap = Map.Make(struct
    type t = int
    let compare i j = i-j
  end)
  module IHTbl = Hashtbl.Make(struct
    type t = int
    let equal i j = i=j
    let hash i = i land max_int
  end)

  type 'a env = 'a RAL.t
    (** Environment for De Bruijn variables: a random-access list. *)

  let _to_int = function
    | Var _ -> 1
    | Ref _ -> 2
    | App _ -> 3

  let rec cmp t1 t2 = match t1, t2 with
    | Var i1, Var i2 -> i1 - i2
    | Ref i1, Ref i2 -> i1 - i2
    | App (f1, l1), App (f2, l2) ->
        let c = Symbol.compare f1 f2 in
        if c <> 0 then c
        else cmp_list l1 l2
    | _ -> _to_int t1 - _to_int t2
  and cmp_list l1 l2 = match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | t1::l1', t2::l2' ->
        let c = cmp t1 t2 in
        if c <> 0 then c else cmp_list l1' l2'

  let eq t1 t2 = cmp t1 t2 = 0

  module Set2T = Set.Make(struct
    type t = term*term
    let compare (l1,r1)(l2,r2) =
      let c = cmp l1 l2 in
      if c <> 0 then c else cmp r1 r2
  end)

  let eq_set t1 t2 =
    let cycle = ref Set2T.empty in
    let rec eq env t1 t2 = match t1, t2 with
      | Ref i, _ -> eq env (RAL.get env i) t2
      | _, Ref j -> eq env t1 (RAL.get env j)
      | Var i, Var j -> i=j
      | _ when Set2T.mem (t1,t2) !cycle -> true
      | App(f1,l1), App(f2,l2) when Symbol.compare f1 f2 = 0 ->
        (* if the subterms are equal, and we try to solve again t1=t2,
           then we shouldn't cycle. Hence we protect ourself. *)
        cycle := Set2T.add (t1, t2) !cycle;
        let env = RAL.cons t1 env in
        begin try
          List.for_all2 (eq env) l1 l2
        with Invalid_argument _ -> false
        end
      | _ -> false
    in
    eq RAL.empty t1 t2

  let _count = ref 0

  let var () =
    let v = Var !_count in
    incr _count;
    v

  let mk_ref i = Ref i

  let app s l = App (s, l)

  let const s = App (s, [])

  let rec pp buf t = match t with
    | Var i -> Printf.bprintf buf "X%d" i
    | Ref i -> Printf.bprintf buf "*%d" i
    | App (s, []) ->
        Buffer.add_string buf (Symbol.to_string s)
    | App (s, l) ->
        Printf.bprintf buf "%s(%a)" (Symbol.to_string s) pp_list l
  and pp_list buf l = match l with
    | [] -> ()
    | [x] -> pp buf x
    | x::((_::_) as l') -> 
      pp buf x; Buffer.add_string buf ", "; pp_list buf l'

  let to_string t =
    let b = Buffer.create 16 in
    pp b t;
    Buffer.contents b

  let fmt fmt t = Format.pp_print_string fmt (to_string t)

  let rename t =
    let names = IHTbl.create 16 in
    let rec rename t = match t with
      | Var i ->
          begin try IHTbl.find names i
          with Not_found ->
            (* rename variable into a fresh one *)
            let v = var() in
            IHTbl.add names i v;
            v
          end
      | Ref _ -> t  (* no need to rename *)
      | App (s, l) ->
          app s (List.map rename l)
    in rename t

  module Subst = struct
    type t = term IMap.t

    let empty = IMap.empty

    let bind s i t =
      match t with
        | _ when IMap.mem i s -> failwith "Subst.bind"
        | Var j when i=j -> s  (* id *)
        | _ -> IMap.add i t s

    let rec deref s t = match t with
      | Var i ->
          begin try deref s (IMap.find i s)
          with Not_found -> t
          end
      | Ref _
      | App _ -> t

    (* does the variable [v] occur in [subst(t)]? *)
    let rec _occur subst ~var t =
      match deref subst t with
      | Var _ -> eq var t
      | Ref _
      | App (_, []) -> false
      | App (_, l) -> List.exists (_occur subst ~var) l

    let apply ?(depth=0) subst t =
      (* [depth]: current depth w.r.t root, [back]: map from var to
         the depth of the term they are bound to *)
      let rec apply depth back subst t = match t with
        | Ref _ -> t
        | Var i ->
            let t' = deref subst t in
            (* interesting case. Either [t] is bound to a term [t']
               that contains it, which makes a cyclic term, or it's
               not in which case it's easy. *)
            begin match t' with
              | Ref _ -> t
              | App (s, l) ->
                if _occur subst ~var:t t'
                  then
                  (* in any case we are possibly going to modify [r']
                     by replacing [x] by a backref. *)
                    let back = IMap.add i depth back in
                    let subst = IMap.remove i subst in
                    app s (List.map (apply (depth+1) back subst) l)
                  else
                    (* simply keep t'->s(l) *)
                    app s (List.map (apply (depth+1) back subst) l)
              | Var j ->
                assert (not (IMap.mem j subst));
                begin try
                  let k = IMap.find j back in
                  (* the variable is actually bound to a superterm,
                     which is at depth [k]. The depth difference is
                     therefore [depth-k]. *)
                  Ref (depth-k)
                with Not_found ->
                  t'   (* truly unbound variable. *)
                end
            end
        | App (s, l) ->
            app s (List.map (apply (depth+1) back subst) l)
      in apply depth IMap.empty subst t

    let pp buf subst =
      Buffer.add_string buf "{";
      let first = ref true in
      IMap.iter
        (fun i t ->
          if !first then first:= false else Buffer.add_string buf ", ";
          Printf.bprintf buf "X%d → %a" i pp t)
        subst;
      Buffer.add_string buf "}";
      ()

    let to_string t =
      let b = Buffer.create 16 in
      pp b t;
      Buffer.contents b

    let fmt fmt t = Format.pp_print_string fmt (to_string t)
  end

  exception Fail

  let matching ?(subst=Subst.empty) t1 t2 =
    assert false  (* TODO (need to gather variables of [t2]... *)

  let unify ?(subst=Subst.empty) t1 t2 =
    (* pairs of terms already unified *)
    let cycle = ref Set2T.empty in
    (* [env] contains references to superterms *)
    let rec unif env subst t1 t2 =
      match Subst.deref subst t1, Subst.deref subst t2 with
      | Ref i1, _ -> unif env subst (RAL.get env i1) t2
      | _, Ref i2 -> unif env subst t1 (RAL.get env i2)
      | Var i, Var j when i=j -> subst
      | Var i, _ -> Subst.bind subst i t2
      | _, Var j -> Subst.bind subst j t1
      | t1, t2 when Set2T.mem (t1,t2) !cycle ->
        subst  (* t1,t2 already being unified, avoid cycling forever *)
      | App (f1, l1) as t1, (App (f2, l2) as t2) ->
        if Symbol.compare f1 f2 <> 0 then raise Fail;
        (* remember we are unifying those terms *)
        cycle := Set2T.add (t1, t2) !cycle;
        (* now we can assume [t1 = t2] if unification succeeds, so
           we just push [t1] into the env *)
        let env = RAL.cons t1 env in
        try
          List.fold_left2 (unif env) subst l1 l2
        with Invalid_argument _ -> raise Fail
    in
    try Some (unif RAL.empty subst t1 t2)
    with Fail -> None
end

module Str = struct
  type t = string
  let compare = String.compare
  let to_string s = s
end

module Default = Make(Str)
