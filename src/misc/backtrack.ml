
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module NonLogical = struct
  type 'a t = unit -> 'a
  let return x () = x
  let (>>=) x f () = let y = x() in f y ()
end

type ('a, 'b) list_view =
  | Nil of exn
  | Cons of 'a * 'b

(** The monad is parametrised in the types of state, environment and
    writer. *)
module type Param = sig
  (** Read only *)
  type e
(** Write only *)
  type w
(** [w] must be a monoid *)
  val wunit : w
  val wprod : w -> w -> w
(** Read-write *)
  type s
(** Update-only. Essentially a writer on [u->u]. *)
  type u
(** [u] must be pointed. *)
  val uunit : u
end

module Logical (P:Param) = struct
  type state = {
    e: P.e;
    w: P.w;
    s: P.s;
    u: P.u;
  }

  type _ t =
    | Ignore : _ t -> unit t
    | Return : 'a -> 'a t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Get : P.s t
    | Set : P.s -> unit t
    | Modify : (P.s -> P.s) -> unit t
    | Put : P.w -> unit t
    | Current : P.e t
    | Local : P.e * 'a t -> 'a t (* local bind *)
    | Update : (P.u -> P.u) -> unit t
    | Zero : exn -> 'a t
    | WithState : state * 'a t -> 'a t (* use other state *)
    | Plus : 'a t * (exn -> 'a t ) -> 'a t
    | Split : 'a t -> ('a, exn -> 'a t) list_view t
    | Once : 'a t -> 'a t  (* keep at most one element *)
    | Break : (exn -> exn option) * 'a t -> 'a t

  let return x = Return x

  let (>>=) x f = Bind (x, f)

  let map f x = match x with
    | Return x -> return (f x)
    | Map (y, g) -> Map (y, fun x -> f (g x))
    | _ -> Map (x, f)

  let rec ignore : type a. a t -> unit t = function
    | Return _ -> Return ()
    | Map (x, _) -> ignore x
    | x -> Ignore x

  let set x = Set x
  let get = Get
  let modify f = Modify f
  let put x = Put x
  let current = Current
  let local x y = Local (x, y)
  let update f = Update f
  let zero e = Zero e
  let with_state st x = WithState (st, x)

  let rec plus a f = match a with
    | Zero e -> f e
    | Plus (a1, f1) ->
      plus a1 (fun e -> plus (f1 e) f)
    | _ -> Plus (a, f)

  let split x = Split x

  let rec once : type a. a t -> a t = function
    | Zero e -> Zero e
    | Return x -> Return x
    | Map (x, f) -> map f (once x)
    | x -> Once x

  let break f x = Break (f, x)

  type 'a reified =
    | RNil of exn
    | RCons of 'a * (exn -> 'a reified)

  let repr r () = match r with
    | RNil e -> Nil e
    | RCons (x, f) -> Cons (x, f)

  let cons x cont = Cons (x, cont)
  let nil e = Nil e

  let rcons x cont = RCons (x, cont)
  let rnil e = RNil e

  type 'a splitted = (('a * state), exn -> 'a t) list_view

  let rec run_rec
    : type a. state -> a t -> a splitted
    = fun st t -> match t with
    | Return x -> cons (x, st) zero
    | Ignore x ->
      begin match run_rec st x with
        | Nil e -> Nil e
        | Cons ((_, st), cont) -> cons ((), st) (fun e -> Ignore (cont e))
      end
    | Bind (x,f) ->
      begin match run_rec st x with
        | Nil e -> Nil e
        | Cons ((x, st_x), cont) ->
          let y = f x in
          run_rec st_x (plus y (fun e -> with_state st (cont e >>= f)))
      end
    | Map (x,f) ->
      begin match run_rec st x with
        | Nil e -> Nil e
        | Cons ((x, st), cont) ->
          cons (f x, st) (fun e -> map f (cont e))
      end
    | Get -> cons (st.s, st) zero
    | Set s -> cons ((), {st with s}) zero 
    | Modify f ->
      let st = {st with s = f st.s} in
      cons ((), st) zero
    | Put w -> cons ((), {st with w}) zero
    | Current -> cons (st.e, st) zero
    | Local (e,x) ->
      (* bind [st.e = e] in [x] *)
      let st' = {st with e} in
      run_rec st' x
    | Update f ->
      let st = {st with u=f st.u} in
      cons ((), st) zero
    | WithState (st', x) -> run_rec st' x (* ignore [st] *)
    | Zero e -> Nil e (* failure *)
    | Plus (x,cont) ->
      begin match run_rec st x with
        | Nil e -> run_rec st (cont e)
        | Cons ((x, st), cont') ->
          cons (x, st) (fun e -> plus (cont' e) cont)
      end
    | Split x ->
      begin match run_rec st x with
        | Nil e -> cons (Nil e, st) zero
        | Cons ((x, st'), cont) -> cons (cons x cont, st') zero
      end
    | Once x ->
      begin match run_rec st x with
        | Nil e -> Nil e
        | Cons ((x, st), _) -> cons (x, st) zero
      end
    | Break (f,x) -> assert false (* TODO: ? *)

  let run t e s =
    let state = {e; s; u=P.uunit; w=P.wunit} in
    let rec run_list
      : type a. state -> a t -> (a * state) reified
      = fun state t -> match run_rec state t with
      | Nil e -> rnil e
      | Cons ((x, st), cont) ->
        rcons (x, st) (fun e -> run_list state (cont e))
    in
    run_list state t
end

