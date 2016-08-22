
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Error Monad} *)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

(** {2 Basics} *)

type (+'good, +'bad) t =
  [ `Ok of 'good
  | `Error of 'bad
  ]

let return x = `Ok x

let fail s = `Error s

(* TODO: optional argument for printing stacktrace? *)
let fail_printf format =
  let buf = Buffer.create 16 in
  Printf.kbprintf
    (fun buf -> fail (Buffer.contents buf))
    buf format

(* TODO: easy ways to print backtrace/stack *)

let _printers = ref []

let register_printer p = _printers := p :: !_printers

(* FIXME: just use {!Printexc.register_printer} instead? *)

let of_exn e =
  let buf = Buffer.create 32 in
  let rec try_printers l = match l with
    | [] -> Buffer.add_string buf (Printexc.to_string e)
    | p :: l' ->
        try p buf e
        with _ -> try_printers l'
  in
  try_printers !_printers;
  `Error (Buffer.contents buf)

let of_exn_trace e =
  let buf = Buffer.create 128 in
  let rec try_printers l = match l with
    | [] -> Buffer.add_string buf (Printexc.to_string e)
    | p :: l' ->
        try p buf e
        with _ -> try_printers l'
  in
  try_printers !_printers;
  Buffer.add_char buf '\n';
  Buffer.add_string buf (Printexc.get_backtrace ());
  `Error (Buffer.contents buf)

let map f e = match e with
  | `Ok x -> `Ok (f x)
  | `Error s -> `Error s

let map_err f e = match e with
  | `Ok _ as res -> res
  | `Error y -> `Error (f y)

let map2 f g e = match e with
  | `Ok x -> `Ok (f x)
  | `Error s -> `Error (g s)

let iter f e = match e with
  | `Ok x -> f x
  | `Error _ -> ()

let get_exn = function
  | `Ok x -> x
  | `Error _ -> raise (Invalid_argument "CCError.get_exn")

let catch e ~ok ~err = match e with
  | `Ok x -> ok x
  | `Error y -> err y

let flat_map f e = match e with
  | `Ok x -> f x
  | `Error s -> `Error s

let (>|=) e f = map f e

let (>>=) e f = flat_map f e

let equal ?(err=Pervasives.(=)) eq a b = match a, b with
  | `Ok x, `Ok y -> eq x y
  | `Error s, `Error s' -> err s s'
  | _ -> false

let compare ?(err=Pervasives.compare) cmp a b = match a, b with
  | `Ok x, `Ok y -> cmp x y
  | `Ok _, _  -> 1
  | _, `Ok _ -> -1
  | `Error s, `Error s' -> err s s'

let fold ~success ~failure x = match x with
  | `Ok x -> success x
  | `Error s -> failure s

(** {2 Wrappers} *)

let guard f =
  try `Ok (f ())
  with e -> `Error e

let guard_str f =
  try `Ok (f())
  with e -> of_exn e

let guard_str_trace f =
  try `Ok (f())
  with e -> of_exn_trace e

let wrap1 f x =
  try return (f x)
  with e -> `Error e

let wrap2 f x y =
  try return (f x y)
  with e -> `Error e

let wrap3 f x y z =
  try return (f x y z)
  with e -> `Error e

(** {2 Applicative} *)

let pure = return

let (<*>) f x = match f with
  | `Error s -> fail s
  | `Ok f -> map f x

let join t = match t with
  | `Ok (`Ok o) -> `Ok o
  | `Ok (`Error e) -> `Error e
  | (`Error _) as e -> e

let both x y =
  match x,y with
   | `Ok o, `Ok o' -> `Ok (o, o')
   | `Ok _, `Error e -> `Error e
   | `Error e, _  -> `Error e

(** {2 Collections} *)

let map_l f l =
  let rec map acc l = match l with
  | [] -> `Ok (List.rev acc)
  | x::l' ->
      match f x with
      | `Error s -> `Error s
      | `Ok y -> map (y::acc) l'
  in map [] l

exception LocalExit

let fold_seq f acc seq =
  let err = ref None in
  try
    let acc = ref acc in
    seq
      (fun x -> match f !acc x with
      | `Error s -> err := Some s; raise LocalExit
        | `Ok y -> acc := y
      );
    `Ok !acc
  with LocalExit ->
    match !err with None -> assert false | Some s -> `Error s

let fold_l f acc l = fold_seq f acc (fun k -> List.iter k l)

(** {2 Misc} *)

let choose l =
  let rec _find = function
    | [] -> raise Not_found
    | ((`Ok _) as res) :: _ -> res
    | (`Error _) :: l' -> _find l'
  in
  try _find l
  with Not_found ->
    let l' = List.map (function `Error s -> s | `Ok _ -> assert false) l in
    `Error l'

let retry n f =
  let rec retry n acc = match n with
  | 0 -> fail (List.rev acc)
  | _ ->
      match f () with
      | `Ok _ as res -> res
      | `Error e -> retry (n-1) (e::acc)
  in retry n []

(** {2 Infix} *)

module Infix = struct
  let (>>=) = (>>=)
  let (>|=) = (>|=)
  let (<*>) = (<*>)
end

(** {2 Monadic Operations} *)

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) = struct
  let (>>=) = M.(>>=)

  let map_m f e = match e with
    | `Error s -> M.return (`Error s)
    | `Ok x -> f x >>= fun y -> M.return (`Ok y)

  let sequence_m m = map_m (fun x->x) m

  let fold_m f acc e = match e with
    | `Error _ -> M.return acc
    | `Ok x -> f acc x >>= fun y -> M.return y

  let retry_m n f =
    let rec retry n acc = match n with
    | 0 -> M.return (fail (List.rev acc))
    | _ ->
        f () >>= function
        | `Ok x -> M.return (`Ok x)
        | `Error e -> retry (n-1) (e::acc)
    in retry n []
end

(** {2 Conversions} *)

let to_opt = function
  | `Ok x -> Some x
  | `Error _ -> None

let of_opt = function
  | None -> `Error "of_opt"
  | Some x -> `Ok x

let to_seq e k = match e with
  | `Ok x -> k x
  | `Error _ -> ()

(** {2 IO} *)

let pp pp_x buf e = match e with
  | `Ok x -> Printf.bprintf buf "ok(%a)" pp_x x
  | `Error s -> Printf.bprintf buf "error(%s)" s

let pp' pp_x pp_e buf e = match e with
  | `Ok x -> Printf.bprintf buf "ok(%a)" pp_x x
  | `Error s -> Printf.bprintf buf "error(%a)" pp_e s

let print pp_x fmt e = match e with
  | `Ok x -> Format.fprintf fmt "@[ok(@,%a)@]" pp_x x
  | `Error s -> Format.fprintf fmt "@[error(@,%s)@]" s

let print' pp_x pp_e fmt e = match e with
  | `Ok x -> Format.fprintf fmt "@[ok(@,%a)@]" pp_x x
  | `Error s -> Format.fprintf fmt "@[error(@,%a)@]" pp_e s
