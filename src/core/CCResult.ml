(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Error Monad} *)

type 'a iter = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Basics} *)

type nonrec (+'good, +'bad) result = ('good, 'bad) result =
  | Ok of 'good
  | Error of 'bad

type (+'good, +'bad) t = ('good, 'bad) result =
  | Ok of 'good
  | Error of 'bad

let return x = Ok x

let fail s = Error s

let fail_printf format =
  let buf = Buffer.create 64 in
  Printf.kbprintf
    (fun buf -> fail (Buffer.contents buf))
    buf format

(*$T
  (Error "ohno 42") = (fail_printf "ohno %d" 42)
*)

let fail_fprintf format =
  let buf = Buffer.create 64 in
  let out = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun out -> Format.pp_print_flush out (); fail (Buffer.contents buf))
    out format

(*$T
  (Error "ohno 42") = (fail_fprintf "ohno %d" 42)
*)

let add_ctx msg x = match x with
  | Error e -> Error (e ^ "\ncontext:" ^ msg)
  | Ok x -> Ok x

let add_ctxf msg =
  let buf = Buffer.create 64 in
  let out = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun out e -> Format.pp_print_flush out (); add_ctx (Buffer.contents buf) e)
    out msg

(*$=
   (Error "error\ncontext:message(number 42, foo: true)") \
     (add_ctxf "message(number %d, foo: %B)" 42 true (Error "error"))
*)

let of_exn e =
  let msg = Printexc.to_string e in
  Error msg

let of_exn_trace e =
  let res = Printf.sprintf "%s\n%s"
      (Printexc.to_string e) (Printexc.get_backtrace ())
  in
  Error res

let map f e = match e with
  | Ok x -> Ok (f x)
  | Error s -> Error s

let map_err f e = match e with
  | Ok _ as res -> res
  | Error y -> Error (f y)

let map2 f g e = match e with
  | Ok x -> Ok (f x)
  | Error s -> Error (g s)

let iter f e = match e with
  | Ok x -> f x
  | Error _ -> ()

let iter_err f e = match e with
  | Ok _ -> ()
  | Error err -> f err

(*$R iter_err
  let called_with = ref None in
  let f e = called_with := Some e in
  iter_err f (Ok 1);
  assert_bool "should not apply when Ok" (!called_with = None);
  iter_err f (Error 1);
  assert_bool "should apply f to Error" (!called_with = Some 1)
*)

exception Get_error

let get_exn = function
  | Ok x -> x
  | Error _ -> raise Get_error

let get_or e ~default = match e with
  | Ok x -> x
  | Error _ -> default

let get_or_failwith = function
  | Ok x -> x
  | Error msg -> failwith msg

(*$T
  get_or_failwith (Ok 1) = 1
  try ignore @@ get_or_failwith (Error "e"); false with Failure msg -> msg = "e"
*)

let get_lazy default_fn x = match x with
  | Ok x -> x
  | Error e -> default_fn e

(*$= get_lazy
  (get_lazy (fun _ -> 2) (Ok 1)) (1)
  (get_lazy (fun _ -> 2) (Error "error")) (2)
*)

let map_or f e ~default = match e with
  | Ok x -> f x
  | Error _ -> default

let catch e ~ok ~err = match e with
  | Ok x -> ok x
  | Error y -> err y

let flat_map f e = match e with
  | Ok x -> f x
  | Error s -> Error s

let equal ~err eq a b = match a, b with
  | Ok x, Ok y -> eq x y
  | Error s, Error s' -> err s s'
  | _ -> false

let compare ~err cmp a b = match a, b with
  | Ok x, Ok y -> cmp x y
  | Ok _, _  -> 1
  | _, Ok _ -> -1
  | Error s, Error s' -> err s s'

let fold ~ok ~error x = match x with
  | Ok x -> ok x
  | Error s -> error s

let fold_ok f acc r = match r with
  | Ok x -> f acc x
  | Error _ -> acc

(*$=
  42 (fold_ok (+) 2 (Ok 40))
  40 (fold_ok (+) 40 (Error "foo"))
*)

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error = function
  | Ok _ -> false
  | Error _ -> true

(** {2 Wrappers} *)

let guard f =
  try Ok (f ())
  with e -> Error e

let guard_str f =
  try Ok (f())
  with e -> of_exn e

let guard_str_trace f =
  try Ok (f())
  with e -> of_exn_trace e

let wrap1 f x =
  try return (f x)
  with e -> Error e

let wrap2 f x y =
  try return (f x y)
  with e -> Error e

let wrap3 f x y z =
  try return (f x y z)
  with e -> Error e

(** {2 Applicative} *)

let pure = return

let (<*>) f x = match f with
  | Error s -> fail s
  | Ok f -> map f x

let join t = match t with
  | Ok (Ok o) -> Ok o
  | Ok (Error e) -> Error e
  | (Error _) as e -> e

let both x y = match x,y with
  | Ok o, Ok o' -> Ok (o, o')
  | Ok _, Error e -> Error e
  | Error e, _  -> Error e

(** {2 Collections} *)

let map_l f l =
  let rec map acc l = match l with
    | [] -> Ok (List.rev acc)
    | x::l' ->
      match f x with
        | Error s -> Error s
        | Ok y -> map (y::acc) l'
  in map [] l

let flatten_l l =
  let rec loop acc l = match l with
    | [] -> Ok (List.rev acc)
    | Ok x::l' -> loop (x::acc) l'
    | Error e::_ -> Error e
  in loop [] l

(*$=
  (Ok []) (flatten_l [])
  (Ok [1;2;3]) (flatten_l [Ok 1; Ok 2; Ok 3])
  (Error "ohno") (flatten_l [Ok 1; Error "ohno"; Ok 2; Ok 3; Error "wut"])
*)

exception LocalExit

let fold_iter f acc seq =
  let err = ref None in
  try
    let acc = ref acc in
    seq
      (fun x -> match f !acc x with
         | Error s -> err := Some s; raise LocalExit
         | Ok y -> acc := y);
    Ok !acc
  with LocalExit ->
  match !err with None -> assert false | Some s -> Error s

let fold_l f acc l = fold_iter f acc (fun k -> List.iter k l)

(** {2 Misc} *)

let choose l =
  let rec find_ = function
    | [] -> raise Not_found
    | ((Ok _) as res) :: _ -> res
    | (Error _) :: l' -> find_ l'
  in
  try find_ l
  with Not_found ->
    let l' = List.map (function Error s -> s | Ok _ -> assert false) l in
    Error l'

let retry n f =
  let rec retry n acc = match n with
    | 0 -> fail (List.rev acc)
    | _ ->
      match f () with
        | Ok _ as res -> res
        | Error e -> retry (n-1) (e::acc)
  in retry n []

(** {2 Infix} *)

module Infix = struct
  let (>|=) e f = map f e
  let (>>=) e f = flat_map f e
  let (<*>) = (<*>)

  include CCShimsMkLet_.Make2(struct
      type ('a,'e) t = ('a,'e) result
      let (>>=) = (>>=)
      let (>|=) = (>|=)
      let monoid_product x1 x2 = match x1, x2 with
        | Ok x, Ok y -> Ok (x,y)
        | Error e, _ -> Error e
        | _, Error e -> Error e
    end)
end

include Infix

(** {2 Monadic Operations} *)

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) = struct
  let (>>=) = M.(>>=)

  let map_m f e = match e with
    | Error s -> M.return (Error s)
    | Ok x -> f x >>= fun y -> M.return (Ok y)

  let sequence_m m = map_m (fun x->x) m

  let fold_m f acc e = match e with
    | Error _ -> M.return acc
    | Ok x -> f acc x >>= fun y -> M.return y

  let retry_m n f =
    let rec retry n acc = match n with
      | 0 -> M.return (fail (List.rev acc))
      | _ ->
        f () >>= function
        | Ok x -> M.return (Ok x)
        | Error e -> retry (n-1) (e::acc)
    in retry n []
end

(** {2 Conversions} *)

let to_opt = function
  | Ok x -> Some x
  | Error _ -> None

let of_opt = function
  | None -> Error "of_opt"
  | Some x -> Ok x

let to_std_seq e () = match e with
  | Ok x -> Seq.Cons (x, Seq.empty)
  | Error _ -> Seq.Nil

let to_iter e k = match e with
  | Ok x -> k x
  | Error _ -> ()

type ('a, 'b) error = [`Ok of 'a | `Error of 'b]

let of_err = function
  | `Ok x -> Ok x
  | `Error y -> Error y

let to_err = function
  | Ok x -> `Ok x
  | Error y -> `Error y

(** {2 IO} *)

let pp pp_x fmt e = match e with
  | Ok x -> Format.fprintf fmt "@[ok(@,%a)@]" pp_x x
  | Error s -> Format.fprintf fmt "@[error(@,%s)@]" s

let pp' pp_x pp_e fmt e = match e with
  | Ok x -> Format.fprintf fmt "@[ok(@,%a)@]" pp_x x
  | Error s -> Format.fprintf fmt "@[error(@,%a)@]" pp_e s
