
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Printer Combinators}

This module provides combinators to build printers for user-defined types.
It doesn't try to do {b pretty}-printing (see for instance Pprint for this),
but a simple way to print complicated values without writing a lot of code.
*)

type 'a sequence = ('a -> unit) -> unit

type 'a t = Buffer.t -> 'a -> unit
(** A printer for the type ['a] *)

(** {2 Combinators} *)

let silent _buf _ = ()

let unit buf () = Buffer.add_string buf "()"
let int buf i = Buffer.add_string buf (string_of_int i)
let string buf s = Buffer.add_string buf s
let bool buf b = Printf.bprintf buf "%B" b
let float3 buf f = Printf.bprintf buf "%.3f" f
let float buf f = Buffer.add_string buf (string_of_float f)
let char buf c = Buffer.add_char buf c

let list ?(start="[") ?(stop="]") ?(sep=", ") pp buf l =
  let rec pp_list l = match l with
  | x::((_::_) as l) ->
    pp buf x;
    Buffer.add_string buf sep;
    pp_list l
  | x::[] -> pp buf x
  | [] -> ()
  in
  Buffer.add_string buf start;
  pp_list l;
  Buffer.add_string buf stop

let array ?(start="[") ?(stop="]") ?(sep=", ") pp buf a =
  Buffer.add_string buf start;
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Buffer.add_string buf sep);
    pp buf a.(i)
  done;
  Buffer.add_string buf stop

let arrayi ?(start="[") ?(stop="]") ?(sep=", ") pp buf a =
  Buffer.add_string buf start;
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Buffer.add_string buf sep);
    pp buf (i, a.(i))
  done;
  Buffer.add_string buf stop

let seq ?(start="[") ?(stop="]") ?(sep=", ") pp buf seq =
  Buffer.add_string buf start;
  let first = ref true in
  seq (fun x ->
    (if !first then first := false else Buffer.add_string buf sep);
    pp buf x);
  Buffer.add_string buf stop

let opt pp buf x = match x with
  | None -> Buffer.add_string buf "none"
  | Some x -> Printf.bprintf buf "some %a" pp x

let pair ppa ppb buf (a, b) =
  Printf.bprintf buf "(%a, %a)" ppa a ppb b

let triple ppa ppb ppc buf (a, b, c) =
  Printf.bprintf buf "(%a, %a, %a)" ppa a ppb b ppc c

let quad ppa ppb ppc ppd buf (a, b, c, d) =
  Printf.bprintf buf "(%a, %a, %a, %a)" ppa a ppb b ppc c ppd d

let map f pp buf x =
  pp buf (f x);
  ()

(** {2 IO} *)

let output oc pp x =
  let buf = Buffer.create 64 in
  pp buf x;
  Buffer.output_buffer oc buf

let to_string pp x =
  let buf = Buffer.create 64 in
  pp buf x;
  Buffer.contents buf

let sprintf format =
  let buffer = Buffer.create 64 in
  Printf.kbprintf
    (fun _fmt -> Buffer.contents buffer)
    buffer
    format

let fprintf oc format =
  let buffer = Buffer.create 64 in
  Printf.kbprintf
    (fun _fmt -> Buffer.output_buffer oc buffer)
    buffer
    format

let printf format = fprintf stdout format
let eprintf format = fprintf stderr format

let _with_file_out filename f =
  let oc = open_out filename in
  begin try
    let x = f oc in
    close_out oc;
    x
  with e ->
    close_out_noerr oc;
    raise e
  end

let to_file filename format =
  _with_file_out filename (fun oc -> fprintf oc format)

(** {2 Monadic IO} *)

module type MONAD_IO = sig
  type 'a t     (** the IO monad *)

  type output   (** Output channels *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val write : output -> string -> unit t
end

module MakeIO(M : MONAD_IO) = struct
  let output out pp x =
    let buf = Buffer.create 128 in
    pp buf x;
    M.write out (Buffer.contents buf)

  let printl out pp x =
    let buf = Buffer.create 128 in
    pp buf x;
    Buffer.add_char buf '\n';
    M.write out (Buffer.contents buf)

  let fprintf out format =
    let buf = Buffer.create 128 in
    Printf.kbprintf
      (fun buf -> M.write out (Buffer.contents buf))
      buf
      format
end
