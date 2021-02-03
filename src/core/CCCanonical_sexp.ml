
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Simple S-expression parsing/printing} *)

open CCShims_

type 'a or_error = ('a, string) result
type 'a gen = unit -> 'a option

module type SEXP = CCSexp_intf.BASIC_SEXP
module type S = CCSexp_intf.S0

let equal_string (a : string) b = Stdlib.(=) a b
let compare_string (a : string) b = Stdlib.compare a b

let _with_in filename f =
  let ic = open_in filename in
  try
    let x = f ic in
    close_in ic;
    x
  with e ->
    close_in ic;
    Error (Printexc.to_string e)

let _with_out filename f =
  let oc = open_out filename in
  try
    let x = f oc in
    close_out oc;
    x
  with e ->
    close_out oc;
    raise e

module Make(Sexp : SEXP) = struct
  type t = Sexp.t
  type sexp = t

  let atom = Sexp.atom
  let list = Sexp.list
  let of_int x = Sexp.atom (string_of_int x)
  let of_float x = Sexp.atom (string_of_float x)
  let of_bool x = Sexp.atom (string_of_bool x)
  let of_unit = Sexp.list []
  let of_list l = Sexp.list l
  let of_rev_list l = Sexp.list (List.rev l)
  let of_pair (x,y) = Sexp.list [x;y]
  let of_triple (x,y,z) = Sexp.list [x;y;z]
  let of_quad (x,y,z,u) = Sexp.list [x;y;z;u]

  let of_variant name args = Sexp.list (Sexp.atom name :: args)
  let of_field name t = Sexp.list [Sexp.atom name; t]
  let of_record l =
    Sexp.list (List.map (fun (n,x) -> of_field n x) l)

  (** {3 Printing} *)

  let rec to_buf b t =
    Sexp.match_ t
      ~atom:(fun s -> Printf.bprintf b "%d:%s" (String.length s) s)
      ~list:(function
          | [] -> Buffer.add_string b "()"
          | [x] -> Printf.bprintf b "(%a)" to_buf x
          | l ->
            Buffer.add_char b '(';
            List.iter (to_buf b) l;
            Buffer.add_char b ')')

  let to_string t =
    let b = Buffer.create 128 in
    to_buf b t;
    Buffer.contents b

  let rec pp_noindent fmt t =
    Sexp.match_ t
      ~atom:(fun s -> Format.fprintf fmt "%d:%s" (String.length s) s)
      ~list:(function
          | [] -> Format.pp_print_string fmt "()"
          | [x] -> Format.fprintf fmt "(%a)" pp_noindent x
          | l ->
            Format.fprintf fmt "(";
            List.iter (pp_noindent fmt) l;
            Format.fprintf fmt ")")

  let pp = pp_noindent

  let rec to_chan oc t =
    Sexp.match_ t
      ~atom:(fun s -> Printf.fprintf oc "%d:%s" (String.length s) s)
      ~list:(function
          | [] -> output_string oc "()"
          | [x] -> Printf.fprintf oc "(%a)" to_chan x
          | l ->
            output_char oc '(';
            List.iter (to_chan oc) l;
            output_char oc ')')

  let to_file_iter filename iter =
    _with_out filename
      (fun oc -> iter (fun t -> to_chan oc t))

  let to_file filename t = to_file_iter filename (fun k -> k t)

  (** {3 Parsing} *)

  module type INPUT = sig
    exception EOF
    val read_char : unit -> char
    val read_string : int -> string
  end

  module Decoder(I:INPUT) = struct
    let[@inline] is_num_ c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
    let[@inline] as_num_ c = Char.code c - Char.code '0'

    let next_ () : sexp or_error * bool =
      let rec read_string_len n =
        match I.read_char () with
        | c when is_num_ c -> read_string_len (n * 10 + as_num_ c)
        | ':' ->
          let s = I.read_string n in
          atom s
        | _ -> failwith "expected string length"

      and eat_colon () =
        match I.read_char () with
        | ':' -> ()
        | _ -> failwith "expected ':'"

      and read_in_paren acc =
        match I.read_char () with
        | ')' -> list (List.rev acc)
        | c when is_num_ c ->
          let sexp = read_string_len (as_num_ c) in
          read_in_paren (sexp::acc)
        | '(' ->
          let sexp = read_in_paren [] in
          read_in_paren (sexp::acc)
        | _ -> failwith "expected list of sexprs"
      in
      (* read a S-expr *)
      try
        begin match I.read_char () with
          | exception I.EOF -> Error "unexpected EOF", true
          | '(' -> Ok (read_in_paren []), false
          | '0' -> eat_colon (); Ok (atom ""), false
          | c when is_num_ c -> Ok (read_string_len (as_num_ c)), false
          | _ -> Error "unexpected char, expected toplevel sexpr", false
        end
      with Failure e -> Error e, false

    let to_list () : _ or_error =
      let rec iter acc =
        match next_ () with
        | Error _, true -> Ok (List.rev acc)
        | Ok x, _ -> iter (x::acc)
        | Error _ as res, _ -> res
      in
      try iter []
      with e -> Error (Printexc.to_string e)

    let[@inline] next_or_error () : _ or_error = fst (next_ ())
  end[@@inline]

  module Decoder_str(X : sig val s : string end) =
    Decoder(struct
      exception EOF
      let i = ref 0
      let n = String.length X.s
      let read_char () =
        if !i >= n then raise_notrace EOF;
        let c = String.unsafe_get X.s !i in
        incr i;
        c
      let read_string len =
        if !i + len > n then raise_notrace EOF;
        let res = String.sub X.s !i len in
        i := !i + len;
        res
    end)[@@inline]

  let parse_string s : t or_error =
    let module D = Decoder_str(struct let s=s end) in
    D.next_or_error ()

  let parse_string_list s : t list or_error =
    let module D = Decoder_str(struct let s=s end) in
    D.to_list ()

  module Decoder_ic(X : sig val ic : in_channel end) =
    Decoder(struct
      exception EOF = End_of_file
      let[@inline] read_char () = input_char X.ic
      let read_string n =
        match n with
        | 0 -> ""
        | 1 -> String.make 1 (read_char ())
        | _ ->
          let buf = Bytes.make n '\000' in
          let i = ref 0 in
          while !i < n do
            let len = input X.ic buf !i (n - !i) in
            i := !i + len;
          done;
          Bytes.unsafe_to_string buf
    end)[@@inline]

  let parse_chan_ ?file ic : sexp or_error =
    let module D = Decoder_ic(struct let ic=ic end) in
    match D.next_or_error(), file with
    | Error s, Some file -> Error (Printf.sprintf "%s in '%s'" s file)
    | r, _ -> r

  let parse_chan_list_ ?file ic =
    let module D = Decoder_ic(struct let ic=ic end) in
    match D.to_list (), file with
    | Error s, Some file -> Error (Printf.sprintf "%s in '%s'" s file)
    | r, _ -> r

  let parse_chan ic = parse_chan_ ic
  let parse_chan_list ic = parse_chan_list_ ic

  let parse_chan_gen ic =
    let module D = Decoder_ic(struct let ic=ic end) in
    fun () ->
      match D.next_ () with
      | _, true -> None
      | Error e, _ -> Some (Error e)
      | Ok x, _ -> Some (Ok x)

  let parse_file filename = _with_in filename (parse_chan_ ~file:filename)

  let parse_file_list filename = _with_in filename (parse_chan_list_ ~file:filename)
end

type t = [
  | `Atom of string
  | `List of t list
]

let rec equal a b = match a, b with
  | `Atom s1, `Atom s2 ->
    equal_string s1 s2
  | `List l1, `List l2 ->
    begin try List.for_all2 equal l1 l2 with Invalid_argument _ -> false end
  | `Atom _, _ | `List _, _ -> false

let rec compare_list a b = match a, b with
  | [], [] -> 0
  | [], _::_ -> -1
  | _::_, [] -> 1
  | x::xs, y::ys ->
    begin match compare x y with
      | 0 -> compare_list xs ys
      | c -> c
    end

and compare a b = match a, b with
  | `Atom s1, `Atom s2 -> compare_string s1 s2
  | `List l1, `List l2 -> compare_list l1 l2
  | `Atom _, _ -> -1
  | `List _, _ -> 1

module Basic_ = struct
  type nonrec t = t
  let atom x = `Atom x
  let list x = `List x
  let match_ x ~atom ~list = match x with
    | `Atom x -> atom x
    | `List l -> list l
end

include (Make(Basic_) : S with type t := t)

(*$inject
  let csexp_bijective s = to_string s |> parse_string = Ok s
*)

(*$= & ~printer:CCFormat.(to_string (Dump.result pp))
  (Ok (`List [`Atom ""])) (parse_string {|(0:)|})
  (Ok (`List [`Atom "a"; `Atom "b "])) (parse_string {|(1:a2:b )|})
*)

(*$T
  csexp_bijective (`List [`Atom ""])
*)

(*$inject
  let sexp_gen =
    let mkatom a = `Atom a and mklist l = `List l in
    let atom = Q.Gen.(map mkatom (string_size ~gen:char (1 -- 30))) in
    let gen = Q.Gen.(
      sized (fix
        (fun self n st -> match n with
        | 0 -> atom st
        | _ ->
          frequency
            [ 1, atom
            ; 2, map mklist (list_size (0 -- 10) (self (n/10)))
            ] st
        )
    )) in
    let rec small = function
      | `Atom s -> String.length s
      |  `List l -> List.fold_left (fun n x->n+small x) 0 l
    and print = function
      | `Atom s -> Printf.sprintf "`Atom \"%s\"" s
      | `List l -> "`List " ^ Q.Print.list print l
    and shrink = function
      | `Atom s -> Q.Iter.map mkatom (Q.Shrink.string s)
      | `List l -> Q.Iter.map mklist (Q.Shrink.list ~shrink l)
    in
    Q.make ~print ~small ~shrink gen
*)

(*$Q & ~count:100
    sexp_gen csexp_bijective
*)

(*$R
  let s1 =
    `List (CCList.init 100_000
      (fun i -> `List [`Atom "-"; `Atom (string_of_int i); `Atom ")(\n]"])) in
  let str = to_string s1 in
  match parse_string str with
    | Ok s2 -> assert_equal s1 s2
    | Error e -> assert_failure e
*)
