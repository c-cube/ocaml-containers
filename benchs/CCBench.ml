
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

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption) however
caused and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
*)

(** {1 helpers for benchmarks} *)

let print_line_ fmt () =
  Format.pp_print_string fmt (CCString.repeat "*" 80);
  Format.pp_print_newline fmt ()

let print_list_ ?(sep=", ") pp_item fmt l =
  let rec print fmt l = match l with
    | x::((_::_) as l) ->
      pp_item fmt x;
      Format.pp_print_string fmt sep;
      Format.pp_print_cut fmt ();
      print fmt l
    | x::[] -> pp_item fmt x
    | [] -> ()
  in
  print fmt l

(** {2 Bench Tree} *)

module SMap = Map.Make(String)

type single_bench = unit -> Benchmark.samples
type bench =
  | Multiple of bench list * bench SMap.t
  | Bench of single_bench
  | WithInt of ((int -> bench) * int) list

let is_multiple = function
  | Multiple _ -> true
  | _ -> false

let rec merge_ t1 t2 = match t1, t2 with
  | Multiple (l, map), ((Bench _ | WithInt _) as x) ->
      Multiple (x :: l, map)
  | Multiple (l1, m1), Multiple (l2, m2) ->
      let m = SMap.merge
        (fun _ o1 o2 -> merge_opt_ o1 o2)
        m1 m2
      in
      Multiple (l1 @ l2, m)
  | (Bench _ | WithInt _), Multiple _ -> merge_ t2 t1
  | Bench _, _
  | WithInt _, _ ->
      Multiple ([t1; t2], SMap.empty)  (* composite *)
and merge_opt_ o1 o2 = match o1, o2 with
  | None, None -> None
  | Some o, None
  | None, Some o -> Some o
  | Some o1, Some o2 -> Some (merge_ o1 o2)

let mk_list = function
  | [] -> invalid_arg "mk_list"
  | x :: tail -> List.fold_left merge_ x tail

let raw f = Bench f

let throughput1 ?min_count ?style ?fwidth ?fdigits ?repeat time ?name f x =
  Bench (fun () ->
    Benchmark.throughput1 ?min_count ?style ?fwidth ?fdigits ?repeat time ?name f x)

let throughputN ?style ?fwidth ?fdigits ?repeat time f =
  Bench (fun () ->
    Benchmark.throughputN ?style ?fwidth ?fdigits ?repeat time f)

let (>::) n t =
  if n = "" then invalid_arg ">::";
  Multiple ([], SMap.singleton n t)

let (>:::) n l =
  if n = "" then invalid_arg ">:::";
  Multiple ([], SMap.singleton n (mk_list l))

let with_int f = function
  | [] -> invalid_arg "with_int: empty list"
  | l -> WithInt (List.map (fun n -> f, n) l)

let map_int l =
  if l = [] then invalid_arg "map_int";
  WithInt l

(* print the structure of the tree *)
let rec print fmt = function
  | Multiple (l, m) ->
      Format.fprintf fmt "@[<hv>%a%a@]"
        (print_list_ ~sep:"," print) l
        print_map m
  | WithInt l ->
      Format.fprintf fmt "@[<hv>[%a]@]"
        (print_list_ print_pair)
        (List.map (fun (f, n) -> n, f n) l)
  | Bench _ -> Format.fprintf fmt "<>"
and print_pair fmt (n,t) =
  Format.fprintf fmt "@[<h>%d: %a@]" n print t
and print_map fmt m =
  Format.pp_open_hvbox fmt 0;
  SMap.iter (fun n t -> Format.fprintf fmt "@[%s.%a@]" n print t) m;
  Format.pp_close_box fmt ()

(** {2 Path} *)

type path = string list

let print_path fmt path =
  Format.fprintf fmt "@[<h>%a@]"
    (print_list_ ~sep:"." Format.pp_print_string) path

let str_split_ ~by s =
  let len_by = String.length by in
  assert (len_by > 0);
  let l = ref [] in
  let n = String.length s in
  let rec search prev i =
    if i >= n
      then (
        if i>prev then l := String.sub s prev (n-prev) :: !l ;
        List.rev !l
      )
    else if is_prefix i 0
      then begin
        l := (String.sub s prev (i-prev)) :: !l;  (* save substring *)
        search (i+len_by) (i+len_by)
      end
    else search prev (i+1)
  and is_prefix i j =
    if j = len_by
      then true
    else if i = n
      then false
    else s.[i] = by.[j] && is_prefix (i+1) (j+1)
  in search 0 0

let parse_path s = str_split_ ~by:"." s

let () =
  assert (parse_path "foo.bar" = ["foo";"bar"]);
  assert (parse_path "foo" = ["foo"]);
  assert (parse_path "" = []);
  ()

let prefix path t = List.fold_right (fun s t -> s >:: t) path t

(** {2 Run} *)

(* run one atomic single_bench *)
let run_single_bench_ fmt path f =
  print_line_ fmt ();
  Format.fprintf fmt "run single_bench %a@." print_path (List.rev path);
  let res = f () in
  Benchmark.tabulate res

(* run all benchs *)
let rec run_all fmt path t = match t with
  | Bench f -> run_single_bench_ fmt path f
  | Multiple (l, m) ->
      List.iter (run_all fmt path) l;
      SMap.iter
        (fun n t' ->
          let path = n :: path in
          run_all fmt path t'
        ) m
  | WithInt l ->
      List.iter (fun (f, n) -> run_all fmt path (f n)) l

let run fmt t = run_all fmt [] t

let sprintf_ format =
  let b = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer b in
  Format.kfprintf
    (fun fmt -> Format.pp_print_flush fmt (); Buffer.contents b) fmt format

(* run all within a path *)
let rec run_path_rec_ fmt path remaining t = match t, remaining with
  | _, [] -> run_all fmt path t
  | Multiple (_, m), s :: remaining' ->
      begin try
        let t' = SMap.find s m in
        run_path_rec_ fmt (s::path) remaining' t'
      with Not_found ->
        let msg = sprintf_ "could not find %s under path %a"
          s print_path (List.rev path) in
        failwith msg
      end
  | WithInt l, _ ->
      List.iter (fun (f, n) -> run_path_rec_ fmt path remaining (f n)) l
  | Bench _, _::_ -> ()

let run_path fmt t path = run_path_rec_ fmt [] path t

let run_main ?(argv=Sys.argv) ?(out=Format.std_formatter) t =
  let path = ref [] in
  let do_print_tree = ref false in
  let set_path_ s = path := parse_path s in
  let options =
    [ "-p", Arg.String set_path_, "only apply to subpath"
    ; "-tree", Arg.Set do_print_tree, "print bench tree"
    ] in
  try
    Arg.parse_argv argv options (fun _ -> ()) "run benchmarks [options]";
    if !do_print_tree
      then Format.fprintf out "@[%a@]@." print t
      else (
        Format.printf "run on path %a@." print_path !path;
        run_path out t !path   (* regular path *)
      )
  with Arg.Help msg ->
    Format.pp_print_string out msg

(** {2 Global Registration} *)

module Glob = struct
  let tree_ = ref (Multiple ([], SMap.empty))

  let get () = !tree_

  let register ?(path=[]) new_t =
    tree_ := merge_ !tree_ (prefix path new_t)

  let register' ~path new_t =
    register ~path:(parse_path path) new_t

  let run_main ?argv ?out () =
    run_main ?argv ?out !tree_
end
