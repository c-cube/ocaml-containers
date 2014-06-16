
(** Example of printing trees: lambda-term evaluation *)

open Containers_misc

type term =
  | Lambda of string * term
  | App of term * term
  | Var of string

let _gensym =
  let r = ref 0 in
  fun () ->
    let s = Printf.sprintf "x%d" !r in
    incr r;
    s

module SSet = Set.Make(String)
module SMap = Map.Make(String)

let rec fvars t = match t with
  | Var s -> SSet.singleton s
  | Lambda (v,t') ->
      let set' = fvars t' in
      SSet.remove v set'
  | App (t1, t2) -> SSet.union (fvars t1) (fvars t2)

(* replace [var] with the term [by] *)
let rec replace t ~var ~by = match t with
  | Var s -> if s=var then by else t
  | App (t1,t2) -> App (replace t1 ~var ~by, replace t2 ~var ~by)
  | Lambda (v, t') when v=var -> t  (* no risk *)
  | Lambda (v, t') -> Lambda (v, replace t' ~var ~by)

(* rename [t] so that [var] doesn't occur in it *)
let rename ~var t =
  if SSet.mem var (fvars t)
    then replace t ~var ~by:(Var (_gensym ()))
    else t

let (>>=) o f = match o with
  | None -> None
  | Some x -> f x

let rec one_step t = match t with
  | App (Lambda (var, t1), t2) ->
      let t2' = rename ~var t2 in
      Some (replace t1 ~var ~by:t2')
  | App (t1, t2) ->
      begin match one_step t1 with
      | None ->
          one_step t2 >>= fun t2' ->
          Some (App (t1,t2'))
      | Some t1' ->
          Some (App (t1',t2))
      end
  | Var _ -> None
  | Lambda (v,t') ->
      one_step t' >>= fun t'' ->
      Some (Lambda (v, t''))

let normal_form t =
  let rec aux acc t = match one_step t with
    | None -> List.rev (t::acc)
    | Some t' -> aux (t::acc) t'
  in
  aux [] t

let _split_fuel f =
  assert (f>=2);
  if f=2 then 1,1
  else
    let x = 1+Random.int (f-1) in
    f-x, x

let _random_var () =
  let v = [| "x"; "y"; "z"; "u"; "w" |] in
  v.(Random.int (Array.length v))

let  _choose_var ~vars = match vars with
  | [] -> Var (_random_var ())
  | _::_ ->
      let i = Random.int (List.length vars) in
      List.nth vars i

let rec _random_term fuel vars =
  match Random.int 2 with
  | _ when fuel = 1 -> _choose_var ~vars
  | 0 ->
      let f1,f2 = _split_fuel fuel in
      App (_random_term f1 vars, _random_term f2 vars)
  | 1 ->
      let v = _random_var () in
      Lambda (v, _random_term (fuel-1) (Var v::vars))
  | _ -> assert false

let print_term t =
  PrintBox.mk_tree
    (function
      | Var v -> PrintBox.line v, []
      | App (t1,t2) -> PrintBox.line "app", [t1;t2]
      | Lambda (v,t') -> PrintBox.line "lambda", [Var v; t']
    ) t

let print_reduction t =
  let l = normal_form t in
  let l = List.map (fun t -> PrintBox.pad (print_term t)) l in
  PrintBox.vlist ~bars:false l

let () =
  Random.self_init ();
  let t = _random_term (5 + Random.int 20) [] in
  PrintBox.output ~indent:2 stdout (print_reduction t)
