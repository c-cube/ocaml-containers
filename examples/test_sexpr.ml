
(** {2 Test sequences} *)

(** print a list of items using the printing function *)
let pp_list ?(sep=", ") pp_item formatter l = 
  Sequence.pp_seq ~sep pp_item formatter (Sequence.of_list l)

(** Set of integers *)
module ISet = Set.Make(struct type t = int let compare = compare end)
let iset = (module ISet : Set.S with type elt = int and type t = ISet.t)

module OrderedString = struct type t = string let compare = compare end
module SMap = Sequence.Map.Make(OrderedString)

let my_map = SMap.of_seq (Sequence.of_list ["1", 1; "2", 2; "3", 3; "answer", 42])

let sexpr = "(foo bar (bazz quux hello 42) world (zoo foo bar (1 2 (3 4))))"

type term = | Lambda of term | Const of string | Var of int | Apply of term * term

let random_term () =
  let max = 10
  and num = ref 0 in
  let rec build depth =
    if depth > 4 || !num > max then Const (random_const ()) else
    match Random.int 6 with
    | 0 -> if depth > 0 then Var (Random.int depth) else Const (random_const ())
    | 1 -> incr num; Lambda (build (depth+1))
    | 2 -> Const (random_const ())
    | _ -> incr num; Apply ((build depth), (build depth))
  and random_const () = [|"a"; "b"; "c"; "f"; "g"; "h"|].(Random.int 6)
  in build 0

let rec sexpr_of_term t =
  let f t k = match t with
    | Var i -> Sexpr.output_str "var" (string_of_int i) k
    | Lambda t' -> Sexpr.output_seq "lambda" (sexpr_of_term t') k
    | Apply (t1, t2) -> Sexpr.output_seq "apply" (Sequence.append (sexpr_of_term t1) (sexpr_of_term t2)) k
    | Const s -> Sexpr.output_str "const" s k
  in Sequence.from_iter (f t)

let term_parser =
  let open Sexpr in
  let rec p_term () =
    left >>
    (("lambda", p_lambda) ^|| ("var", p_var) ^|| ("const", p_const) ^||
      ("apply", p_apply) ^|| fail "bad term") >>= fun x ->
    right >> return x
  and p_apply () =
    p_term () >>= fun x ->
    p_term () >>= fun y ->
    return (Apply (x,y))
  and p_var () = p_int >>= fun i -> return (Var i)
  and p_const () = p_str >>= fun s -> return (Const s)
  and p_lambda () = p_term () >>= fun t -> return (Lambda t)
  in p_term ()

let term_of_sexp seq = Sexpr.parse term_parser seq

let test_term () =
  let t = random_term () in
  Format.printf "@[<h>random term: %a@]@." Sexpr.pp_tokens (sexpr_of_term t);
  let tokens = sexpr_of_term t in
  let t' = term_of_sexp tokens in
  Format.printf "@[<h>parsed: %a@]@." Sexpr.pp_tokens (sexpr_of_term t');
  ()

let _ =
  (* lists *)
  let l = [0;1;2;3;4;5;6] in
  let l' = Sequence.to_list
    (Sequence.filter (fun x -> x mod 2 = 0) (Sequence.of_list l)) in
  let l'' = Sequence.to_list
    (Sequence.take 3 (Sequence.drop 1 (Sequence.of_list l))) in
  let h = Hashtbl.create 3 in
  for i = 0 to 5 do
    Hashtbl.add h i (i*i);
  done;
  let l2 = Sequence.to_list
    (Sequence.map (fun (x, y) -> (string_of_int x) ^ " -> " ^ (string_of_int y))
      (Sequence.of_hashtbl h))
  in
  let l3 = Sequence.to_list (Sequence.rev (Sequence.int_range ~start:0 ~stop:42)) in
  let set = List.fold_left (fun set x -> ISet.add x set) ISet.empty [4;3;100;42] in
  let l4 = Sequence.to_list (Sequence.of_set iset set) in
  Format.printf "l=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l;
  Format.printf "l'=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l';
  Format.printf "l''=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l'';
  Format.printf "l2=@[<h>[%a]@]@." (pp_list Format.pp_print_string) l2;
  Format.printf "l3=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l3;
  Format.printf "s={@[<h>%a@]}@." (Sequence.pp_seq Format.pp_print_int) (Sequence.of_set iset set);
  Format.printf "l4=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l4;
  Format.printf "l3[:5]+l4=@[<h>[%a]@]@." (Sequence.pp_seq Format.pp_print_int)
    (Sequence.of_array
      (Sequence.to_array (Sequence.append
        (Sequence.take 5 (Sequence.of_list l3)) (Sequence.of_list l4))));
  (* sequence, persistent, etc *)
  let seq = Sequence.int_range ~start:0 ~stop:100000 in
  let seq' = Sequence.persistent seq in
  let stream = Sequence.to_stream seq' in
  Format.printf "test length [0..100000]: persistent1 %d, stream %d, persistent2 %d"
    (Sequence.length seq') (Sequence.length (Sequence.of_stream stream)) (Sequence.length seq');
  (* maps *)
  Format.printf "@[<h>map: %a@]@."
    (Sequence.pp_seq (fun formatter (k,v) -> Format.fprintf formatter "\"%s\" -> %d" k v))
    (SMap.to_seq my_map);
  let module MyMapSeq = Sequence.Map.Adapt(Map.Make(OrderedString)) in
  let my_map' = MyMapSeq.of_seq (Sequence.of_list ["1", 1; "2", 2; "3", 3; "answer", 42]) in
  Format.printf "@[<h>map: %a@]@."
    (Sequence.pp_seq (fun formatter (k,v) -> Format.fprintf formatter "\"%s\" -> %d" k v))
    (MyMapSeq.to_seq my_map');
  (* sum *)
  let n = 1000000 in
  let sum = Sequence.fold (+) 0 (Sequence.take n (Sequence.repeat 1)) in
  Format.printf "%dx1 = %d@." n sum;
  assert (n=sum);
  (* sexpr *)
  let s = Sexpr.of_seq (Sexpr.lex (Sequence.of_str sexpr)) in
  let s = Sexpr.of_seq (Sequence.map
    (function | `Atom s -> `Atom (String.capitalize s) | tok -> tok)
    (Sexpr.traverse s))
  in
  Format.printf "@[<hov2>transform @[<h>%s@] into @[<h>%a@]@]@." sexpr (Sexpr.pp_sexpr ~indent:false) s;
  Format.printf "@[<hv2> cycle:%a@]@." Sexpr.pp_tokens
    (Sequence.concat (Sequence.take 10 (Sequence.repeat (Sexpr.traverse s))));
  (* sexpr parsing/printing *)
  for i = 0 to 20 do
    Format.printf "%d-th term test@." i;
    test_term ();
  done;
  ()
