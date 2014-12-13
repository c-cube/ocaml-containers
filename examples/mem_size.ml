(** Compute the memory footprint of a value (and its subvalues). Reference is
    http://rwmj.wordpress.com/2009/08/05/ocaml-internals-part-2-strings-and-other-types/ *)



(** A graph vertex is an Obj.t value *)
let graph =
  let force x =
    if Obj.is_block x
      then
        let children = Sequence.map (fun i -> i, Obj.field x i) (0--(Obj.size x - 1)) in
        LazyGraph.Node (x, Obj.tag x, children)
      else
        LazyGraph.Node (x, Obj.obj x, Sequence.empty)
  in LazyGraph.make ~eq:(==) force

let word_size = Sys.word_size / 8

let size x =
  if Obj.is_block x
    then (1+Obj.size x) * word_size
    else word_size

let compute_size x =
  let o = Obj.repr x in
  let vertices = LazyGraph.bfs graph o in
  Sequence.fold (fun sum (o',_,_) -> size o' + sum) 0 vertices

let print_val fmt x =
  let o = Obj.repr x in
  let graph' = LazyGraph.map ~edges:(fun i -> [`Label (string_of_int i)])
                 ~vertices:(fun v -> [`Label (string_of_int v); `Shape "box"]) graph in
  LazyGraph.Dot.pp ~name:"value" graph' fmt (Sequence.singleton o)

let print_val_file filename x =
  let out = open_out filename in
  let fmt = Format.formatter_of_out_channel out in
  print_val fmt x;
  Format.pp_print_flush fmt ();
  close_out out

let process_val ~name x =
  print_val_file (Format.sprintf "/tmp/%s.dot" name) x;
  Format.printf "size of val is %d@." (compute_size x)

module ISet = Set.Make(struct type t = int let compare = compare end)

let mk_circ n =
  let start = Sequence.to_list (1--n) in
  (* make the end of the list point to its beginning *)
  let rec cycle l = match l with
  | [] -> assert false
  | [_] -> Obj.set_field (Obj.repr l) 1 (Obj.repr start)
  | _::l' -> cycle l'
  in
  cycle start;
  start

let _ =
  let s = Sequence.fold (fun s x -> ISet.add x s) ISet.empty (1--100) in
  process_val ~name:"foo" s;
  let l = Sequence.to_list (Sequence.map (fun i -> Sequence.to_list (i--(i+42)))
    (Sequence.of_list [0;100;1000])) in
  process_val ~name:"bar" l;
  let l' = mk_circ 100 in
  process_val ~name:"baaz" l';
  ()
