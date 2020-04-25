(* module Deque = Core_kernel.Deque *)
module Int_map = CCMap.Make(CCInt)
module Int_set = CCSet.Make(CCInt)

let dup = CCPair.dup
let id = CCFun.id
let ns n = List.init n CCFun.id
let iter_range n f = List.iter f (ns n)

let gen_cons x xs =
  let saw_x = ref false in
  fun () ->
    if !saw_x then (saw_x := true; Some x)
    else xs ()

let front = Sek.front
let dummy = 0

let types = [
  "Stdlib.List", (fun n -> Obj.magic @@ ns n);
  "Stdlib.Array", (fun n -> Obj.magic @@ Array.init n id);
  "Stdlib.Hashtbl", (fun n -> Obj.magic @@ CCHashtbl.of_iter Iter.(init dup |> take n));
  "Base.Hashtbl", (fun n -> Obj.magic @@ Base.Hashtbl.Poly.of_alist_exn (List.init n dup));
  "Stdlib.Map", (fun n -> Obj.magic @@ Int_map.of_iter Iter.(init dup |> take n));
  "Stdlib.Set", (fun n -> Obj.magic @@ Int_set.of_iter Iter.(init id |> take n));
  "CCFun_vec", (fun n -> Obj.magic @@ CCFun_vec.of_list (ns n));
  "CCRAL", (fun n -> Obj.magic @@ CCRAL.of_list (ns n));
  "BatVect", (fun n -> Obj.magic @@ BatVect.of_list (ns n));
  "Sek.Persistent", (fun n -> Obj.magic @@ List.fold_left (Sek.Persistent.push front) (Sek.Persistent.create dummy) (ns n));
  "Sek.Ephemeral", (fun n -> Obj.magic @@ let c = Sek.Ephemeral.create dummy in iter_range n (Sek.Ephemeral.push front c); c);
  "CCVector", (fun n -> Obj.magic @@ let c = CCVector.create () in iter_range n (CCVector.push c); c);
  (* "Core_kernel.Deque", (fun n -> Obj.magic @@ let c = Deque.create () in iter_range n (Deque.enqueue_back c); c); *)
  "Base.Queue", (fun n -> Obj.magic @@ let c = Base.Queue.create () in iter_range n (Base.Queue.enqueue c); c);
  "Stdlib.Queue", (fun n -> Obj.magic @@ (let q = Queue.create () in iter_range n (fun x -> Queue.push x q); q));
  "CCQueue", (fun n -> Obj.magic @@ CCDeque.of_list (ns n));
  "Iter", (fun n -> Obj.magic @@ List.fold_right Iter.cons (ns n) Iter.empty);
  "Gen", (fun n -> Obj.magic @@ List.fold_right gen_cons (ns n) Gen.empty);
  "Stdlib.Seq", (fun n -> Obj.magic @@ List.fold_right OSeq.cons (ns n) OSeq.empty);
]

let () =
  let sizes = [0; 1; 10; 100; 1000; 10000] in
  Printf.printf "%-20s  " "";
  sizes |> List.iter (fun n -> Printf.printf "%6d " n);
  Printf.printf "\n";
  types
  |> List.iter (fun (name, create) ->
      Printf.printf "%-20s: " name;
      sizes
      |> List.iter (fun n ->
          let obj = create n in
          let size = Objsize.size_w obj in
          (* let size = Obj.reachable_words (Obj.repr obj) in *)
          Printf.printf "%6d " size
        );
      Printf.printf "\n"
    )
