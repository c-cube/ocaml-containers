(** Test hash functions *)

type tree =
  | Empty
  | Node of int * tree list

let mk_node i l = Node (i,l)

let random_tree =
  CCRandom.(fix
    ~base:(return Empty)
    ~subn:[int 10, (fun sublist -> pure mk_node <*> small_int <*> sublist)]
    (int_range 15 150)
  )

let random_list =
  CCRandom.(
    int 5 >>= fun len ->
    CCList.random_len len random_tree
  )

let rec eq t1 t2 = match t1, t2 with
  | Empty, Empty -> true
  | Node(i1,l1), Node (i2,l2) -> i1=i2 && CCList.equal eq l1 l2
  | Node _, _
  | _, Node _ -> false

let rec hash_tree t = match t with
  | Empty -> CCHash.string "empty"
  | Node (i, l) ->
    CCHash.(combine2 (int i) (list hash_tree l))

module H = Hashtbl.Make(struct
  type t = tree
  let equal = eq
  let hash = hash_tree
end)

let print_hashcons_stats st =
  let open Hashtbl in
    Format.printf
      "tbl stats: %d elements, num buckets: %d, max bucket: %d@."
      st.num_bindings st.num_buckets st.max_bucket_length;
    Array.iteri
      (fun i n -> Format.printf "  %d\t buckets have length %d@." n i)
      st.bucket_histogram

let () =
  let st = Random.State.make_self_init () in
  let n = 50_000 in
  Format.printf "generate %d elements...\n" n;
  let l = CCRandom.run ~st (CCList.random_len n random_tree) in
  (* with custom hashtable *)
  Format.printf "### custom hashtable\n";
  let tbl = H.create 256 in
  List.iter (fun t -> H.replace tbl t ()) l;
  print_hashcons_stats (H.stats tbl);
  (* with default hashtable *)
  Format.printf "### default hashtable\n";
  let tbl' = Hashtbl.create 256 in
  List.iter (fun t -> Hashtbl.replace tbl' t ()) l;
  print_hashcons_stats (Hashtbl.stats tbl');
  ()

