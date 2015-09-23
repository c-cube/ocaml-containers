
(* goal: measure memory consumption *)

(* number of words allocated *)
let mem_allocated () =
  let gc = Gc.stat () in
  gc.Gc.minor_words +. gc.Gc.major_words -. gc.Gc.promoted_words

(* overhead in memory *)
let mem_occupied x = Objsize.size_kb (Obj.repr x)

type stats = {
  time: float;
  occ: int;
  alloc: float;
}

let measure_time_mem f =
  let mem_alloc1 = mem_allocated () in
  let start = Unix.gettimeofday() in
  let x = f () in
  let stop  = Unix.gettimeofday() in
  Gc.compact ();
  let mem_alloc2 = mem_allocated () in
  let mem_occupied = mem_occupied x in
  ignore x;
  { occ=mem_occupied;
    alloc=mem_alloc2-.mem_alloc1;
    time=stop -. start;
  }

let spf = Printf.sprintf

let do_test ~name f =
  Format.printf "test %s...@." name;
  let res = measure_time_mem f in
  Format.printf "  allocated:%.2f MB, occupied:%d kB, time: %.2f s@."
    (res.alloc *. 8. /. 1_000_000.)
    res.occ
    res.time

let test_hashtrie n =
  let module M = CCHashTrie.Make(CCInt) in
  do_test ~name:(spf "hashtrie(%d)" n)
    (fun () ->
      let m = M.of_seq Sequence.(1 -- n |> map (fun x-> x,x)) in
      m
    )

let test_hamt n =
  let module M = Hamt.Make'(CCInt) in
  do_test ~name:(spf "hamt(%d)" n)
    (fun () ->
      let m = Sequence.(1 -- n
        |> map (fun x-> x,x)
        |> fold (fun m (k,v) -> M.add k v m) M.empty
      ) in
      m
    )

let test_map n =
  let module M = CCMap.Make(CCInt) in
  do_test ~name:(spf "map(%d)" n)
    (fun () ->
      let m = M.of_seq Sequence.(1 -- n |> map (fun x-> x,x)) in
      m
    )

let test_wbt n =
  let module M = CCWBTree.Make(CCInt) in
  do_test ~name:(spf "wbt(%d)" n)
    (fun () ->
      let m = M.of_seq Sequence.(1 -- n |> map (fun x-> x,x)) in
      m
    )

let test_hashtbl n =
  let module H = CCHashtbl.Make(CCInt) in
  do_test ~name:(spf "hashtbl(%d)" n)
    (fun () ->
      let m = H.of_seq Sequence.(1 -- n |> map (fun x-> x,x)) in
      m
    )

let test_intmap n =
  let module M = CCIntMap in
  do_test ~name:(spf "intmap(%d)" n)
    (fun () ->
      let m = M.of_seq Sequence.(1 -- n |> map (fun x-> x,x)) in
      m
    )

let tests_ =
  [ "hashtrie", test_hashtrie
  ; "map", test_map
  ; "hamt", test_hamt
  ; "wbt", test_wbt
  ; "hashtbl", test_hashtbl
  ; "intmap", test_intmap
  ]

let run_test ~n name = List.assoc name tests_ n

let print_list () =
  Format.printf "@[<v2>tests:@ %a@]@."
    (CCList.print CCString.print) (List.map fst tests_)

let () =
  let to_test = ref [] in
  let n = ref 1_000_000 in
  let options = Arg.align
    [ "-n", Arg.Set_int n, " size of the collection"
  ] in
  Arg.parse options (CCList.Ref.push to_test) "usage: mem_measure [name*]";
  match !to_test with
    | [] ->
        print_list ();
        exit 0
    | _ -> List.iter (run_test ~n:!n) (List.rev !to_test)
