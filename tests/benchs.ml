
(** Benchmarking *)

module IHashtbl = Hashtbl.Make(struct
  type t = int
  let equal i j = i - j = 0
  let hash i = i
end)

let phashtbl_add n =
  let h = PHashtbl.create 50 in
  for i = n downto 0 do
    PHashtbl.add h i i;
  done;
  h

let hashtbl_add n =
  let h = Hashtbl.create 50 in
  for i = n downto 0 do
    Hashtbl.add h i i;
  done;
  h

let ihashtbl_add n =
  let h = IHashtbl.create 50 in
  for i = n downto 0 do
    IHashtbl.add h i i;
  done;
  h

let _ =
  Format.printf "----------------------------------------@.";
  let res = Bench.bench_n
    ["phashtbl_add", (fun n -> ignore (phashtbl_add n));
     "hashtbl_add", (fun n -> ignore (hashtbl_add n));
     "ihashtbl_add", (fun n -> ignore (ihashtbl_add n));]
  in
  Bench.summarize 1. res

let phashtbl_replace n =
  let h = PHashtbl.create 50 in
  for i = 0 to n do
    PHashtbl.replace h i i;
  done;
  for i = n downto 0 do
    PHashtbl.replace h i i;
  done;
  h

let hashtbl_replace n =
  let h = Hashtbl.create 50 in
  for i = 0 to n do
    Hashtbl.replace h i i;
  done;
  for i = n downto 0 do
    Hashtbl.replace h i i;
  done;
  h

let ihashtbl_replace n =
  let h = IHashtbl.create 50 in
  for i = 0 to n do
    IHashtbl.replace h i i;
  done;
  for i = n downto 0 do
    IHashtbl.replace h i i;
  done;
  h

let _ =
  Format.printf "----------------------------------------@.";
  let res = Bench.bench_n
    ["phashtbl_replace", (fun n -> ignore (phashtbl_replace n));
     "hashtbl_replace", (fun n -> ignore (hashtbl_replace n));
     "ihashtbl_replace", (fun n -> ignore (ihashtbl_replace n));]
  in
  Bench.summarize 1. res

let my_len = 250
let round_n n = abs (n mod my_len)

let phashtbl_find h =
  fun n ->
    ignore (PHashtbl.find h (round_n n))
     
let hashtbl_find h =
  fun n ->
    ignore (Hashtbl.find h (round_n n))
     
let ihashtbl_find h =
  fun n ->
    ignore (IHashtbl.find h (round_n n))

let _ =
  Format.printf "----------------------------------------@.";
  let h = phashtbl_add my_len in
  let h' = hashtbl_add my_len in
  let h'' = ihashtbl_add my_len in
  let res = Bench.bench_n
    ["phashtbl_mem", phashtbl_find h;
     "hashtbl_mem", hashtbl_find h';
     "ihashtbl_mem", ihashtbl_find h'';]
  in
  Bench.summarize 1. res
