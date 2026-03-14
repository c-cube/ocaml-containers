open CCHash
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> int 42 >= 0;;
t @@ fun () -> int max_int >= 0;;
t @@ fun () -> int max_int = int max_int;;
t @@ fun () -> int min_int >= 0;;
t @@ fun () -> int 0 >= 0;;
t @@ fun () -> char 'c' >= 0;;
t @@ fun () -> int 152352 = int 152352;;
t @@ fun () -> list_comm int [ 1; 2 ] = list_comm int [ 2; 1 ];;
t @@ fun () -> list_comm int [ 1; 2 ] <> list_comm int [ 2; 3 ];;
t @@ fun () -> string "abcd" >= 0;;
t @@ fun () -> string "abc" <> string "abcd";;

q Q.int (fun i ->
    Q.assume (i >= 0);
    int i = int64 (Int64.of_int i));;

(* --- stress tests -------------------------------------------------------- *)

(* Chi-squared distribution test over [count] consecutive integers in [buckets] buckets.
   A uniform hash gives chi2 ~ buckets-1; we allow 4 standard deviations of slack. *)
t ~name:"int hash distribution chi2" @@ fun () ->
  let count = 50_000 and buckets = 500 in
  let counts = Array.make buckets 0 in
  for i = 0 to count - 1 do
    let b = CCHash.int i mod buckets in
    counts.(b) <- counts.(b) + 1
  done;
  let expected = float count /. float buckets in
  let c2 =
    Array.fold_left
      (fun acc c -> acc +. ((float c -. expected) ** 2.0 /. expected))
      0.0 counts
  in
  let df = float (buckets - 1) in
  c2 < df +. 4.0 *. sqrt (2.0 *. df);;

(* Strict avalanche criterion: flip one input bit, expect ~50% output bits to change. *)
t ~name:"int hash avalanche" @@ fun () ->
  let bits = Sys.int_size - 1 in
  let total_flips = ref 0 in
  let total = ref 0 in
  let rng = Random.State.make [| 42; 17; 99 |] in
  for _ = 1 to 300 do
    let x = Random.State.bits rng in
    let hx = CCHash.int x in
    for b = 0 to bits - 1 do
      let hx' = CCHash.int (x lxor (1 lsl b)) in
      total_flips := !total_flips + CCInt.popcount (hx lxor hx');
      total := !total + bits
    done
  done;
  let frac = float !total_flips /. float !total in
  frac >= 0.45 && frac <= 0.55;;

(* String hash: no collisions among distinct keys. *)
t ~name:"string hash no collisions" @@ fun () ->
  let n = 50_000 in
  let tbl = Hashtbl.create n in
  let ok = ref true in
  for i = 0 to n - 1 do
    let h = CCHash.string (Printf.sprintf "key:%d" i) in
    if Hashtbl.mem tbl h then ok := false;
    Hashtbl.replace tbl h ()
  done;
  !ok;;

(* CCHash64 pipeline matches CCHash.pair combiner. *)
q Q.int (fun i ->
    let j = i lxor 0xdeadbeef in
    let h_pair = CCHash.pair CCHash.int CCHash.int (i, j) in
    let h_manual =
      CCHash64.(finalize (int (int seed (CCHash.int i)) (CCHash.int j)))
    in
    h_pair = h_manual)
