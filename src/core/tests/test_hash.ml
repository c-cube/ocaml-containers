(* test hash functions a bit *)

module H64 = CCHash64
module XXH = Containers_xxhash

let n = ref 1_000_000
let verbose = ref false

let check_bit_proba name hash_fn n_samples =
  let bits = Array.make 64 0 in
  for i = 1 to n_samples do
    let h = hash_fn i in
    for b = 0 to 63 do
      if Int64.(logand h (shift_left 1L b)) <> 0L then bits.(b) <- bits.(b) + 1
    done
  done;
  if !verbose then (
    Format.printf "%s bit probabilities after %d samples:@." name n_samples;
    for b = 0 to 63 do
      let prob = float bits.(b) /. float n_samples in
      Format.printf "bit %2d: %.4f@." b prob
    done
  );
  let ok = ref true in
  for b = 0 to 63 do
    let prob = float bits.(b) /. float n_samples in
    if prob < 0.48 || prob > 0.52 then (
      Format.printf "FAIL: bit %d has proba %.4f (outside 0.48-0.52)@." b prob;
      ok := false
    )
  done;
  if !ok then
    Format.printf "%s: OK@." name
  else
    ();
  !ok

let speclist =
  [
    "-v", Arg.Set verbose, " verbose mode";
    "-n", Arg.Set_int n, " size of the range";
  ]

let () =
  Arg.parse (Arg.align speclist) (fun _ -> ()) "test_hash.exe";
  let ok1 =
    check_bit_proba "CCHash64" (fun i -> H64.finalize64 (H64.int H64.seed i)) !n
  in
  let ok2 = check_bit_proba "XXH" (fun i -> XXH.hash_int i) !n in
  if (not ok1) || not ok2 then exit 1
