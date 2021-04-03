
open CCRandom

let uniformity_test ?(size_hint=10) k rng st =
  let histogram = Hashtbl.create size_hint in
  let add x = let n = try Hashtbl.find histogram x with Not_found -> 0 in
    Hashtbl.replace histogram x (n + 1) in
  let () =
    for _i = 0 to ( k - 1 ) do
      add (rng st)
    done in
  let cardinal = float_of_int (Hashtbl.length histogram) in
  let kf = float_of_int k in
  (* average number of points assuming an uniform distribution *)
  let average = kf /. cardinal in
  (* The number of points is a sum of random variables with binomial distribution *)
  let p = 1. /. cardinal in
  (* The variance of a binomial distribution with average p is *)
  let variance = p *. (1. -. p ) in
  (* Central limit theorem: a confidence interval of 4σ provides a false positive rate
     of 0.00634% *)
  let confidence = 4. in
  let std = confidence *. (sqrt (kf *. variance)) in
  let predicate _key n acc =
    let (<) (a : float) b = CCShims_.Stdlib.(<) a b in
    acc && abs_float (average -. float_of_int n) < std in
  Hashtbl.fold predicate histogram true

let () =
  let st = Random.State.make_self_init() in
  let ok = run ~st ( uniformity_test 50_000 (split_list 10 ~len:3) ) in
  if not ok then failwith "uniformity check failed"
