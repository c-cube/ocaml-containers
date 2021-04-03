

(* test hash functions a bit *)

module H = CCHash

module Hist = struct
  type t = {
    tbl: (int,int) Hashtbl.t;
    mutable n_samples: int;
  }
  let create() : t = {tbl=Hashtbl.create 32; n_samples=0}
  let add self x =
    Hashtbl.replace self.tbl x (1 + try Hashtbl.find self.tbl x with _ ->0);
    self.n_samples <- 1 + self.n_samples

  let pp out (self:t) : unit =
    let max = Hashtbl.fold (fun k _ n -> max k n) self.tbl 0 in
    let min = Hashtbl.fold (fun k _ n -> min k n) self.tbl max in
    for i=min to max do
      let n = try Hashtbl.find self.tbl i with _ -> 0 in
      Format.fprintf out "[v=%-4d, n-inputs %-6d] %s@." i n
        (String.make (int_of_float @@ ceil (log (float n))) '#');
    done
end

(* how long does it take to get back to [n0]?
   @param max if provided, stop when reaching it *)
let orbit_size ?(max=max_int) n0 : int =
  let size = ref 1 in
  let n = ref (H.int n0) in
  while !n <> n0 && !size < max do
    n := H.int !n;
    incr size;
  done;
  !size

let reset_line = "\x1b[2K\r"

let t_int n1 n2 =
  Printf.printf "test hash_int on %d--%d\n" n1 n2;
  let hist = Hist.create() in
  for i=n1 to n2 do
    Printf.printf "%sorbit for %d…%!" reset_line i;
    let orb = orbit_size ~max:30_000 i in
    Hist.add hist orb;
  done;
  Printf.printf "%s%!" reset_line;
  Format.printf "histogram:@.%a@." Hist.pp hist;
  (*assert (Hist.check_uniform hist);*)
  ()

(* how long does it take to get back to [n0] by
   hashing [n] with [m0] ?
   @param max if provided, stop when reaching it *)
let left_orbit_size ?(max=max_int) n0 m0 : int =
  let size = ref 1 in
  let n = ref (H.combine2 (H.int n0) (H.int m0)) in
  while !n <> n0 && !size < max do
    n := H.combine2 (H.int !n) (H.int m0);
    incr size;
  done;
  !size

let t_int2 n1 n2 n3 n4 =
  Printf.printf "test hash combine2 on %d--%d x %d--%d\n" n1 n2 n3 n4;
  let hist = Hist.create() in
  for i=n1 to n2 do
    for j=n3 to n4 do
      Printf.printf "%sleft-orbit for %d x %d…%!" reset_line i j;
      let orb = left_orbit_size ~max:20_000 i j in
      Hist.add hist orb;
    done;
  done;
  Printf.printf "%s%!" reset_line;
  Format.printf "histogram:@.%a@." Hist.pp hist;
  ()

let help =
{|Test the hash function on integers, by computing orbits for inputs.
An orbit is a cycle [n -> h(n) -> h(h(n)) -> h^3(n) -> … h^k(n)]
where [h^k(n) == n]. For a good hash functions, orbits should be as long as
possible.
|}

let () =
  print_endline help;
  t_int 0 80_000;
  t_int 3_000_000 3_040_000;
  t_int (-40_000) (-17_000);
  t_int2 10 300 600 800;
  ()
