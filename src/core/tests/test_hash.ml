

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
  let add_n self x n =
    Hashtbl.replace self.tbl x (n + try Hashtbl.find self.tbl x with _ ->0);
    self.n_samples <- n + self.n_samples

  let pp out (self:t) : unit =
    let max = Hashtbl.fold (fun k _ n -> max k n) self.tbl 0 in
    let min = Hashtbl.fold (fun k _ n -> min k n) self.tbl max in
    for i=min to max do
      let n = try Hashtbl.find self.tbl i with _ -> 0 in
      Format.fprintf out "[v=%-4d, n-inputs %-6d] %s@." i n
        (String.make (int_of_float @@ ceil (log (float n))) '#');
    done
end

let reset_line = "\x1b[2K\r"

let t_int n1 n2 =
  Printf.printf "test hash_int on %d--%d\n" n1 n2;
  let count = Hashtbl.create 128 in
  for i=n1 to n2 do
    Printf.printf "%shash %d…%!" reset_line i;
    let h = H.int i in
    Hashtbl.replace count h (1 + CCHashtbl.get_or count h ~default:0);
    if i mod 1024*1024*1024 = 0 then Gc.major();
  done;
  Printf.printf "%s%!" reset_line;
  (* reverse table *)
  let by_count =
    CCHashtbl.to_iter count
    |> Iter.map (fun (_h,n) -> n)
    |> Iter.count ~hash:H.int
  in
  let hist = Hist.create() in
  by_count (fun (n,i) -> Hist.add_n hist n i);
  Format.printf "histogram:@.%a@." Hist.pp hist;
  (*assert (Hist.check_uniform hist);*)
  ()

let () =
  t_int 0 2_000_000;
  t_int (-4_000_000) (-3_500_000);
  ()
