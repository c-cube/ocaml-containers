
let _sum g =
  Gen.Restart.fold (+) 0 g


module MList = struct
  type 'a t = 'a node option ref
  and 'a node = {
    content : 'a;
    mutable prev : 'a node;
    mutable next : 'a node;
  }

  let create () = ref None

  let is_empty d =
    match !d with
    | None -> true
    | Some _ -> false

  let push_back d x =
    match !d with
    | None ->
      let rec elt = {
        content = x; prev = elt; next = elt; } in
      d := Some elt
    | Some first ->
      let elt = { content = x; next=first; prev=first.prev; } in
      first.prev.next <- elt;
      first.prev <- elt

  (* conversion to gen *)
  let to_gen d =
    fun () ->
      match !d with
      | None -> (fun () -> None)
      | Some first ->
        let cur = ref first in (* current element of the list *)
        let stop = ref false in (* are we done yet? *)
        fun () ->
          if !stop then None
          else begin
            let x = (!cur).content in
            cur := (!cur).next;
            (if !cur == first then stop := true); (* EOG, we made a full cycle *)
            Some x
          end
end

(** Store content of the generator in an enum *)
let persistent_mlist gen =
  let l = MList.create () in
  Gen.iter (MList.push_back l) gen;
  MList.to_gen l

let bench_mlist n =
  for _i = 0 to 100 do
    let g = persistent_mlist Gen.(1 -- n) in
    ignore (_sum g)
  done

(** {6 Unrolled mutable list} *)
module UnrolledList = struct
  type 'a node =
    | Nil
    | Partial of 'a array * int
    | Cons of 'a array * 'a node ref

  let of_gen gen =
    let start = ref Nil in
    let chunk_size = ref 16 in
    let rec fill prev cur =
      match cur, gen() with
      | Partial (a,n), None ->
          prev := Cons (Array.sub a 0 n, ref Nil); ()  (* done *)
      | _, None -> prev := cur; ()  (* done *)
      | Nil, Some x ->
          let n = !chunk_size in
          if n < 4096 then chunk_size := 2 * !chunk_size;
          fill prev (Partial (Array.make n x, 1))
      | Partial (a, n), Some x ->
          assert (n < Array.length a);
          a.(n) <- x;
          if n+1 = Array.length a
          then begin
            let r = ref Nil in
            prev := Cons(a, r);
            fill r Nil
          end else fill prev (Partial (a, n+1))
      | Cons _, _ -> assert false
    in
    fill start !start ;
    !start

  let to_gen l () =
    let cur = ref l in
    let i = ref 0 in
    let rec next() = match !cur with
    | Nil -> None
    | Cons (a,l') ->
        if !i = Array.length a
        then begin
          cur := !l';
          i := 0;
          next()
        end else begin
          let y = a.(!i) in
          incr i;
          Some y
        end
    | Partial _ -> assert false
    in
    next
end

(** Store content of the generator in an enum *)
let persistent_unrolled gen =
  let l = UnrolledList.of_gen gen in
  UnrolledList.to_gen l

let bench_unrolled n =
  for _i = 0 to 100 do
    let g = persistent_unrolled Gen.(1 -- n) in
    ignore (_sum g)
  done

let bench_naive n =
  for _i = 0 to 100 do
    let l = Gen.to_rev_list Gen.(1 -- n) in
    let g = Gen.Restart.of_list (List.rev l) in
    ignore (_sum g)
  done

let bench_current n =
  for _i = 0 to 100 do
    let g = Gen.persistent Gen.(1 -- n) in
    ignore (_sum g)
  done

let bench_current_lazy n =
  for _i = 0 to 100 do
    let g = Gen.persistent_lazy Gen.(1 -- n) in
    ignore (_sum g)
  done

let () =
  let bench_n n = 
    Printf.printf "BENCH for %d\n" n;
    let res = Benchmark.throughputN 5
      [ "mlist", bench_mlist, n
      ; "naive", bench_naive, n
      ; "unrolled", bench_unrolled, n
      ; "current", bench_current, n
      ; "current_lazy", bench_current_lazy, n
      ]
    in Benchmark.tabulate res
  in
  bench_n 100;
  bench_n 100_000;
  ()

