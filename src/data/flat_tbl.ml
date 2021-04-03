
(* Another attempt at making a fast, flat Hash table.

   https://www.sebastiansylvan.com/post/robin-hood-hashing-should-be-your-default-hash-table-implementation/

   deletion:
   https://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
   *)

type 'a iter = ('a -> unit) -> unit

module type S = sig
  type key

  type 'a t

  val create : int -> 'a t
  (** Create a hashtable. *)

  val copy : 'a t -> 'a t

  val clear : 'a t -> unit
  (** Clear the content of the hashtable *)

  val find : 'a t -> key -> 'a
  (** Find the value for this key, or
      @raise Not_found if not present *)

  val find_opt : 'a t -> key -> 'a option
  (** Find the value for this key *)

  val replace : 'a t -> key -> 'a -> unit
  (** Add/replace the binding for this key. O(1) amortized. *)

  val remove : 'a t -> key -> unit
  (** Remove the binding for this key, if any *)

  val length : 'a t -> int
  (** Number of bindings in the table *)

  val mem : 'a t -> key -> bool
  (** Is the key present in the hashtable? *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  (** Iterate on bindings *)

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** Fold on bindings *)

  val to_iter : 'a t -> (key * 'a) iter

  val add_iter : 'a t -> (key * 'a) iter -> unit

  val of_iter : (key * 'a) iter -> 'a t

  val to_list : 'a t -> (key * 'a) list

  val add_list : 'a t -> (key * 'a) list -> unit

  val of_list : (key * 'a) list -> 'a t

  val stats : 'a t -> int * int * int * int * int * int
  (** Cf Weak.S *)
end

module Make(H : Hashtbl.HashedType) = struct
  type key = H.t


  (* we cannot flatten further than that, so we'll just pay for the
     additional pointer anyway. *)
  type 'a slot =
    | Empty
    | Used of {
        k: key;
        mutable v: 'a;
        hash: int;
        mutable dib: int; (* DIB: distance to initial bucket *)
      }

  let max_load = 0.92

  type 'a t = {
    mutable slots: 'a slot array; (* slot for index [i] *)
    mutable size : int;
    mutable max_dib : int;
  }

  let create size : _ t =
    let size = max 8 size in
    { slots = Array.make size Empty;
      size = 0;
      max_dib = 0;
    }

  let copy self =
    { slots = Array.copy self.slots;
      size = self.size;
      max_dib = self.max_dib;
    }

  (** clear the table, by resetting all states to Empty *)
  let clear self =
    let {slots; max_dib=_; size=_} = self in
    Array.fill slots 0 (Array.length slots) Empty;
    self.max_dib <- 0;
    self.size <- 0

  (* Index of slot, for i-th probing starting from hash [h] in
     a table of length [n].
     Note: we make sure the [h+dist] part is positive first,
     and we do not use [abs] since it can be negative on [min_int]. *)
  let[@inline] addr_ h n dist =
    ((h + dist) land max_int) mod n

  (* Insert [k -> v] in [self], starting with the hash [h].
     Does not modify the size. *)
  let insert_ (self:_ t) h k v : unit =
    let {slots; max_dib=_; size=_} = self in
    let n = Array.length slots in

    (* lookup an empty slot to insert the key->value in. *)
    let rec insert_rec_ h k v dib =
      let j = addr_ h n dib in

      match Array.unsafe_get slots j with
      | Empty ->
        Array.unsafe_set slots j (Used {k; v; hash=h; dib});
        self.max_dib <- max dib self.max_dib
      | Used used_j ->
        if used_j.hash = h && H.equal k used_j.k then (
          (* same key: replace *)
          used_j.v <- v;
          used_j.dib <- dib;
          self.max_dib <- max dib self.max_dib;

        ) else if used_j.dib < dib then (
          (* displace element*)
          let k_j = used_j.k in
          let v_j = used_j.v in
          let h_j = used_j.hash in
          let dib_j = used_j.dib in

          Array.unsafe_set slots j (Used {k;v;hash=h;dib});
          self.max_dib <- max dib self.max_dib;

          insert_rec_ h_j k_j v_j dib_j
        ) else (
          (* look further *)
          insert_rec_ h k v (dib+1)
        )
    in

    insert_rec_ h k v 0

  (* Resize the array, by inserting its content into twice as large an array *)
  let resize (self:_ t) : unit =
    let {slots=old_slots; max_dib=_; size=_} = self in

    let new_size =
      let n = Array.length old_slots in
      let n = n + n lsr 2 in (* ×1.5 *)
      min n Sys.max_array_length
    in
    if new_size <= Array.length old_slots then failwith "flat_tbl: cannot resize further";

    self.slots <- Array.make new_size Empty;
    self.max_dib <- 0;

    (* insert elements into new table *)
    Array.iter
      (fun slot -> match slot with
         | Empty -> ()
         | Used {k; v; hash; _} ->
           insert_ self hash k v)
      old_slots;
    ()

  (* Lookup [key] in the table *)
  let find_opt self k =
    let {slots; max_dib; size=_} = self in
    let n = Array.length slots in
    let h = H.hash k in

    let rec find_rec_ dib =
      (*assert (dist < n); (* load factor would be 1 *)*)
      let j = addr_ h n dib in
      match Array.unsafe_get slots j with
      | Empty -> None
      | Used used_j ->
        if used_j.hash = h && H.equal used_j.k k then (
          Some used_j.v (* found *)
        ) else if dib >= max_dib then (
          None (* no need to go further *)
        ) else (

          (* unroll by hand *)
          let dib = dib+1 in

          let j = addr_ h n dib in
          match Array.unsafe_get slots j with
          | Empty -> None
          | Used used_j ->
            if used_j.hash = h && H.equal used_j.k k then (
              Some used_j.v (* found *)
            ) else if dib >= max_dib then (
              None (* no need to go further *)
            ) else (
            find_rec_ (dib+1)
            )
        )
    in

    (* try a direct hit first *)
    begin match Array.unsafe_get slots (addr_ h n 0) with
      | Empty -> None
      | Used {k=k2; v; hash; _} when h = hash && H.equal k k2 -> Some v
      | _ -> find_rec_ 1
    end

  let find self k =
    match find_opt self k with
      | Some x -> x
      | None -> raise Not_found

  (** put [key] -> [value] in the hashtable *)
  let replace self k v : unit =
    (* need to resize? *)
    let load = float_of_int self.size /. float_of_int (Array.length self.slots) in
    if load > max_load then (
      resize self;
    );

    let h = H.hash k in
    self.size <- 1 + self.size;
    insert_ self h k v

  (* Remove the key from the table. We use backward shift deletion
     (see https://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/ )
     to keep probe_distance low, instead of using tombstones. *)
  let remove self k : unit =
    let {slots; max_dib=_; size=_} = self in
    let n = Array.length slots in
    let h = H.hash k in

    (* given that [i] is empty, and [i_succ = (i+1) mod n],
       see if we can shift the element at [i_succ] to the left
       to decrease its probe count. *)
    let rec backward_shift_ i i_succ : unit =
      match Array.unsafe_get slots i_succ with
        | Empty -> ()

        | Used used as slot when used.dib > 0 ->
          (* shift to the left, decreasing DIB by one *)
          Array.unsafe_set slots i slot;
          used.dib <- used.dib - 1;
          Array.unsafe_set slots i_succ Empty;

          backward_shift_ i_succ ((i_succ + 1) mod n)

        | Used _ -> ()
    in

    let rec find_rec_ dib =
      assert (dib<n);

      let j = addr_ h n dib in
      begin match Array.unsafe_get slots j with
        | Empty -> ()
        | _ when dib > self.max_dib -> ()
        | Used used_j ->
          if h = used_j.hash && H.equal k used_j.k then (
            (* found element, remove it *)
            Array.unsafe_set slots j Empty;
            self.size <- self.size - 1;

            backward_shift_ j ((j+1) mod n); (* shift slots that come just next *)

          ) else (
            find_rec_ (dib+1)
          )
      end
    in

    if self.size > 0 then (
      find_rec_ 0
    )

  (* size of the table *)
  let[@inline] length t = t.size

  (* Is the key member of the table? *)
  let mem self k =
    match find_opt self k with
      | Some _ -> true
      | None -> false

  (* Iterate on key -> value pairs *)
  let iter f self =
    Array.iter
      (function
        | Used {k; v; _} -> f k v
        | Empty -> ())
      self.slots

  (* Fold on key -> value pairs *)
  let fold f self acc =
    Array.fold_left
      (fun acc sl -> match sl with
         | Empty -> acc
         | Used {k;v;_} -> f k v acc)
      acc self.slots

  let to_iter t yield =
    iter (fun k v -> yield (k, v)) t

  let add_iter t seq =
    seq (fun (k,v) -> replace t k v)

  let of_iter seq =
    let self = create 32 in
    add_iter self seq;
    self

  let to_list self =
    if length self > 0 then (
      fold (fun k v l -> (k,v)::l) self []
    ) else []

  let add_list self l =
    List.iter (fun (k,v) -> replace self k v) l

  let of_list l =
    let self = create 32 in
    add_list self l;
    self

  (* Statistics on the table *)
  let stats t = (Array.length t.slots, t.size, t.size, 0, 0, 1)
end

(*$inject
  module T = Flat_tbl.Make(CCInt)
  let sort l = List.sort compare l

  let ppt_bool out tbl = CCFormat.(Dump.(list @@ pair int bool)) out (T.to_list tbl)
*)

(*$= & ~cmp:(fun a b -> sort a=sort b) ~printer:Q.Print.(list (pair int bool))
  [] T.(to_list @@ of_list [])
  [1,true; 2,false; 3,true] T.(to_list@@ of_list [2,false;3,true;1,true])
*)

(*$T
  (let tbl=T.create 32 in T.replace tbl 1 true; T.replace tbl 3 false; T.find tbl 1)
  (not (let tbl=T.create 32 in T.replace tbl 1 true; T.replace tbl 3 false; T.find tbl 3))
  (try ignore(let tbl=T.create 32 in T.replace tbl 1 true; T.replace tbl 3 false; T.find tbl 4); false \
   with Not_found -> true)
*)

(*$R
  let tbl = T.create 32 in
  T.replace tbl (-50) false;
  T.remove tbl (-50);
  assert_equal ~printer:(Q.Print.int) 0 (T.length tbl);
  assert_equal ~printer:(Q.Print.(option bool)) None (T.find_opt tbl (-50));
  *)


(*$R
  let tbl = T.create 32 in
  T.replace tbl 7 false;
  T.replace tbl 7 true;
  assert_equal ~printer:Q.Print.(list (pair int bool)) [7, true] (T.to_list tbl);
  *)

(*$inject
  type op =
    | Insert of int * bool
    | Remove of int
    | Get of int
    | Clear

  module IntSet = CCSet.Make(CCInt)

  let genop keys : op Q.Gen.t =
    Q.Gen.(frequency @@ List.flatten [
        (if IntSet.is_empty keys then [] else [
            (3, oneofl (IntSet.to_list keys) >|= fun k->Remove k);
            (4, oneofl (IntSet.to_list keys) >|= fun k->Get k);
          ]);
        [6, map2 (fun k v -> Insert (k,v)) (-100 -- 200) bool];
        [1, return Clear];
    ])

  let genops size : _ Q.Gen.t =
    let rec loop keys l size =
      let open Q.Gen in
      if size<=0 then return l
      else (
        genop keys >>= fun op ->
        let new_keys = match op with
          | Insert(k,_) -> IntSet.add k keys
          | Remove k -> IntSet.remove k keys
          | Get _ | Clear -> keys
        in
        loop new_keys (op :: l) (size-1)
      )
    in
    loop IntSet.empty [] size

  let shrink = Q.Shrink.list

  let to_str = function
    | Insert (k,v) -> Printf.sprintf "Insert(%d,%b)" k v
    | Remove k -> Printf.sprintf "Remove(%d)" k
    | Get k -> Printf.sprintf "Get(%d)" k
    | Clear -> "clear"

  let arb_ops =
    Q.make ~shrink ~print:(Q.Print.list to_str)
      Q.Gen.((0 -- 700) >>= genops)

  module Int_tbl = CCHashtbl.Make(CCInt)

  let exec_op tbl op =
   match op with
    | Insert (k,v) -> T.replace tbl k v;
    | Remove k -> T.remove tbl k;
    | Get _k -> ()
    | Clear -> T.clear tbl
*)

(*$QR & ~count:800 ~long_factor:10
  Q.(arb_ops) (fun ops ->
    let module Fmt = CCFormat in
    let tbl = T.create 32 in
    let tbl_r = Int_tbl.create 32 in

    let check_same() =
     if sort (T.to_list tbl) <> sort (Int_tbl.to_list tbl_r) then (
       Q.Test.fail_reportf "mismatch:@ tbl=%a,@ tbl_ref=%a"
         ppt_bool tbl (Fmt.Dump.(list (pair int bool))) (Int_tbl.to_list tbl_r)
     )
    in

    List.iter
      (fun op ->
         begin match op with
          | Insert (k,v) ->
            T.replace tbl k v;
            Int_tbl.replace tbl_r k v
          | Remove k ->
            T.remove tbl k;
            Int_tbl.remove tbl_r k;
          | Get k ->
            (try
            let v = T.find tbl k in
            let v' = Int_tbl.find tbl_r k in
            if v<>v' then (
              Q.Test.fail_reportf "mismatch on %d:@ tbl=%a,@ tbl_ref=%a"
                k
                ppt_bool tbl
                (Fmt.Dump.(list (pair int bool))) (Int_tbl.to_list tbl_r)
            )
             with Not_found -> Q.assume false)
          | Clear ->
            T.clear tbl;
            Int_tbl.clear tbl_r;
         end;
         check_same())
      ops;

    check_same();
    true
  )
*)

(*$R
  let ops = [Insert(33,true); Insert(-63,false); Insert(-30,false); Remove(-63)] in
  let tbl = T.create 32 in
  List.iter (exec_op tbl) ops;
  assert_equal ~printer:Q.Print.(list (pair int bool))
    [(-30),false; 33,true] (sort (T.to_list tbl))
*)




