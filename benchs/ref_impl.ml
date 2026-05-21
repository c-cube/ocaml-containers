(* reference implementations for some structures, for comparison purpose *)

module PersistentHashtbl (H : Hashtbl.HashedType) = struct
  module Table = Hashtbl.Make (H)
  (** Imperative hashtable *)

  type key = H.t

  type 'a t = 'a zipper ref

  and 'a zipper =
    | Table of 'a Table.t  (** Concrete table *)
    | Add of key * 'a * 'a t  (** Add key *)
    | Replace of key * 'a * 'a t  (** Replace key by value *)
    | Remove of key * 'a t  (** As the table, but without given key *)

  let create i = ref (Table (Table.create i))
  let empty () = create 11

  (* pass continuation to get a tailrec rerooting *)
  let rec _reroot t k =
    match !t with
    | Table tbl -> k tbl (* done *)
    | Add (key, v, t') ->
      _reroot t' (fun tbl ->
          t' := Remove (key, t);
          Table.add tbl key v;
          t := Table tbl;
          k tbl)
    | Replace (key, v, t') ->
      _reroot t' (fun tbl ->
          let v' = Table.find tbl key in
          t' := Replace (key, v', t);
          t := Table tbl;
          Table.replace tbl key v;
          k tbl)
    | Remove (key, t') ->
      _reroot t' (fun tbl ->
          let v = Table.find tbl key in
          t' := Add (key, v, t);
          t := Table tbl;
          Table.remove tbl key;
          k tbl)

  (* Reroot: modify the zipper so that the current node is a proper
     hashtable, and return the hashtable *)
  let reroot t =
    match !t with
    | Table tbl -> tbl
    | _ -> _reroot t (fun x -> x)

  let is_empty t = Table.length (reroot t) = 0
  let find t k = Table.find (reroot t) k

  (*$R
    let h = H.of_seq my_seq in
    OUnit.assert_equal "a" (H.find h 1);
    OUnit.assert_raises Not_found (fun () -> H.find h 5);
    let h' = H.replace h 5 "e" in
    OUnit.assert_equal "a" (H.find h' 1);
    OUnit.assert_equal "e" (H.find h' 5);
    OUnit.assert_equal "a" (H.find h 1);
    OUnit.assert_raises Not_found (fun () -> H.find h 5);
  *)

  (*$R
    let n = 10000 in
    let seq = Sequence.map (fun i -> i, string_of_int i) Sequence.(0--n) in
    let h = H.of_seq seq in
    Sequence.iter
      (fun (k,v) ->
        OUnit.assert_equal ~printer:(fun x -> x) v (H.find h k))
      seq;
    OUnit.assert_raises Not_found (fun () -> H.find h (n+1));
  *)

  (*$QR
    _list_int_int
      (fun l ->
        let h = H.of_list l in
        List.for_all
          (fun (k,v) ->
            try
              H.find h k = v
            with Not_found -> false)
          l
      )
  *)

  let get_exn k t = find t k
  let get k t = try Some (find t k) with Not_found -> None
  let mem t k = Table.mem (reroot t) k
  let length t = Table.length (reroot t)

  (*$R
    let h = H.of_seq
      Sequence.(map (fun i -> i, string_of_int i)
        (0 -- 200)) in
    OUnit.assert_equal 201 (H.length h);
  *)

  (*$QR
    _list_int_int (fun l ->
      let h = H.of_list l in
      H.length h = List.length l
    )
  *)

  let replace t k v =
    let tbl = reroot t in
    (* create the new hashtable *)
    let t' = ref (Table tbl) in
    (* update [t] to point to the new hashtable *)
    (try
       let v' = Table.find tbl k in
       t := Replace (k, v', t')
     with Not_found -> t := Remove (k, t'));
    (* modify the underlying hashtable *)
    Table.replace tbl k v;
    t'

  let remove t k =
    let tbl = reroot t in
    try
      let v' = Table.find tbl k in
      (* value present, make a new hashtable without this value *)
      let t' = ref (Table tbl) in
      t := Add (k, v', t');
      Table.remove tbl k;
      t'
    with Not_found ->
      (* not member, nothing to do *)
      t

  (*$R
    let h = H.of_seq my_seq in
    OUnit.assert_equal (H.find h 2) "b";
    OUnit.assert_equal (H.find h 3) "c";
    OUnit.assert_equal (H.find h 4) "d";
    OUnit.assert_equal (H.length h) 4;
    let h = H.remove h 2 in
    OUnit.assert_equal (H.find h 3) "c";
    OUnit.assert_equal (H.length h) 3;
    OUnit.assert_raises Not_found (fun () -> H.find h 2)
  *)

  (*$R
    let open Sequence.Infix in
    let n = 10000 in
    let seq = Sequence.map (fun i -> i, string_of_int i) (0 -- n) in
    let h = H.of_seq seq in
    OUnit.assert_equal (n+1) (H.length h);
    let h = Sequence.fold (fun h i -> H.remove h i) h (0 -- 500) in
    OUnit.assert_equal (n-500) (H.length h);
    OUnit.assert_bool "is_empty" (H.is_empty (H.create 16));
  *)

  (*$QR
    _list_int_int (fun l ->
      let h = H.of_list l in
      let h = List.fold_left (fun h (k,_) -> H.remove h k) h l in
      H.is_empty h)
  *)

  let update t k f =
    let v = get k t in
    match v, f v with
    | None, None -> t (* no change *)
    | Some _, None -> remove t k
    | _, Some v' -> replace t k v'

  let copy t =
    let tbl = reroot t in
    (* no one will point to the new [t] *)
    let t = ref (Table (Table.copy tbl)) in
    t

  let iter t f =
    let tbl = reroot t in
    Table.iter f tbl

  let fold f acc t =
    let tbl = reroot t in
    Table.fold (fun k v acc -> f acc k v) tbl acc

  let map f t =
    let tbl = reroot t in
    let res = Table.create (Table.length tbl) in
    Table.iter (fun k v -> Table.replace res k (f k v)) tbl;
    ref (Table res)

  let filter p t =
    let tbl = reroot t in
    let res = Table.create (Table.length tbl) in
    Table.iter (fun k v -> if p k v then Table.replace res k v) tbl;
    ref (Table res)

  let filter_map f t =
    let tbl = reroot t in
    let res = Table.create (Table.length tbl) in
    Table.iter
      (fun k v ->
        match f k v with
        | None -> ()
        | Some v' -> Table.replace res k v')
      tbl;
    ref (Table res)

  exception ExitPTbl

  let for_all p t =
    try
      iter t (fun k v -> if not (p k v) then raise ExitPTbl);
      true
    with ExitPTbl -> false

  let exists p t =
    try
      iter t (fun k v -> if p k v then raise ExitPTbl);
      false
    with ExitPTbl -> true

  let merge f t1 t2 =
    let tbl = Table.create (max (length t1) (length t2)) in
    iter t1 (fun k v1 ->
        let v2 = try Some (find t2 k) with Not_found -> None in
        match f k (Some v1) v2 with
        | None -> ()
        | Some v' -> Table.replace tbl k v');
    iter t2 (fun k v2 ->
        if not (mem t1 k) then (
          match f k None (Some v2) with
          | None -> ()
          | Some _ -> Table.replace tbl k v2
        ));
    ref (Table tbl)

  (*$R
    let t1 = H.of_list [1, "a"; 2, "b1"] in
    let t2 = H.of_list [2, "b2"; 3, "c"] in
    let t = H.merge
      (fun _ v1 v2 -> match v1, v2 with
        | None, _ -> v2
        | _ , None -> v1
        | Some s1, Some s2 -> if s1 < s2 then Some s1 else Some s2)
      t1 t2
    in
    OUnit.assert_equal ~printer:string_of_int 3 (H.length t);
    OUnit.assert_equal "a" (H.find t 1);
    OUnit.assert_equal "b1" (H.find t 2);
    OUnit.assert_equal "c" (H.find t 3);
  *)

  let add_seq init seq =
    let tbl = ref init in
    seq (fun (k, v) -> tbl := replace !tbl k v);
    !tbl

  let of_seq seq = add_seq (empty ()) seq
  let add_list init l = add_seq init (fun k -> List.iter k l)

  (*$QR
    _list_int_int (fun l ->
      let l1, l2 = List.partition (fun (x,_) -> x mod 2 = 0) l in
      let h1 = H.of_list l1 in
      let h2 = H.add_list h1 l2 in
      List.for_all
        (fun (k,v) -> H.find h2 k = v)
        l
      &&
      List.for_all
        (fun (k,v) -> H.find h1 k = v)
        l1
      &&
      List.length l1 = H.length h1
      &&
      List.length l = H.length h2
      )
  *)

  let of_list l = add_list (empty ()) l

  let to_list t =
    let tbl = reroot t in
    let bindings = Table.fold (fun k v acc -> (k, v) :: acc) tbl [] in
    bindings

  (*$R
    let h = H.of_seq my_seq in
    let l = Sequence.to_list (H.to_seq h) in
    OUnit.assert_equal my_list (List.sort compare l)
  *)

  let to_seq t k =
    let tbl = reroot t in
    Table.iter (fun x y -> k (x, y)) tbl

  (*$R
    let h = H.of_seq my_seq in
    OUnit.assert_equal "b" (H.find h 2);
    OUnit.assert_equal "a" (H.find h 1);
    OUnit.assert_raises Not_found (fun () -> H.find h 42);
  *)

  let equal eq t1 t2 =
    length t1 = length t2
    && for_all
         (fun k v ->
           match get k t2 with
           | None -> false
           | Some v' -> eq v v')
         t1

  let pp pp_k pp_v buf t =
    Buffer.add_string buf "{";
    let first = ref true in
    iter t (fun k v ->
        if !first then
          first := false
        else
          Buffer.add_string buf ", ";
        Printf.bprintf buf "%a -> %a" pp_k k pp_v v);
    Buffer.add_string buf "}"

  let print pp_k pp_v fmt t =
    Format.pp_print_string fmt "{";
    let first = ref true in
    iter t (fun k v ->
        if !first then
          first := false
        else (
          Format.pp_print_string fmt ", ";
          Format.pp_print_cut fmt ()
        );
        Format.fprintf fmt "%a -> %a" pp_k k pp_v v);
    Format.pp_print_string fmt "}"
end

module HashTrie (Key : sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end) =
struct
  (** Simple reference implementation using association lists, used for benchmarking
      as a baseline *)

  type key = Key.t
  type 'a t = (key * 'a) list

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let singleton k v = [ k, v ]

  let rec find_key k = function
    | [] -> None
    | (k', v) :: tl ->
      if Key.equal k k' then
        Some v
      else
        find_key k tl

  let add k v m =
    match find_key k m with
    | Some _ ->
      (* replace *)
      let rec replace = function
        | [] -> []
        | (k', v') :: tl ->
          if Key.equal k k' then
            (k, v) :: tl
          else
            (k', v') :: replace tl
      in
      replace m
    | None -> (k, v) :: m

  let get k m = find_key k m

  let get_exn k m =
    match find_key k m with
    | Some v -> v
    | None -> raise Not_found

  let mem k m = Option.is_some (find_key k m)

  let remove k m =
    let rec remove_acc acc = function
      | [] -> List.rev acc
      | (k', v) :: tl ->
        if Key.equal k k' then
          List.rev acc @ tl
        else
          remove_acc ((k', v) :: acc) tl
    in
    remove_acc [] m

  let update k ~f m =
    match find_key k m, f (find_key k m) with
    | None, None -> m
    | Some _, None -> remove k m
    | _, Some v -> add k v m

  let cardinal = List.length
  let iter ~f m = List.iter (fun (k, v) -> f k v) m
  let fold ~f ~x:acc m = List.fold_left (fun acc (k, v) -> f acc k v) acc m
  let to_list = Fun.id
  let add_list m l = List.fold_left (fun acc (k, v) -> add k v acc) m l
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l

  let add_iter m seq =
    let acc = ref m in
    seq (fun (k, v) -> acc := add k v !acc);
    !acc

  let of_iter seq = add_iter empty seq
  let to_iter m yield = List.iter (fun kv -> yield kv) m

  let add_gen m g =
    let rec aux acc =
      match g () with
      | None -> acc
      | Some (k, v) -> aux (add k v acc)
    in
    aux m

  let of_gen g = add_gen empty g

  let to_gen m =
    let state = ref m in
    let rec gen () =
      match !state with
      | [] -> None
      | (k, v) :: tl ->
        state := tl;
        Some (k, v)
    in
    gen

  let choose = function
    | [] -> None
    | (k, v) :: _ -> Some (k, v)

  let choose_exn = function
    | [] -> raise Not_found
    | (k, v) :: _ -> k, v

  let pp ppk ppv out m =
    let first = ref true in
    List.iter
      (fun (k, v) ->
        if !first then
          first := false
        else
          Format.fprintf out ";@ ";
        ppk out k;
        Format.pp_print_string out " -> ";
        ppv out v)
      m
end
