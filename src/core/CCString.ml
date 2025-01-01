(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic String Utils} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

(* standard implementations *)

include String

let compare_int (a : int) b = Stdlib.compare a b
let compare = String.compare
let hash s = Hashtbl.hash s
let length = String.length
let is_empty s = equal s ""

let rev s =
  let n = length s in
  init n (fun i -> s.[n - i - 1])

let rec _to_list s acc i len =
  if len = 0 then
    List.rev acc
  else
    _to_list s (s.[i] :: acc) (i + 1) (len - 1)

let _is_sub ~sub i s j ~len =
  let rec check k =
    if k = len then
      true
    else
      CCChar.equal sub.[i + k] s.[j + k] && check (k + 1)
  in
  j + len <= String.length s && check 0

let is_sub ~sub i s j ~len =
  if i + len > String.length sub then invalid_arg "CCString.is_sub";
  _is_sub ~sub i s j ~len

type _ direction =
  | Direct : [ `Direct ] direction
  | Reverse : [ `Reverse ] direction

(* we follow https://en.wikipedia.org/wiki/Knuth–Morris–Pratt_algorithm *)
module Find = struct
  type 'a kmp_pattern = {
    failure: int array;
    str: string;
  }
  (* invariant: [length failure = length str].
     We use a phantom type to avoid mixing the directions. *)

  let kmp_pattern_length p = String.length p.str

  (* access the [i]-th element of [s] according to direction [dir] *)
  let get_ : type a. dir:a direction -> string -> int -> char =
   fun ~dir ->
    match dir with
    | Direct -> String.get
    | Reverse -> fun s i -> s.[String.length s - i - 1]

  let kmp_compile_ : type a. dir:a direction -> string -> a kmp_pattern =
   fun ~dir str ->
    let len = length str in
    let get = get_ ~dir in
    (* how to read elements of the string *)
    match len with
    | 0 -> { failure = [||]; str }
    | 1 -> { failure = [| -1 |]; str }
    | _ ->
      (* at least 2 elements, the algorithm can work *)
      let failure = Array.make len 0 in
      failure.(0) <- -1;
      (* i: current index in str *)
      let i = ref 2 in
      (* j: index of candidate substring *)
      let j = ref 0 in
      while !i < len do
        match !j with
        | _ when CCChar.equal (get str (!i - 1)) (get str !j) ->
          (* substring starting at !j continues matching current char *)
          incr j;
          failure.(!i) <- !j;
          incr i
        | 0 ->
          (* back to the beginning *)
          failure.(!i) <- 0;
          incr i
        | _ ->
          (* fallback for the prefix string *)
          assert (!j > 0);
          j := failure.(!j)
      done;
      (* Format.printf "{@[failure:%a, str:%s@]}@." CCFormat.(array int) failure str; *)
      { failure; str }

  let kmp_compile s = kmp_compile_ ~dir:Direct s
  let kmp_rcompile s = kmp_compile_ ~dir:Reverse s

  (* proper search function.
     [i] index in [s]
     [j] index in [pattern]
     [len] length of [s] *)
  let kmp_find ~pattern s idx =
    let len = length s in
    let i = ref idx in
    let j = ref 0 in
    let pat_len = kmp_pattern_length pattern in
    while !j < pat_len && !i + !j < len do
      let c = String.get s (!i + !j) in
      let expected = String.get pattern.str !j in
      if CCChar.equal c expected then
        (* char matches *)
        incr j
      else (
        let fail_offset = pattern.failure.(!j) in
        if fail_offset >= 0 then (
          assert (fail_offset < !j);
          (* follow the failure link *)
          i := !i + !j - fail_offset;
          j := fail_offset
        ) else (
          (* beginning of pattern *)
          j := 0;
          incr i
        )
      )
    done;
    if !j = pat_len then
      !i
    else
      -1

  (* proper search function, from the right.
     [i] index in [s]
     [j] index in [pattern]
     [len] length of [s] *)
  let kmp_rfind ~pattern s idx =
    let len = length s in
    let i = ref (len - idx - 1) in
    let j = ref 0 in
    let pat_len = kmp_pattern_length pattern in
    while !j < pat_len && !i + !j < len do
      let c = String.get s (len - !i - !j - 1) in
      let expected =
        String.get pattern.str (String.length pattern.str - !j - 1)
      in
      if CCChar.equal c expected then
        (* char matches *)
        incr j
      else (
        let fail_offset = pattern.failure.(!j) in
        if fail_offset >= 0 then (
          assert (fail_offset < !j);
          (* follow the failure link *)
          i := !i + !j - fail_offset;
          j := fail_offset
        ) else (
          (* beginning of pattern *)
          j := 0;
          incr i
        )
      )
    done;
    (* adjust result: first, [res = string.length s - res -1] to convert
       back to real indices; then, what we got is actually the position
       of the end of the pattern, so we subtract the [length of the pattern -1]
       to obtain the real result. *)
    if !j = pat_len then
      len - !i - kmp_pattern_length pattern
    else
      -1

  type 'a pattern =
    | P_char of char
    | P_KMP of 'a kmp_pattern

  let pattern_length = function
    | P_char _ -> 1
    | P_KMP p -> kmp_pattern_length p

  let compile sub : [ `Direct ] pattern =
    if length sub = 1 then
      P_char sub.[0]
    else
      P_KMP (kmp_compile sub)

  let rcompile sub : [ `Reverse ] pattern =
    if length sub = 1 then
      P_char sub.[0]
    else
      P_KMP (kmp_rcompile sub)

  let find ?(start = 0) ~(pattern : [ `Direct ] pattern) s =
    match pattern with
    | P_char c -> (try String.index_from s start c with Not_found -> -1)
    | P_KMP pattern -> kmp_find ~pattern s start

  let rfind ?start ~(pattern : [ `Reverse ] pattern) s =
    let start =
      match start with
      | Some n -> n
      | None -> String.length s - 1
    in
    match pattern with
    | P_char c -> (try String.rindex_from s start c with Not_found -> -1)
    | P_KMP pattern -> kmp_rfind ~pattern s start
end

let find ?(start = 0) ~sub =
  let pattern = Find.compile sub in
  fun s -> Find.find ~start ~pattern s

let find_all ?(start = 0) ~sub =
  let pattern = Find.compile sub in
  fun s ->
    let i = ref start in
    fun () ->
      let res = Find.find ~start:!i ~pattern s in
      if res = ~-1 then
        None
      else (
        i := res + 1;
        (* possible overlap *)
        Some res
      )

let find_all_l ?start ~sub s =
  let rec aux acc g =
    match g () with
    | None -> List.rev acc
    | Some i -> aux (i :: acc) g
  in
  aux [] (find_all ?start ~sub s)

let mem ?start ~sub s = find ?start ~sub s >= 0

let rfind ~sub =
  let pattern = Find.rcompile sub in
  fun s -> Find.rfind ~start:(String.length s - 1) ~pattern s

(* Replace substring [s.[pos] … s.[pos+len-1]] by [by] in [s] *)
let replace_at_ ~pos ~len ~by s =
  let b = Buffer.create (length s + length by - len) in
  Buffer.add_substring b s 0 pos;
  Buffer.add_string b by;
  Buffer.add_substring b s (pos + len) (String.length s - pos - len);
  Buffer.contents b

let replace ?(which = `All) ~sub ~by s =
  if is_empty sub then invalid_arg "CCString.replace";
  match which with
  | `Left ->
    let i = find ~start:0 ~sub s in
    if i >= 0 then
      replace_at_ ~pos:i ~len:(String.length sub) ~by s
    else
      s
  | `Right ->
    let i = rfind ~sub s in
    if i >= 0 then
      replace_at_ ~pos:i ~len:(String.length sub) ~by s
    else
      s
  | `All ->
    (* compile search pattern only once *)
    let pattern = Find.compile sub in
    let b = Buffer.create (String.length s) in
    let start = ref 0 in
    while !start < String.length s do
      let i = Find.find ~start:!start ~pattern s in
      if i >= 0 then (
        (* between last and cur occurrences *)
        Buffer.add_substring b s !start (i - !start);
        Buffer.add_string b by;
        start := i + String.length sub
      ) else (
        (* add remainder *)
        Buffer.add_substring b s !start (String.length s - !start);
        start := String.length s (* stop *)
      )
    done;
    Buffer.contents b

module Split = struct
  type drop_if_empty = {
    first: bool;
    last: bool;
  }

  let no_drop = { first = false; last = false }
  let default_drop = no_drop

  type split_state =
    | SplitStop
    | SplitAt of int (* previous *)

  let rec _split ~by s state =
    match state with
    | SplitStop -> None
    | SplitAt prev -> _split_search ~by s prev

  and _split_search ~by s prev =
    let j = Find.find ~start:prev ~pattern:by s in
    if j < 0 then
      Some (SplitStop, prev, String.length s - prev)
    else
      Some (SplitAt (j + Find.pattern_length by), prev, j - prev)

  let _tuple3 x y z = x, y, z

  let _mkgen ~drop ~by s k =
    let state = ref (SplitAt 0) in
    let by = Find.compile by in
    let rec next () =
      match _split ~by s !state with
      | None -> None
      | Some (state', 0, 0) when drop.first ->
        state := state';
        next ()
      | Some (_, i, 0) when drop.last && i = length s -> None
      | Some (state', i, len) ->
        state := state';
        Some (k s i len)
    in
    next

  let gen ?(drop = default_drop) ~by s = _mkgen ~drop ~by s _tuple3
  let gen_cpy ?(drop = default_drop) ~by s = _mkgen ~drop ~by s String.sub

  let _mklist ~drop ~by s k =
    let by = Find.compile by in
    let rec build acc state =
      match _split ~by s state with
      | None -> List.rev acc
      | Some (state', 0, 0) when drop.first -> build acc state'
      | Some (_, i, 0) when drop.last && i = length s -> List.rev acc
      | Some (state', i, len) -> build (k s i len :: acc) state'
    in
    build [] (SplitAt 0)

  let list_ ?(drop = default_drop) ~by s = _mklist ~drop ~by s _tuple3
  let list_cpy ?(drop = default_drop) ~by s = _mklist ~drop ~by s String.sub

  let _mkseq ~drop ~by s k =
    let by = Find.compile by in
    let rec make state () =
      match _split ~by s state with
      | None -> Seq.Nil
      | Some (state', 0, 0) when drop.first -> make state' ()
      | Some (_, i, 0) when drop.last && i = length s -> Seq.Nil
      | Some (state', i, len) -> Seq.Cons (k s i len, make state')
    in
    make (SplitAt 0)

  let seq ?(drop = default_drop) ~by s = _mkseq ~drop ~by s _tuple3
  let seq_cpy ?(drop = default_drop) ~by s = _mkseq ~drop ~by s String.sub

  let _mk_iter ~drop ~by s f k =
    let by = Find.compile by in
    let rec aux state =
      match _split ~by s state with
      | None -> ()
      | Some (state', 0, 0) when drop.first -> aux state'
      | Some (_, i, 0) when drop.last && i = length s -> ()
      | Some (state', i, len) ->
        k (f s i len);
        aux state'
    in
    aux (SplitAt 0)

  let iter ?(drop = default_drop) ~by s = _mk_iter ~drop ~by s _tuple3
  let iter_cpy ?(drop = default_drop) ~by s = _mk_iter ~drop ~by s String.sub

  let left_exn ~by s =
    let i = find ~sub:by s in
    if i = ~-1 then
      raise Not_found
    else (
      let right = i + String.length by in
      String.sub s 0 i, String.sub s right (String.length s - right)
    )

  let left ~by s = try Some (left_exn ~by s) with Not_found -> None

  let right_exn ~by s =
    let i = rfind ~sub:by s in
    if i = ~-1 then
      raise Not_found
    else (
      let right = i + String.length by in
      String.sub s 0 i, String.sub s right (String.length s - right)
    )

  let right ~by s = try Some (right_exn ~by s) with Not_found -> None
end

let split ~by s = Split.list_cpy ~by s

let compare_versions a b =
  let of_int s = try Some (int_of_string s) with Failure _ -> None in
  let rec cmp_rec a b =
    match a (), b () with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some x, Some y ->
      (match of_int x, of_int y with
      | None, None ->
        let c = String.compare x y in
        if c <> 0 then
          c
        else
          cmp_rec a b
      | Some _, None -> 1
      | None, Some _ -> -1
      | Some x, Some y ->
        let c = compare_int x y in
        if c <> 0 then
          c
        else
          cmp_rec a b)
  in
  cmp_rec (Split.gen_cpy ~by:"." a) (Split.gen_cpy ~by:"." b)

type nat_chunk =
  | NC_char of char
  | NC_int of int

let compare_natural a b =
  (* stream of chunks *)
  let chunks s : unit -> nat_chunk option =
    let i = ref 0 in
    let rec next () =
      if !i = length s then
        None
      else (
        match String.get s !i with
        | '0' .. '9' as c ->
          incr i;
          read_int (Char.code c - Char.code '0')
        | c ->
          incr i;
          Some (NC_char c)
      )
    and read_int n =
      if !i = length s then
        Some (NC_int n)
      else (
        match String.get s !i with
        | '0' .. '9' as c ->
          incr i;
          read_int ((10 * n) + Char.code c - Char.code '0')
        | _ -> Some (NC_int n)
      )
    in
    next
  in
  let rec cmp_rec a b =
    match a (), b () with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some x, Some y ->
      (match x, y with
      | NC_char x, NC_char y ->
        let c = Char.compare x y in
        if c <> 0 then
          c
        else
          cmp_rec a b
      | NC_int _, NC_char _ -> 1
      | NC_char _, NC_int _ -> -1
      | NC_int x, NC_int y ->
        let c = compare_int x y in
        if c <> 0 then
          c
        else
          cmp_rec a b)
  in
  cmp_rec (chunks a) (chunks b)

let edit_distance ?(cutoff = max_int) s1 s2 =
  let n1 = length s1 in
  let n2 = length s2 in
  if n1 = 0 then
    min cutoff n2
  else if n2 = 0 then
    min cutoff n1
  else if equal s1 s2 then
    0
  else if n1 - n2 >= cutoff || n2 - n1 >= cutoff then
    cutoff
  (* at least cutoff inserts *)
  else (
    try
      (* distance vectors (v0=previous, v1=current) *)
      let v0 = Array.make (length s2 + 1) 0 in
      let v1 = Array.make (length s2 + 1) 0 in
      (* initialize v0: v0(i) = A(0)(i) = delete i chars from t *)
      let lower_bound = ref max_int in
      for i = 0 to length s2 do
        v0.(i) <- i
      done;
      (* main loop for the bottom up dynamic algorithm *)
      for i = 0 to length s1 - 1 do
        (* first edit distance is the deletion of i+1 elements from s *)
        v1.(0) <- i + 1;

        (* try add/delete/replace operations *)
        for j = 0 to length s2 - 1 do
          let cost =
            if Char.equal (String.get s1 i) (String.get s2 j) then
              0
            else
              1
          in
          v1.(j + 1) <- min (v1.(j) + 1) (min (v0.(j + 1) + 1) (v0.(j) + cost))
        done;

        if
          cutoff < Array.length v1
          && i <= 2 * cutoff
          && (2 * cutoff) - i < String.length s2
        then
          lower_bound := min !lower_bound v1.((2 * cutoff) - i);
        (* did we compute up to the diagonal 2*cutoff+1? *)
        if cutoff < Array.length v1 && i = cutoff * 2 && !lower_bound >= cutoff
        then
          raise_notrace Exit;

        (* copy v1 into v0 for next iteration *)
        Array.blit v1 0 v0 0 (length s2 + 1)
      done;
      v1.(length s2)
    with Exit -> cutoff
  )

let repeat s n =
  assert (n >= 0);
  let len = String.length s in
  assert (len > 0);
  init (len * n) (fun i -> s.[i mod len])

let prefix ~pre s =
  let len = String.length pre in
  if len > String.length s then
    false
  else (
    let rec check i =
      if i = len then
        true
      else if Stdlib.( <> ) (String.unsafe_get s i) (String.unsafe_get pre i)
      then
        false
      else
        check (i + 1)
    in
    check 0
  )

let suffix ~suf s =
  let len = String.length suf in
  if len > String.length s then
    false
  else (
    let off = String.length s - len in
    let rec check i =
      if i = len then
        true
      else if
        Stdlib.( <> ) (String.unsafe_get s (off + i)) (String.unsafe_get suf i)
      then
        false
      else
        check (i + 1)
    in
    check 0
  )

let take n s =
  if n < String.length s then
    String.sub s 0 n
  else
    s

let take_while f s =
  let i = ref 0 in
  while !i < String.length s && f (String.unsafe_get s !i) do
    incr i
  done;
  String.sub s 0 !i

let rtake_while f s =
  let s_len_pred = String.length s - 1 in
  let i = ref s_len_pred in
  while !i >= 0 && f (String.unsafe_get s !i) do
    decr i
  done;
  if !i < s_len_pred then
    String.sub s (!i + 1) (s_len_pred - !i)
  else
    ""

let drop n s =
  if n < String.length s then
    String.sub s n (String.length s - n)
  else
    ""

let take_drop n s = take n s, drop n s

let chop_suffix ~suf s =
  if suffix ~suf s then
    Some (String.sub s 0 (String.length s - String.length suf))
  else
    None

let chop_prefix ~pre s =
  if prefix ~pre s then
    Some
      (String.sub s (String.length pre) (String.length s - String.length pre))
  else
    None

let blit = String.blit

let fold f acc s =
  let rec fold_rec f acc s i =
    if i = String.length s then
      acc
    else
      fold_rec f (f acc s.[i]) s (i + 1)
  in
  fold_rec f acc s 0

let foldi f acc s =
  let rec fold_rec f acc s i =
    if i = String.length s then
      acc
    else
      fold_rec f (f acc i s.[i]) s (i + 1)
  in
  fold_rec f acc s 0

let pad ?(side = `Left) ?(c = ' ') n s =
  let len_s = String.length s in
  if len_s >= n then
    s
  else (
    let pad_len = n - len_s in
    match side with
    | `Left ->
      init n (fun i ->
          if i < pad_len then
            c
          else
            s.[i - pad_len])
    | `Right ->
      init n (fun i ->
          if i < len_s then
            s.[i]
          else
            c)
  )

let _to_gen s i0 len =
  let i = ref i0 in
  fun () ->
    if !i = i0 + len then
      None
    else (
      let c = String.unsafe_get s !i in
      incr i;
      Some c
    )

let to_gen s = _to_gen s 0 (String.length s)
let of_char c = String.make 1 c

let of_gen g =
  let b = Buffer.create 32 in
  let rec aux () =
    match g () with
    | None -> Buffer.contents b
    | Some c ->
      Buffer.add_char b c;
      aux ()
  in
  aux ()

let to_iter s k = String.iter k s

let rec _to_seq s i len () =
  if len = 0 then
    Seq.Nil
  else
    Seq.Cons (s.[i], _to_seq s (i + 1) (len - 1))

let to_seq s = _to_seq s 0 (String.length s)

let of_iter i =
  let b = Buffer.create 32 in
  i (Buffer.add_char b);
  Buffer.contents b

let of_seq seq =
  let b = Buffer.create 32 in
  Seq.iter (Buffer.add_char b) seq;
  Buffer.contents b

let to_list s = _to_list s [] 0 (String.length s)

let of_list l =
  let buf = Buffer.create (List.length l) in
  List.iter (Buffer.add_char buf) l;
  Buffer.contents buf

let of_array a = init (Array.length a) (fun i -> a.(i))
let to_array s = Array.init (String.length s) (fun i -> s.[i])

let lines_gen s =
  Split.gen_cpy ~drop:{ Split.first = false; last = true } ~by:"\n" s

let lines_iter s =
  Split.iter_cpy ~drop:{ Split.first = false; last = true } ~by:"\n" s

let lines_seq s =
  Split.seq_cpy ~drop:{ Split.first = false; last = true } ~by:"\n" s

let lines s =
  Split.list_cpy ~drop:{ Split.first = false; last = true } ~by:"\n" s

let concat_gen_buf ~sep g : Buffer.t =
  let b = Buffer.create 256 in
  let rec aux ~first () =
    match g () with
    | None -> b
    | Some s ->
      if not first then Buffer.add_string b sep;
      Buffer.add_string b s;
      aux ~first:false ()
  in
  aux ~first:true ()

let concat_gen ~sep g =
  let buf = concat_gen_buf ~sep g in
  Buffer.contents buf

let concat_iter_buf ~sep i : Buffer.t =
  let buf = Buffer.create 256 in
  let first = ref true in
  i (fun s ->
      if !first then
        first := false
      else
        Buffer.add_string buf sep;
      Buffer.add_string buf s);
  buf

let concat_iter ~sep i =
  let buf = concat_iter_buf ~sep i in
  Buffer.contents buf

let concat_seq_buf ~sep seq : Buffer.t =
  let buf = Buffer.create 256 in
  let first = ref true in
  Seq.iter
    (fun s ->
      if !first then
        first := false
      else
        Buffer.add_string buf sep;
      Buffer.add_string buf s)
    seq;
  buf

let concat_seq ~sep seq =
  let buf = concat_seq_buf ~sep seq in
  Buffer.contents buf

let unlines l =
  let len = List.fold_left (fun n s -> n + 1 + String.length s) 0 l in
  let buf = Bytes.create len in
  let rec aux_blit i l =
    match l with
    | [] ->
      assert (i = len);
      Bytes.to_string buf
    | s :: tail ->
      let len_s = String.length s in
      Bytes.blit_string s 0 buf i len_s;
      Bytes.set buf (i + len_s) '\n';
      aux_blit (i + len_s + 1) tail
  in
  aux_blit 0 l

let unlines_gen g =
  let buf = concat_gen_buf ~sep:"\n" g in
  Buffer.add_char buf '\n';
  Buffer.contents buf

let unlines_iter i =
  let buf = concat_iter_buf ~sep:"\n" i in
  Buffer.add_char buf '\n';
  Buffer.contents buf

let unlines_seq seq =
  let buf = concat_seq_buf ~sep:"\n" seq in
  Buffer.add_char buf '\n';
  Buffer.contents buf

let set s i c =
  if i < 0 || i >= String.length s then invalid_arg "CCString.set";
  init (String.length s) (fun j ->
      if i = j then
        c
      else
        s.[j])

let iter = String.iter

let filter_map f s =
  let buf = Buffer.create (String.length s) in
  iter
    (fun c ->
      match f c with
      | None -> ()
      | Some c' -> Buffer.add_char buf c')
    s;
  Buffer.contents buf

let filter f s =
  let buf = Buffer.create (String.length s) in
  iter (fun c -> if f c then Buffer.add_char buf c) s;
  Buffer.contents buf

let uniq eq s =
  if String.length s = 0 then
    s
  else (
    let buf = Buffer.create (String.length s) in
    Buffer.add_char buf s.[0];
    let _ =
      fold
        (fun previous_c c ->
          if not (eq previous_c c) then Buffer.add_char buf c;
          c)
        s.[0] s
    in
    Buffer.contents buf
  )

let flat_map ?sep f s =
  let buf = Buffer.create (String.length s) in
  iteri
    (fun i c ->
      (match sep with
      | Some _ when i = 0 -> ()
      | None -> ()
      | Some sep -> Buffer.add_string buf sep);
      Buffer.add_string buf (f c))
    s;
  Buffer.contents buf

exception MyExit

let for_all p s =
  try
    iter (fun c -> if not (p c) then raise MyExit) s;
    true
  with MyExit -> false

let exists p s =
  try
    iter (fun c -> if p c then raise MyExit) s;
    false
  with MyExit -> true

let drop_while f s =
  let i = ref 0 in
  while !i < length s && f (unsafe_get s !i) do
    incr i
  done;
  if !i > 0 then
    sub s !i (length s - !i)
  else
    s

let rdrop_while f s =
  let i = ref (length s - 1) in
  while !i >= 0 && f (unsafe_get s !i) do
    decr i
  done;
  if !i < length s - 1 then
    sub s 0 (!i + 1)
  else
    s

(* notion of whitespace for trim *)
let is_space_ = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let ltrim s = drop_while is_space_ s
let rtrim s = rdrop_while is_space_ s

let map2 f s1 s2 =
  if length s1 <> length s2 then invalid_arg "CCString.map2";
  init (String.length s1) (fun i -> f s1.[i] s2.[i])

let iter2 f s1 s2 =
  if length s1 <> length s2 then invalid_arg "CCString.iter2";
  for i = 0 to String.length s1 - 1 do
    f s1.[i] s2.[i]
  done

let iteri2 f s1 s2 =
  if length s1 <> length s2 then invalid_arg "CCString.iteri2";
  for i = 0 to String.length s1 - 1 do
    f i s1.[i] s2.[i]
  done

let fold2 f acc s1 s2 =
  if length s1 <> length s2 then invalid_arg "CCString.fold2";
  let rec fold' acc s1 s2 i =
    if i = String.length s1 then
      acc
    else
      fold' (f acc s1.[i] s2.[i]) s1 s2 (i + 1)
  in
  fold' acc s1 s2 0

let for_all2 p s1 s2 =
  try
    iter2 (fun c1 c2 -> if not (p c1 c2) then raise MyExit) s1 s2;
    true
  with MyExit -> false

let exists2 p s1 s2 =
  try
    iter2 (fun c1 c2 -> if p c1 c2 then raise MyExit) s1 s2;
    false
  with MyExit -> true

(** {2 Ascii functions} *)

let equal_caseless s1 s2 : bool =
  String.length s1 = String.length s2
  && for_all2
       (fun c1 c2 ->
         CCChar.equal (CCChar.lowercase_ascii c1) (CCChar.lowercase_ascii c2))
       s1 s2

let to_hex (s : string) : string =
  let i_to_hex (i : int) =
    if i < 10 then
      Char.chr (i + Char.code '0')
    else
      Char.chr (i - 10 + Char.code 'a')
  in

  let res = Bytes.create (2 * length s) in
  for i = 0 to length s - 1 do
    let n = Char.code (get s i) in
    Bytes.set res (2 * i) (i_to_hex ((n land 0xf0) lsr 4));
    Bytes.set res ((2 * i) + 1) (i_to_hex (n land 0x0f))
  done;
  Bytes.unsafe_to_string res

let of_hex_exn (s : string) : string =
  let n_of_c = function
    | '0' .. '9' as c -> Char.code c - Char.code '0'
    | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
    | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
    | _ -> invalid_arg "string: invalid hex"
  in
  if String.length s mod 2 <> 0 then
    invalid_arg "string: hex sequence must be of even length";
  let res = Bytes.make (String.length s / 2) '\x00' in
  for i = 0 to (String.length s / 2) - 1 do
    let n1 = n_of_c (String.get s (2 * i)) in
    let n2 = n_of_c (String.get s ((2 * i) + 1)) in
    let n = (n1 lsl 4) lor n2 in
    Bytes.set res i (Char.chr n)
  done;
  Bytes.unsafe_to_string res

let of_hex s = try Some (of_hex_exn s) with Invalid_argument _ -> None

let pp_buf buf s =
  Buffer.add_char buf '"';
  Buffer.add_string buf s;
  Buffer.add_char buf '"'

let pp fmt s = Format.fprintf fmt "\"%s\"" s

module Infix = struct
  let ( = ) = equal
  let ( <> ) a b = not (equal a b)
  let ( > ) : t -> t -> bool = Stdlib.( > )
  let ( >= ) : t -> t -> bool = Stdlib.( >= )
  let ( < ) : t -> t -> bool = Stdlib.( < )
  let ( <= ) : t -> t -> bool = Stdlib.( <= )
end

include Infix
