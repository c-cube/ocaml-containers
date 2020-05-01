
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic String Utils} *)

open CCShims_

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

(* standard implementations *)

include String

let compare_int (a : int) b = Stdlib.compare a b
let compare = String.compare

let hash s = Hashtbl.hash s

let length = String.length

let is_empty s = equal s ""

let rev s =
  let n = length s in
  init n (fun i -> s.[n-i-1])

(*$Q
  Q.printable_string (fun s -> s = rev (rev s))
  Q.printable_string (fun s -> length s = length (rev s))
*)

(*$Q
  Q.printable_string (fun s -> \
    rev s = (to_list s |> List.rev |> of_list))
*)


(*$=
  "abc" (rev "cba")
  "" (rev "")
  " " (rev " ")
*)

let rec _to_list s acc i len =
  if len=0 then List.rev acc
  else _to_list s (s.[i]::acc) (i+1) (len-1)

let _is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
    then true
    else CCChar.equal sub.[i+k] s.[j+k] && check (k+1)
  in
  j+len <= String.length s && check 0

let is_sub ~sub i s j ~len =
  if i+len > String.length sub then invalid_arg "CCString.is_sub";
  _is_sub ~sub i s j ~len

type _ direction =
  | Direct : [`Direct] direction
  | Reverse : [`Reverse] direction

(* we follow https://en.wikipedia.org/wiki/Knuth–Morris–Pratt_algorithm *)
module Find = struct
  type 'a kmp_pattern = {
    failure : int array;
    str : string;
  }
  (* invariant: [length failure = length str].
     We use a phantom type to avoid mixing the directions. *)

  let kmp_pattern_length p = String.length p.str

  (* access the [i]-th element of [s] according to direction [dir] *)
  let get_
    : type a. dir:a direction -> string -> int -> char
    = fun ~dir -> match dir with
      | Direct -> String.get
      | Reverse -> (fun s i -> s.[String.length s - i - 1])

  let kmp_compile_
    : type a. dir:a direction -> string -> a kmp_pattern
    = fun ~dir str ->
      let len = length str in
      let get = get_ ~dir in (* how to read elements of the string *)
      match len with
        | 0 -> {failure=[| |]; str;}
        | 1 -> {failure=[| -1 |]; str;}
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
              | _ when CCChar.equal (get str (!i-1)) (get str !j) ->
                (* substring starting at !j continues matching current char *)
                incr j;
                failure.(!i) <- !j;
                incr i;
              | 0 ->
                (* back to the beginning *)
                failure.(!i) <- 0;
                incr i;
              | _ ->
                (* fallback for the prefix string *)
                assert (!j > 0);
                j := failure.(!j)
          done;
          (* Format.printf "{@[failure:%a, str:%s@]}@." CCFormat.(array int) failure str; *)
          { failure; str; }

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
      if CCChar.equal c expected
      then (
        (* char matches *)
        incr j;
      ) else (
        let fail_offset = pattern.failure.(!j) in
        if fail_offset >= 0
        then (
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
    if !j = pat_len
    then !i
    else -1

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
      let expected = String.get pattern.str (String.length pattern.str - !j - 1) in
      if CCChar.equal c expected
      then (
        (* char matches *)
        incr j;
      ) else (
        let fail_offset = pattern.failure.(!j) in
        if fail_offset >= 0
        then (
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
    if !j = pat_len
    then len - !i - kmp_pattern_length pattern
    else -1

  type 'a pattern =
    | P_char of char
    | P_KMP of 'a kmp_pattern

  let pattern_length = function
    | P_char _ -> 1
    | P_KMP p -> kmp_pattern_length p

  let compile sub : [`Direct] pattern =
    if length sub=1
    then P_char sub.[0]
    else P_KMP (kmp_compile sub)

  let rcompile sub : [`Reverse] pattern =
    if length sub=1
    then P_char sub.[0]
    else P_KMP (kmp_rcompile sub)

  let find ?(start=0) ~(pattern:[`Direct] pattern) s = match pattern with
    | P_char c ->
      (try String.index_from s start c with Not_found -> -1)
    | P_KMP pattern -> kmp_find ~pattern s start

  let rfind ?start ~(pattern:[`Reverse] pattern) s =
    let start = match start with
      | Some n -> n
      | None -> String.length s - 1
    in
    match pattern with
      | P_char c ->
        (try String.rindex_from s start c with Not_found -> -1)
      | P_KMP pattern -> kmp_rfind ~pattern s start
end

let find ?(start=0) ~sub =
  let pattern = Find.compile sub in
  fun s -> Find.find ~start ~pattern s

(*$= & ~printer:string_of_int
  1 (find ~sub:"bc" "abcd")
  ~-1 (find ~sub:"bc" "abd")
  1 (find ~sub:"a" "_a_a_a_")
  6 (find ~start:5 ~sub:"a" "a1a234a")
*)

(*$Q & ~count:10_000
  Q.(pair printable_string printable_string) (fun (s1,s2) -> \
    let i = find ~sub:s2 s1 in \
    i < 0 || String.sub s1 i (length s2) = s2)
*)

let find_all ?(start=0) ~sub =
  let pattern = Find.compile sub in
  fun s ->
    let i = ref start in
    fun () ->
      let res = Find.find ~start:!i ~pattern s in
      if res = ~-1 then None
      else (
        i := res + 1; (* possible overlap *)
        Some res
      )

let find_all_l ?start ~sub s =
  let rec aux acc g = match g () with
    | None -> List.rev acc
    | Some i -> aux (i::acc) g
  in
  aux [] (find_all ?start ~sub s)

(*$= & ~printer:Q.Print.(list int)
  [1; 6] (find_all_l ~sub:"bc" "abc aabc  aab")
  [] (find_all_l ~sub:"bc" "abd")
  [76] (find_all_l ~sub:"aaaaaa" \
    "aabbaabbaaaaabbbbabababababbbbabbbabbaaababbbaaabaabbaabbaaaabbababaaaabbaabaaaaaabbbaaaabababaabaaabbaabaaaabbababbaabbaaabaabbabababbbaabababaaabaaababbbaaaabbbaabaaababbabaababbaabbaaaaabababbabaababbbaaabbabbabababaaaabaaababaaaaabbabbaabbabbbbbbbbbbbbbbaabbabbbbbabbaaabbabbbbabaaaaabbababbbaaaa")
*)

let mem ?start ~sub s = find ?start ~sub s >= 0

(*$T
   mem ~sub:"bc" "abcd"
   not (mem ~sub:"a b" "abcd")
*)

let rfind ~sub =
  let pattern = Find.rcompile sub in
  fun s -> Find.rfind ~start:(String.length s-1) ~pattern s

(*$= & ~printer:string_of_int
  1 (rfind ~sub:"bc" "abcd")
  ~-1 (rfind ~sub:"bc" "abd")
  5 (rfind ~sub:"a" "_a_a_a_")
  4 (rfind ~sub:"bc" "abcdbcd")
  6 (rfind ~sub:"a" "a1a234a")
*)

(*$Q & ~count:10_000
  Q.(pair printable_string printable_string) (fun (s1,s2) -> \
    let i = rfind ~sub:s2 s1 in \
    i < 0 || String.sub s1 i (length s2) = s2)
*)

(* Replace substring [s.[pos]....s.[pos+len-1]] by [by] in [s] *)
let replace_at_ ~pos ~len ~by s =
  let b = Buffer.create (length s + length by - len) in
  Buffer.add_substring b s 0 pos;
  Buffer.add_string b by;
  Buffer.add_substring b s (pos+len) (String.length s - pos - len);
  Buffer.contents b

let replace ?(which=`All) ~sub ~by s =
  if is_empty sub then invalid_arg "CCString.replace";
  match which with
    | `Left ->
      let i = find ~start:0 ~sub s in
      if i>=0 then replace_at_ ~pos:i ~len:(String.length sub) ~by s else s
    | `Right ->
      let i = rfind ~sub s in
      if i>=0 then replace_at_ ~pos:i ~len:(String.length sub) ~by s else s
    | `All ->
      (* compile search pattern only once *)
      let pattern = Find.compile sub in
      let b = Buffer.create (String.length s) in
      let start = ref 0 in
      while !start < String.length s do
        let i = Find.find ~start:!start ~pattern s in
        if i>=0 then (
          (* between last and cur occurrences *)
          Buffer.add_substring b s !start (i- !start);
          Buffer.add_string b by;
          start := i + String.length sub
        ) else (
          (* add remainder *)
          Buffer.add_substring b s !start (String.length s - !start);
          start := String.length s (* stop *)
        )
      done;
      Buffer.contents b

(*$= & ~printer:CCFun.id
  (replace ~which:`All ~sub:"a" ~by:"b" "abcdabcd") "bbcdbbcd"
  (replace ~which:`Left ~sub:"a" ~by:"b" "abcdabcd") "bbcdabcd"
  (replace ~which:`Right ~sub:"a" ~by:"b" "abcdabcd") "abcdbbcd"
  (replace ~which:`All ~sub:"ab" ~by:"hello" "  abab cdabb a") \
    "  hellohello cdhellob a"
  (replace ~which:`Left ~sub:"ab" ~by:"nope" " a b c d ") " a b c d "
  (replace ~sub:"a" ~by:"b" "1aa234a") "1bb234b"
*)

module Split = struct
  type drop_if_empty = {
    first: bool;
    last: bool;
  }

  let no_drop = {first=false; last=false}
  let default_drop = no_drop

  type split_state =
    | SplitStop
    | SplitAt of int (* previous *)

  let rec _split ~by s state = match state with
    | SplitStop -> None
    | SplitAt prev -> _split_search ~by s prev

  and _split_search ~by s prev =
    let j = Find.find ~start:prev ~pattern:by s in
    if j < 0
    then Some (SplitStop, prev, String.length s - prev)
    else Some (SplitAt (j+Find.pattern_length by), prev, j-prev)

  let _tuple3 x y z = x,y,z

  let _mkgen ~drop ~by s k =
    let state = ref (SplitAt 0) in
    let by = Find.compile by in
    let rec next() =
      match _split ~by s !state with
        | None -> None
        | Some (state', 0, 0) when drop.first -> state := state'; next()
        | Some (_, i, 0) when drop.last && i = length s -> None
        | Some (state', i, len) ->
          state := state';
          Some (k s i len)
    in
    next

  let gen ?(drop=default_drop) ~by s = _mkgen ~drop ~by s _tuple3

  let gen_cpy ?(drop=default_drop) ~by s = _mkgen ~drop ~by s String.sub

  let _mklist ~drop ~by s k =
    let by = Find.compile by in
    let rec build acc state = match _split ~by s state with
      | None -> List.rev acc
      | Some (state',0,0) when drop.first -> build acc state'
      | Some (_, i, 0) when drop.last && i=length s -> List.rev acc
      | Some (state', i, len) ->
        build (k s i len ::acc) state'
    in
    build [] (SplitAt 0)

  let list_ ?(drop=default_drop) ~by s = _mklist ~drop ~by s _tuple3

  let list_cpy ?(drop=default_drop) ~by s = _mklist ~drop ~by s String.sub

  (*$T
    Split.list_cpy ~by:"," "aa,bb,cc" = ["aa"; "bb"; "cc"]
    Split.list_cpy ~by:"--" "a--b----c--" = ["a"; "b"; ""; "c"; ""]
    Split.list_cpy ~by:" " "hello  world aie" = ["hello"; ""; "world"; "aie"]
  *)

  let _mkseq ~drop ~by s k =
    let by = Find.compile by in
    let rec make state () = match _split ~by s state with
      | None -> Seq.Nil
      | Some (state', 0, 0) when drop.first -> make state' ()
      | Some (_, i, 0) when drop.last && i=length s -> Seq.Nil
      | Some (state', i, len) ->
        Seq.Cons (k s i len , make state')
    in make (SplitAt 0)

  let std_seq ?(drop=default_drop) ~by s = _mkseq ~drop ~by s _tuple3

  let std_seq_cpy ?(drop=default_drop) ~by s = _mkseq ~drop ~by s String.sub

  let _mkklist ~drop ~by s k =
    let by = Find.compile by in
    let rec make state () = match _split ~by s state with
      | None -> `Nil
      | Some (state', 0, 0) when drop.first -> make state' ()
      | Some (_, i, 0) when drop.last && i=length s -> `Nil
      | Some (state', i, len) ->
        `Cons (k s i len , make state')
    in make (SplitAt 0)

  let klist ?(drop=default_drop) ~by s = _mkklist ~drop ~by s _tuple3

  let klist_cpy ?(drop=default_drop) ~by s = _mkklist ~drop ~by s String.sub

  let _mk_iter ~drop ~by s f k =
    let by = Find.compile by in
    let rec aux state = match _split ~by s state with
      | None -> ()
      | Some (state', 0, 0) when drop.first -> aux state'
      | Some (_, i, 0) when drop.last && i=length s -> ()
      | Some (state', i, len) -> k (f s i len); aux state'
    in aux (SplitAt 0)

  let iter ?(drop=default_drop) ~by s = _mk_iter ~drop ~by s _tuple3
  let iter_cpy ?(drop=default_drop) ~by s = _mk_iter ~drop ~by s String.sub

  let seq = iter
  let seq_cpy = iter_cpy

  let left_exn ~by s =
    let i = find ~sub:by s in
    if i = ~-1 then raise Not_found
    else
      let right = i + String.length by in
      String.sub s 0 i, String.sub s right (String.length s - right)

  let left ~by s = try Some (left_exn ~by s) with Not_found -> None

  (*$T
    Split.left ~by:" " "ab cde f g " = Some ("ab", "cde f g ")
    Split.left ~by:"__" "a__c__e_f" = Some ("a", "c__e_f")
    Split.left ~by:"_" "abcde" = None
    Split.left ~by:"bb" "abbc" = Some ("a", "c")
    Split.left ~by:"a_" "abcde" = None
  *)

  let right_exn ~by s =
    let i = rfind ~sub:by s in
    if i = ~-1 then raise Not_found
    else
      let right = i + String.length by in
      String.sub s 0 i, String.sub s right (String.length s - right)

  let right ~by s = try Some (right_exn ~by s) with Not_found -> None

  (*$T
    Split.right ~by:" " "ab cde f g" = Some ("ab cde f", "g")
    Split.right ~by:"__" "a__c__e_f" = Some ("a__c", "e_f")
    Split.right ~by:"_" "abcde" = None
    Split.right ~by:"a_" "abcde" = None
  *)
end

let split_on_char c s: _ list =
  Split.list_cpy ~drop:Split.no_drop ~by:(String.make 1 c) s

(*$= & ~printer:Q.Print.(list string)
  ["a"; "few"; "words"; "from"; "our"; "sponsors"] \
    (split_on_char ' ' "a few words from our sponsors")
*)

(*$Q
  Q.(printable_string) (fun s -> \
    let s = split_on_char ' ' s |> String.concat " " in \
    s = (split_on_char ' ' s |> String.concat " "))
*)

let split ~by s = Split.list_cpy ~by s

let compare_versions a b =
  let of_int s = try Some (int_of_string s) with Failure _ -> None in
  let rec cmp_rec a b = match a(), b() with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some x, Some y ->
      match of_int x, of_int y with
        | None, None ->
          let c = String.compare x y in
          if c<>0 then c else cmp_rec a b
        | Some _, None -> 1
        | None, Some _ -> -1
        | Some x, Some y ->
          let c = compare_int x y in
          if c<>0 then c else cmp_rec a b
  in
  cmp_rec (Split.gen_cpy ~by:"." a) (Split.gen_cpy ~by:"." b)

(*$T
  compare_versions "0.1.3" "0.1" > 0
  compare_versions "10.1" "2.0" > 0
  compare_versions "0.1.alpha" "0.1" > 0
  compare_versions "0.3.dev" "0.4" < 0
  compare_versions "0.foo" "0.0" < 0
  compare_versions "1.2.3.4" "01.2.4.3" < 0
*)

(*$Q
  Q.(pair printable_string printable_string) (fun (a,b) -> \
    CCOrd.equiv (compare_versions a b) (CCOrd.opp compare_versions b a))
*)

type nat_chunk =
  | NC_char of char
  | NC_int of int

let compare_natural a b =
  (* stream of chunks *)
  let chunks s : unit -> nat_chunk option =
    let i = ref 0 in
    let rec next () =
      if !i = length s then None
      else match String.get s !i with
        | '0'..'9' as c -> incr i; read_int (Char.code c - Char.code '0')
        | c -> incr i; Some (NC_char c)
    and read_int n =
      if !i = length s then Some (NC_int n)
      else match String.get s !i with
        | '0'..'9' as c -> incr i; read_int (10 * n + Char.code c - Char.code '0')
        | _ -> Some (NC_int n)
    in
    next
  in
  let rec cmp_rec a b = match a(), b() with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some x, Some y ->
      match x, y with
        | NC_char x, NC_char y ->
          let c = Char.compare x y in
          if c<>0 then c else cmp_rec a b
        | NC_int _, NC_char _ -> 1
        | NC_char _, NC_int _ -> -1
        | NC_int x, NC_int y ->
          let c = compare_int x y in
          if c<>0 then c else cmp_rec a b
  in
  cmp_rec (chunks a) (chunks b)

(*$T
  compare_natural "foo1" "foo2" < 0
  compare_natural "foo11" "foo2" > 0
  compare_natural "foo11" "foo11" = 0
  compare_natural "foo011" "foo11" = 0
  compare_natural "foo1a" "foo1b" < 0
  compare_natural "foo1a1" "foo1a2" < 0
  compare_natural "foo1a17" "foo1a2" > 0
*)

(*Q
  (Q.pair printable_string printable_string) (fun (a,b) -> \
    CCOrd.opp (compare_natural a b) = compare_natural b a)
  (Q.printable_string) (fun a -> compare_natural a a = 0)
  (Q.triple printable_string printable_string printable_string) (fun (a,b,c) -> \
    if compare_natural a b < 0 && compare_natural b c < 0 \
    then compare_natural a c < 0 else Q.assume_fail())
*)

let edit_distance ?(cutoff=max_int) s1 s2 =
  if length s1 = 0
  then min cutoff (length s2)
  else if length s2 = 0
  then min cutoff (length s1)
  else if equal s1 s2
  then 0
  else try
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
      v1.(0) <- i+1;

      (* try add/delete/replace operations *)
      for j = 0 to length s2 - 1 do
        let cost = if Char.equal (String.get s1 i) (String.get s2 j) then 0 else 1 in
        v1.(j+1) <- min (v1.(j) + 1) (min (v0.(j+1) + 1) (v0.(j) + cost));
      done;

      if cutoff < Array.length v1 && i <= 2 * cutoff &&
         2 * cutoff - i < String.length s2 then (
        lower_bound := min !lower_bound v1.(2 * cutoff - i);
      );
      (* did we compute up to the diagonal 2*cutoff+1? *)
      if cutoff < Array.length v1 && i = cutoff * 2 && !lower_bound >= cutoff then (
        raise_notrace Exit;
      );

      (* copy v1 into v0 for next iteration *)
      Array.blit v1 0 v0 0 (length s2 + 1);
    done;
    v1.(length s2)
  with Exit -> cutoff

(*$Q
  Q.(string_of_size Gen.(0 -- 30)) (fun s -> \
    edit_distance s s = 0)
  Q.(let p = string_of_size Gen.(0 -- 20) in pair p p) (fun (s1,s2) -> \
    edit_distance s1 s2 = edit_distance s2 s1)
  Q.(let p = string_of_size Gen.(0 -- 20) in pair p p) (fun (s1,s2) -> \
    let e = edit_distance s1 s2 in \
    let e' = edit_distance ~cutoff:3 s1 s2 in \
    (if e' < 3 then e=e' else e >= 3) && \
    (if e <= 3 then e=e' else true))
*)

(*$= & ~printer:string_of_int
  2 (edit_distance "hello" "helo!")
  5 (edit_distance "abcde" "tuvwx")
  2 (edit_distance ~cutoff:2 "abcde" "tuvwx")
  1 (edit_distance ("a" ^ String.make 100 '_') ("b"^String.make 100 '_'))
  1 (edit_distance ~cutoff:4 ("a" ^ String.make 1000 '_') ("b"^String.make 1000 '_'))
  2 (edit_distance ~cutoff:3 ("a" ^ String.make 1000 '_' ^ "c")\
       ("b" ^ String.make 1000 '_' ^ "d"))
*)

(* test that building a from s, and mutating one char of s, yields
   a string s' that is accepted by a.

   --> generate triples (s, i, c) where c is a char, s a non empty string
   and i a valid index in s.
*)

(*$QR
  (
    let gen = Q.Gen.(
      3 -- 10 >>= fun len ->
      0 -- (len-1) >>= fun i ->
      string_size (return len) >>= fun s ->
      char >|= fun c -> (s,i,c)
    ) in
    let small (s,_,_) = String.length s in
    Q.make ~small gen
  )
  (fun (s,i,c) ->
    let s' = Bytes.of_string s in
    Bytes.set s' i c;
    edit_distance s (Bytes.to_string s') <= 1)
*)

let repeat s n =
  assert (n>=0);
  let len = String.length s in
  assert(len > 0);
  init (len * n) (fun i -> s.[i mod len])

let prefix ~pre s =
  let len = String.length pre in
  if len > String.length s then false
  else (
    let rec check i =
      if i=len then true
      else if Stdlib.(<>) (String.unsafe_get s i) (String.unsafe_get pre i) then false
      else check (i+1)
    in
    check 0
  )

(*$T
  prefix ~pre:"aab" "aabcd"
  not (prefix ~pre:"ab" "aabcd")
  not (prefix ~pre:"abcd" "abc")
  prefix ~pre:"abc" "abcde"
  prefix ~pre:"" ""
  prefix ~pre:"" "abc"
  prefix ~pre:"abc" "abc"
*)

let suffix ~suf s =
  let len = String.length suf in
  if len > String.length s then false
  else (
    let off = String.length s - len in
    let rec check i =
      if i=len then true
      else if Stdlib.(<>) (String.unsafe_get s (off+i)) (String.unsafe_get suf i) then false
      else check (i+1)
    in
    check 0
  )

(*$T
  suffix ~suf:"cd" "abcd"
  suffix ~suf:"" ""
  suffix ~suf:"" "abc"
  not (suffix ~suf:"cd" "abcde")
  not (suffix ~suf:"abcd" "cd")
*)

let take n s =
  if n < String.length s
  then String.sub s 0 n
  else s

let drop n s =
  if n < String.length s
  then String.sub s n (String.length s - n)
  else ""

let take_drop n s = take n s, drop n s

(*$=
  ("ab", "cd") (take_drop 2 "abcd")
  ("abc", "") (take_drop 3 "abc")
  ("abc", "") (take_drop 5 "abc")
*)

let chop_suffix ~suf s =
  if suffix ~suf s
  then Some (String.sub s 0 (String.length s-String.length suf))
  else None

(*$= & ~printer:Q.Print.(option string)
  (Some "ab") (chop_suffix ~suf:"cd" "abcd")
  None (chop_suffix ~suf:"cd" "abcde")
  None (chop_suffix ~suf:"abcd" "cd")
*)

let chop_prefix ~pre s =
  if prefix ~pre s
  then Some (String.sub s (String.length pre) (String.length s-String.length pre))
  else None

(*$= & ~printer:Q.Print.(option string)
  (Some "cd") (chop_prefix ~pre:"aab" "aabcd")
  None (chop_prefix ~pre:"ab" "aabcd")
  None (chop_prefix ~pre:"abcd" "abc")
*)

let blit = String.blit

let fold f acc s =
  let rec fold_rec f acc s i =
    if i = String.length s then acc
    else fold_rec f (f acc s.[i]) s (i+1)
  in fold_rec f acc s 0

let pad ?(side=`Left) ?(c=' ') n s =
  let len_s = String.length s in
  if len_s >= n then s
  else
    let pad_len = n - len_s in
    match side with
      | `Left -> init n (fun i -> if i < pad_len then c else s.[i-pad_len])
      | `Right -> init n (fun i -> if i < len_s then s.[i] else c)

(*$= & ~printer:Q.Print.string
  "  42" (pad 4 "42")
  "0042" (pad ~c:'0' 4 "42")
  "4200" (pad ~side:`Right ~c:'0' 4 "42")
  "hello" (pad 4 "hello")
  "aaa" (pad ~c:'a' 3 "")
  "aaa" (pad ~side:`Right ~c:'a' 3 "")
*)

let _to_gen s i0 len =
  let i = ref i0 in
  fun () ->
    if !i = i0+len then None
    else (
      let c = String.unsafe_get s !i in
      incr i;
      Some c
    )

let to_gen s = _to_gen s 0 (String.length s)

let of_char c = String.make 1 c

let of_gen g =
  let b = Buffer.create 32 in
  let rec aux () = match g () with
    | None -> Buffer.contents b
    | Some c -> Buffer.add_char b c; aux ()
  in aux ()

let to_iter s k = String.iter k s

let rec _to_std_seq s i len () =
  if len=0 then Seq.Nil
  else Seq.Cons (s.[i], _to_std_seq s (i+1)(len-1))

let to_std_seq s = _to_std_seq s 0 (String.length s)

let of_iter i =
  let b = Buffer.create 32 in
  i (Buffer.add_char b);
  Buffer.contents b

let of_std_seq seq =
  let b = Buffer.create 32 in
  Seq.iter (Buffer.add_char b) seq;
  Buffer.contents b

let rec _to_klist s i len () =
  if len=0 then `Nil
  else `Cons (s.[i], _to_klist s (i+1)(len-1))

let of_klist l =
  let b = Buffer.create 15 in
  let rec aux l = match l() with
    | `Nil ->
      Buffer.contents b
    | `Cons (x,l') ->
      Buffer.add_char b x;
      aux l'
  in aux l

let to_klist s = _to_klist s 0 (String.length s)

let to_list s = _to_list s [] 0 (String.length s)

let of_list l =
  let buf = Buffer.create (List.length l) in
  List.iter (Buffer.add_char buf) l;
  Buffer.contents buf

(*$T
  of_list ['a'; 'b'; 'c'] = "abc"
  of_list [] = ""
*)

let of_array a =
  init (Array.length a) (fun i -> a.(i))

let to_array s =
  Array.init (String.length s) (fun i -> s.[i])

let lines_gen s = Split.gen_cpy ~drop:{Split.first=false; last=true} ~by:"\n" s

let lines s = Split.list_cpy ~drop:{Split.first=false; last=true} ~by:"\n" s

(*$= & ~printer:Q.Print.(list @@ Printf.sprintf "%S")
  ["ab"; "c"] (lines "ab\nc")
  ["ab"; "c"] (lines "ab\nc\n")
  [] (lines "")
  [""] (lines "\n")
  [""; "a"] (lines "\na")
*)

let concat_gen_buf ~sep g : Buffer.t =
  let b = Buffer.create 256 in
  let rec aux ~first () = match g () with
    | None -> b
    | Some s ->
      if not first then Buffer.add_string b sep;
      Buffer.add_string b s;
      aux ~first:false ()
  in aux ~first:true ()

let concat_gen ~sep g =
  let buf = concat_gen_buf ~sep g in
  Buffer.contents buf

let unlines l =
  let len = List.fold_left (fun n s -> n + 1 + String.length s) 0 l in
  let buf = Bytes.create len in
  let rec aux_blit i l = match l with
    | [] ->
      assert (i=len);
      Bytes.to_string buf
    | s :: tail ->
      let len_s = String.length s in
      Bytes.blit_string s 0 buf i len_s;
      Bytes.set buf (i+len_s) '\n';
      aux_blit (i+len_s+1) tail
  in
  aux_blit 0 l

let unlines_gen g =
  let buf = concat_gen_buf ~sep:"\n" g in
  Buffer.add_char buf '\n';
  Buffer.contents buf

(*$= & ~printer:CCFun.id
  "" (unlines [])
  "ab\nc\n" (unlines ["ab"; "c"])
*)

(*$Q
  Q.printable_string (fun s -> trim (unlines (lines s)) = trim s)
  Q.printable_string (fun s -> trim (unlines_gen (lines_gen s)) = trim s)
*)

(*$Q
  Q.(small_list small_string) (fun l -> \
    let l = unlines l |> lines in \
    l = (unlines l |> lines))
*)

let set s i c =
  if i<0 || i>= String.length s then invalid_arg "CCString.set";
  init (String.length s) (fun j -> if i=j then c else s.[j])

(*$T
  set "abcd" 1 '_' = "a_cd"
  set "abcd" 0 '-' = "-bcd"
  (try ignore (set "abc" 5 '_'); false with Invalid_argument _ -> true)
*)

let iter = String.iter

let filter_map f s =
  let buf = Buffer.create (String.length s) in
  iter
    (fun c -> match f c with
       | None -> ()
       | Some c' -> Buffer.add_char buf c')
    s;
  Buffer.contents buf

(*$= & ~printer:Q.Print.string
  "bcef" (filter_map \
     (function 'c' -> None | c -> Some (Char.chr (Char.code c + 1))) "abcde")
*)

let filter f s =
  let buf = Buffer.create (String.length s) in
  iter
    (fun c -> if f c then Buffer.add_char buf c)
    s;
  Buffer.contents buf

(*$= & ~printer:Q.Print.string
  "abde" (filter (function 'c' -> false | _ -> true) "abcdec")
*)

(*$Q
  Q.printable_string (fun s -> filter (fun _ -> true) s = s)
*)

let flat_map ?sep f s =
  let buf = Buffer.create (String.length s) in
  iteri
    (fun i c ->
       begin match sep with
         | Some _ when i=0 -> ()
         | None -> ()
         | Some sep -> Buffer.add_string buf sep
       end;
       Buffer.add_string buf (f c)
    ) s;
  Buffer.contents buf

exception MyExit

let for_all p s =
  try iter (fun c -> if not (p c) then raise MyExit) s; true
  with MyExit -> false

let exists p s =
  try iter (fun c -> if p c then raise MyExit) s; false
  with MyExit -> true

let drop_while f s =
  let i = ref 0 in
  while !i < length s && f (unsafe_get s !i) do incr i done;
  if !i > 0 then sub s !i (length s - !i) else s

let rdrop_while f s =
  let i = ref (length s-1) in
  while !i >= 0 && f (unsafe_get s !i) do decr i done;
  if !i < length s-1 then sub s 0 (!i+1) else s

(* notion of whitespace for trim *)
let is_space_ = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let ltrim s = drop_while is_space_ s
let rtrim s = rdrop_while is_space_ s

(*$= & ~printer:id
  "abc " (ltrim " abc ")
  " abc" (rtrim " abc ")
*)

(*$Q
  Q.(printable_string) (fun s -> \
    String.trim s = (s |> ltrim |> rtrim))
  Q.(printable_string) (fun s -> ltrim s = ltrim (ltrim s))
  Q.(printable_string) (fun s -> rtrim s = rtrim (rtrim s))
  Q.(printable_string) (fun s -> \
    let s' = ltrim s in \
    if s'="" then Q.assume_fail() else s'.[0] <> ' ')
  Q.(printable_string) (fun s -> \
    let s' = rtrim s in \
    if s'="" then Q.assume_fail() else s'.[String.length s'-1] <> ' ')
*)

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
    if i = String.length s1 then acc
    else fold' (f acc s1.[i] s2.[i]) s1 s2 (i+1)
  in
  fold' acc s1 s2 0

let for_all2 p s1 s2 =
  try iter2 (fun c1 c2 -> if not (p c1 c2) then raise MyExit) s1 s2; true
  with MyExit -> false

let exists2 p s1 s2 =
  try iter2 (fun c1 c2 -> if p c1 c2 then raise MyExit) s1 s2; false
  with MyExit -> true

(** {2 Ascii functions} *)

let equal_caseless s1 s2: bool =
  String.length s1 = String.length s2 &&
  for_all2
    (fun c1 c2 -> CCChar.equal (CCChar.lowercase_ascii c1) (CCChar.lowercase_ascii c2))
    s1 s2

(*$T
  equal_caseless "foo" "FoO"
  equal_caseless "helLo" "HEllO"
*)

(*$Q
  Q.(pair printable_string printable_string) (fun (s1,s2) -> \
    equal_caseless s1 s2 = (lowercase_ascii s1=lowercase_ascii s2))
  Q.(printable_string) (fun s -> equal_caseless s s)
  Q.(printable_string) (fun s -> equal_caseless (uppercase_ascii s) s)
*)

let pp_buf buf s =
  Buffer.add_char buf '"';
  Buffer.add_string buf s;
  Buffer.add_char buf '"'

let pp fmt s =
  Format.fprintf fmt "\"%s\"" s

(* test consistency of interfaces *)
(*$inject
  module type L = module type of CCString
  module type LL = module type of CCStringLabels
*)

(*$R
  ignore (module CCStringLabels : L)
*)

(*$R
  ignore (module CCString : LL)
*)

