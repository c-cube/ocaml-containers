
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic String Utils} *)

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

module type S = sig
  type t

  val length : t -> int

  val blit : t -> int -> Bytes.t -> int -> int -> unit
  (** Similar to {!String.blit}.
      Compatible with the [-safe-string] option.
      @raise Invalid_argument if indices are not valid *)

  val fold : ('a -> char -> 'a) -> 'a -> t -> 'a

  (** {2 Conversions} *)

  val to_gen : t -> char gen
  val to_seq : t -> char sequence
  val to_klist : t -> char klist
  val to_list : t -> char list

  val pp : Buffer.t -> t -> unit
  val print : Format.formatter -> t -> unit
end

let equal (a:string) b = a=b

let compare = String.compare

let hash s = Hashtbl.hash s

#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2

let init = String.init

#else

let init n f =
  let buf = Bytes.init n f in
  Bytes.unsafe_to_string buf

#endif

let length = String.length

let rev s =
  let n = length s in
  init n (fun i -> s.[n-i-1])

let rec _to_list s acc i len =
  if len=0 then List.rev acc
  else _to_list s (s.[i]::acc) (i+1) (len-1)

let _is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
      then true
      else sub.[i+k] = s.[j+k] && check (k+1)
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
        | _ when get str (!i-1) = get str !j ->
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
      if c = expected
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
      if c = expected
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

  let compile ~sub : [`Direct] pattern =
    if length sub=1
    then P_char sub.[0]
    else P_KMP (kmp_compile sub)

  let rcompile ~sub : [`Reverse] pattern =
    if length sub=1
    then P_char sub.[0]
    else P_KMP (kmp_rcompile sub)

  let find ~pattern s start = match pattern with
    | P_char c ->
        (try String.index_from s start c with Not_found -> -1)
    | P_KMP pattern -> kmp_find ~pattern s start

  let rfind ~pattern s start = match pattern with
    | P_char c ->
        (try String.rindex_from s start c with Not_found -> -1)
    | P_KMP pattern -> kmp_rfind ~pattern s start
end

let find ?(start=0) ~sub =
  let pattern = Find.compile ~sub in
  fun s -> Find.find ~pattern s start

let find_all ?(start=0) ~sub =
  let pattern = Find.compile ~sub in
  fun s ->
    let i = ref start in
    fun () ->
      let res = Find.find ~pattern s !i in
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

let mem ?start ~sub s = find ?start ~sub s >= 0

let rfind ~sub =
  let pattern = Find.rcompile ~sub in
  fun s -> Find.rfind ~pattern s (String.length s-1)

(* Replace substring [s.[pos]....s.[pos+len-1]] by [by] in [s] *)
let replace_at_ ~pos ~len ~by s =
  let b = Buffer.create (length s + length by - len) in
  Buffer.add_substring b s 0 pos;
  Buffer.add_string b by;
  Buffer.add_substring b s (pos+len) (String.length s - pos - len);
  Buffer.contents b

let replace ?(which=`All) ~sub ~by s =
  if sub="" then invalid_arg "CCString.replace";
  match which with
  | `Left ->
      let i = find ~sub s ~start:0 in
      if i>=0 then replace_at_ ~pos:i ~len:(String.length sub) ~by s else s
  | `Right ->
      let i = rfind ~sub s in
      if i>=0 then replace_at_ ~pos:i ~len:(String.length sub) ~by s else s
  | `All ->
      (* compile search pattern only once *)
      let pattern = Find.compile ~sub in
      let b = Buffer.create (String.length s) in
      let start = ref 0 in
      while !start < String.length s do
        let i = Find.find ~pattern s !start in
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

module Split = struct
  type split_state =
    | SplitStop
    | SplitAt of int (* previous *)

  let rec _split ~by s state = match state with
    | SplitStop -> None
    | SplitAt prev -> _split_search ~by s prev
  and _split_search ~by s prev =
    let j = Find.find ~pattern:by s prev in
    if j < 0
      then Some (SplitStop, prev, String.length s - prev)
      else Some (SplitAt (j+Find.pattern_length by), prev, j-prev)

  let _tuple3 x y z = x,y,z

  let _mkgen ~by s k =
    let state = ref (SplitAt 0) in
    let by = Find.compile ~sub:by in
    fun () ->
      match _split ~by s !state with
        | None -> None
        | Some (state', i, len) ->
            state := state';
            Some (k s i len)

  let gen ~by s = _mkgen ~by s _tuple3

  let gen_cpy ~by s = _mkgen ~by s String.sub

  let _mklist ~by s k =
    let by = Find.compile ~sub:by in
    let rec build acc state = match _split ~by s state with
      | None -> List.rev acc
      | Some (state', i, len) ->
          build (k s i len ::acc) state'
    in
    build [] (SplitAt 0)

  let list_ ~by s = _mklist ~by s _tuple3

  let list_cpy ~by s = _mklist ~by s String.sub

  let _mkklist ~by s k =
    let by = Find.compile ~sub:by in
    let rec make state () = match _split ~by s state with
      | None -> `Nil
      | Some (state', i, len) ->
          `Cons (k s i len , make state')
    in make (SplitAt 0)

  let klist ~by s = _mkklist ~by s _tuple3

  let klist_cpy ~by s = _mkklist ~by s String.sub

  let _mkseq ~by s f k =
    let by = Find.compile ~sub:by in
    let rec aux state = match _split ~by s state with
      | None -> ()
      | Some (state', i, len) -> k (f s i len); aux state'
    in aux (SplitAt 0)

  let seq ~by s = _mkseq ~by s _tuple3
  let seq_cpy ~by s = _mkseq ~by s String.sub

  let left_exn ~by s =
    let i = find ~sub:by s in
    if i = ~-1 then raise Not_found
    else
      let right = i + String.length by in
      String.sub s 0 i, String.sub s right (String.length s - right)

  let left ~by s = try Some (left_exn ~by s) with Not_found -> None

  let right_exn ~by s =
    let i = rfind ~sub:by s in
    if i = ~-1 then raise Not_found
    else
      let right = i + String.length by in
      String.sub s 0 i, String.sub s right (String.length s - right)

  let right ~by s = try Some (right_exn ~by s) with Not_found -> None
end

let compare_versions a b =
  let of_int s = try Some (int_of_string s) with _ -> None in
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
            let c = Pervasives.compare x y in
            if c<>0 then c else cmp_rec a b
  in
  cmp_rec (Split.gen_cpy ~by:"." a) (Split.gen_cpy ~by:"." b)

let repeat s n =
  assert (n>=0);
  let len = String.length s in
  assert(len > 0);
  init (len * n) (fun i -> s.[i mod len])

let prefix ~pre s =
  String.length pre <= String.length s &&
  (let i = ref 0 in
    while !i < String.length pre && s.[!i] = pre.[!i] do incr i done;
    !i = String.length pre
  )

let suffix ~suf s =
  String.length suf <= String.length s &&
  let off = String.length s - String.length suf in
  (let i = ref 0 in
    while !i < String.length suf && s.[off + !i] = suf.[!i] do incr i done;
    !i = String.length suf
  )

let take n s =
  if n < String.length s
  then String.sub s 0 n
  else s

let drop n s =
  if n < String.length s
  then String.sub s n (String.length s - n)
  else ""

let take_drop n s = take n s, drop n s

let chop_suffix ~suf s =
  if suffix ~suf s
  then Some (String.sub s 0 (String.length s-String.length suf))
  else None

let chop_prefix ~pre s =
  if prefix ~pre s
  then Some (String.sub s (String.length pre) (String.length s-String.length pre))
  else None

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

let to_seq s k = String.iter k s

let of_seq seq =
  let b= Buffer.create 32 in
  seq (Buffer.add_char b);
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

let of_array a =
  init (Array.length a) (fun i -> a.(i))

let to_array s =
  Array.init (String.length s) (fun i -> s.[i])

let lines_gen s = Split.gen_cpy ~by:"\n" s

let lines s = Split.list_cpy ~by:"\n" s

let concat_gen ~sep g =
  let b = Buffer.create 256 in
  let rec aux ~first () = match g () with
    | None -> Buffer.contents b
    | Some s ->
      if not first then Buffer.add_string b sep;
      Buffer.add_string b s;
      aux ~first:false ()
  in aux ~first:true ()

let unlines l = String.concat "\n" l

let unlines_gen g = concat_gen ~sep:"\n" g

let set s i c =
  if i<0 || i>= String.length s then invalid_arg "CCString.set";
  init (String.length s) (fun j -> if i=j then c else s.[j])

let iter = String.iter

#if OCAML_MAJOR >= 4

let map = String.map
let iteri = String.iteri

#else

let map f s = init (length s) (fun i -> f s.[i])

let iteri f s =
  for i = 0 to String.length s - 1 do
    f i s.[i]
  done

#endif

#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2

let mapi = String.mapi

#else

let mapi f s = init (length s) (fun i -> f i s.[i])

#endif

let filter_map f s =
  let buf = Buffer.create (String.length s) in
  iter
    (fun c -> match f c with
       | None -> ()
       | Some c' -> Buffer.add_char buf c')
    s;
  Buffer.contents buf

let filter f s =
  let buf = Buffer.create (String.length s) in
  iter
    (fun c -> if f c then Buffer.add_char buf c)
    s;
  Buffer.contents buf

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

#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 3

let capitalize_ascii = String.capitalize_ascii
let uncapitalize_ascii = String.uncapitalize_ascii
let uppercase_ascii = String.uppercase_ascii
let lowercase_ascii = String.lowercase_ascii

#else

let capitalize_ascii s =
  mapi
    (fun i c -> if i=0 then CCChar.uppercase_ascii c else c)
    s
    

let uncapitalize_ascii s =
  mapi
    (fun i c -> if i=0 then CCChar.lowercase_ascii c else c)
    s

let uppercase_ascii = map CCChar.uppercase_ascii

let lowercase_ascii = map CCChar.lowercase_ascii

#endif



let pp buf s =
  Buffer.add_char buf '"';
  Buffer.add_string buf s;
  Buffer.add_char buf '"'

let print fmt s =
  Format.fprintf fmt "\"%s\"" s

module Sub = struct
  type t = string * int * int

  let make s i ~len =
    if i<0||len<0||i+len > String.length s then invalid_arg "CCString.Sub.make";
    s,i,len

  let full s = s, 0, String.length s

  let copy (s,i,len) = String.sub s i len

  let underlying (s,_,_) = s

  let sub (s,i,len) i' len' =
    if i+i' + len' > i+len then invalid_arg "CCString.Sub.sub";
    (s, i+i',len')

  let length (_,_,l) = l

  let blit (a1,i1,len1) o1 a2 o2 len =
    if o1+len>len1 then invalid_arg "CCString.Sub.blit";
    blit a1 (i1+o1) a2 o2 len

  let fold f acc (s,i,len) =
    let rec fold_rec f acc s i j =
      if i = j then acc
      else fold_rec f (f acc s.[i]) s (i+1) j
    in fold_rec f acc s i (i+len)

  let to_gen (s,i,len) = _to_gen s i len
  let to_seq (s,i,len) k =
    for i=i to i+len-1 do k s.[i] done
  let to_klist (s,i,len) = _to_klist s i len
  let to_list (s,i,len) = _to_list s [] i len

  let pp buf (s,i,len) =
    Buffer.add_char buf '"';
    Buffer.add_substring buf s i len;
    Buffer.add_char buf '"'

  let print fmt s =
    Format.fprintf fmt "\"%s\"" (copy s)
end
