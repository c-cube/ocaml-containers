
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Basic String Utils} *)

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

module type S = sig
  type t

  val length : t -> int

  val blit : t -> int -> t -> int -> int -> unit
  (** See {!String.blit} *)

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

let rec _to_list s acc i len =
  if len=0 then List.rev acc
  else _to_list s (s.[i]::acc) (i+1) (len-1)

let _is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
      then true
      else sub.[i + k] = s.[j+k] && check (k+1)
  in
  j+len <= String.length s && check 0

let is_sub ~sub i s j ~len =
  if i+len > String.length sub then invalid_arg "String.is_sub";
  _is_sub ~sub i s j ~len

(* note: inefficient *)
let find ?(start=0) ~sub s =
  let n = String.length sub in
  let i = ref start in
  try
    while !i + n < String.length s do
      if _is_sub ~sub 0 s !i ~len:n then raise Exit;
      incr i
    done;
    -1
  with Exit ->
    !i

let rfind ~sub s =
  let n = String.length sub in
  let i = ref (String.length s - n) in
  try
    while !i >= 0 do
      if _is_sub ~sub 0 s !i ~len:n then raise Exit;
      decr i
    done;
    ~-1
  with Exit ->
    !i

module Split = struct
  type split_state =
    | SplitStop
    | SplitAt of int (* previous *)

  (* [by_j... prefix of s_i...] ? *)
  let rec _is_prefix ~by s i j =
    j = String.length by
    ||
    ( i < String.length s &&
      s.[i] = by.[j] &&
      _is_prefix ~by s (i+1) (j+1)
    )

  let rec _split ~by s state = match state with
    | SplitStop -> None
    | SplitAt prev -> _split_search ~by s prev prev
  and _split_search ~by s prev i =
    if i >= String.length s
      then Some (SplitStop, prev, String.length s - prev)
      else if _is_prefix ~by s i 0
        then Some (SplitAt (i+String.length by), prev, i-prev)
      else _split_search ~by s prev (i+1)

  let _tuple3 x y z = x,y,z

  let _mkgen ~by s k =
    let state = ref (SplitAt 0) in
    fun () ->
      match _split ~by s !state with
        | None -> None
        | Some (state', i, len) ->
            state := state';
            Some (k s i len)

  let gen ~by s = _mkgen ~by s _tuple3

  let gen_cpy ~by s = _mkgen ~by s String.sub

  let _mklist ~by s k =
    let rec build acc state = match _split ~by s state with
      | None -> List.rev acc
      | Some (state', i, len) ->
          build (k s i len ::acc) state'
    in
    build [] (SplitAt 0)

  let list_ ~by s = _mklist ~by s _tuple3

  let list_cpy ~by s = _mklist ~by s String.sub

  let _mkklist ~by s k =
    let rec make state () = match _split ~by s state with
      | None -> `Nil
      | Some (state', i, len) ->
          `Cons (k s i len , make state')
    in make (SplitAt 0)

  let klist ~by s = _mkklist ~by s _tuple3

  let klist_cpy ~by s = _mkklist ~by s String.sub

  let _mkseq ~by s f k =
    let rec aux state = match _split ~by s state with
      | None -> ()
      | Some (state', i, len) -> k (f s i len); aux state'
    in aux (SplitAt 0)

  let seq ~by s = _mkseq ~by s _tuple3
  let seq_cpy ~by s = _mkseq ~by s String.sub

  let left ~by s =
    let i = find ~sub:by s in
    if i = ~-1 then None
    else Some (String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1))

  let right ~by s =
    let i = rfind ~sub:by s in
    if i = ~-1 then None
    else Some (String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1))
end

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


let blit = String.blit

let fold f acc s =
  let rec fold_rec f acc s i =
    if i = String.length s then acc
    else fold_rec f (f acc s.[i]) s (i+1)
  in fold_rec f acc s 0

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
  if length s1 <> length s2 then invalid_arg "String.map2";
  init (String.length s1) (fun i -> f s1.[i] s2.[i])

let iter2 f s1 s2 =
  if length s1 <> length s2 then invalid_arg "String.iter2";
  for i = 0 to String.length s1 - 1 do
    f s1.[i] s2.[i]
  done

let iteri2 f s1 s2 =
  if length s1 <> length s2 then invalid_arg "String.iteri2";
  for i = 0 to String.length s1 - 1 do
    f i s1.[i] s2.[i]
  done

let fold2 f acc s1 s2 =
  if length s1 <> length s2 then invalid_arg "String.fold2";
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

  let blit (a1,i1,len1) o1 (a2,i2,len2) o2 len =
    if o1+len>len1 || o2+len>len2 then invalid_arg "CCString.Sub.blit";
    String.blit a1 (i1+o1) a2 (i2+o2) len

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
