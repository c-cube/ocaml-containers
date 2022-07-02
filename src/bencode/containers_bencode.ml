module Str_map = Map.Make(String)

type t =
  | Int of int64
  | String of string
  | List of t list
  | Map of t Str_map.t

let rec equal t1 t2 = match t1, t2 with
  | Int i1, Int i2 -> i1 = i2
  | String s1, String s2 -> s1 = s2
  | List l1, List l2 ->
    (try List.for_all2 equal l1 l2 with Invalid_argument _ -> false)
  | Map d1, Map d2 -> Str_map.equal equal d1 d2
  | (Int _ | String _ | List _ | Map _), _ -> false

let rec hash t =
  let module H = CCHash in
  match t with
  | Int i -> H.int64 i
  | String s -> H.combine2 10 (H.string s)
  | List l -> H.combine2 20 (H.list hash l)
  | Map l ->
    H.combine2 30
      (H.iter (H.pair H.string hash) @@
      (fun k -> Str_map.iter (fun x y -> k(x,y)) l))

let int64 i : t = Int i
let int i : t = int64 (Int64.of_int i)
let string s : t = String s
let list l : t = List l
let map m : t = Map m
let map_of_list l : t =
  map @@ List.fold_left (fun m (k,v) -> Str_map.add k v m) Str_map.empty l

let rec pp_debug out (self:t) : unit =
  let fpf = Format.fprintf in
  match self with
  | Int i -> fpf out "%Ld" i
  | String s -> fpf out "%S" s
  | List l ->
    fpf out "[@[<hv>";
    List.iteri (fun i v ->
      if i>0 then fpf out ";@ ";
      pp_debug out v) l;
    fpf out "@]]"
  | Map m ->
    fpf out "{@[<hv>";
    let i = ref 0 in
    Str_map.iter (fun k v ->
      if !i>0 then fpf out ";@ ";
      incr i;
      fpf out "@[<1>%S:@ %a@]" k pp_debug v) m;
    fpf out "@]}"

let to_string_debug self = Format.asprintf "%a" pp_debug self

module Encode = struct
  let bpf = Printf.bprintf
  let fpf = Printf.fprintf

  let rec to_buffer (buf:Buffer.t) (self:t) : unit =
    let recurse = to_buffer buf in
    let addc = Buffer.add_char in
    match self with
    | Int i -> bpf buf "i%Lde" i
    | String s -> bpf buf "%d:%s" (String.length s) s
    | List l -> addc buf 'l'; List.iter recurse l; addc buf 'e'
    | Map l ->
      addc buf 'd';
      Str_map.iter (fun k v -> bpf buf "%d:%s%a" (String.length k) k to_buffer v) l;
      addc buf 'e'

  let to_string (self:t) : string =
    let buf = Buffer.create 32 in
    to_buffer buf self;
    Buffer.contents buf

  let rec to_chan (oc:out_channel) (self:t) : unit =
    let recurse = to_chan oc in
    let addc = output_char in
    match self with
    | Int i -> fpf oc "i%Lde" i
    | String s -> fpf oc "%d:%s" (String.length s) s
    | List l -> addc oc 'l'; List.iter recurse l; addc oc 'e'
    | Map l ->
      addc oc 'd';
      Str_map.iter (fun k v -> fpf oc "%d:%s%a" (String.length k) k to_chan v) l;
      addc oc 'e'

  let to_fmt out self =
    Format.pp_print_string out (to_string self)
end

module Decode = struct
  exception Fail

  let of_string s =
    let i = ref 0 in

    let[@inline] check_not_eof() =
      if !i >= String.length s then raise_notrace Fail;
    in

    let rec top () : t =
      check_not_eof ();
      match String.unsafe_get s !i with
      | 'l' ->
        incr i;
        read_list []
      | 'd' ->
        incr i;
        read_map Str_map.empty
      | 'i' -> incr i; let n = read_int 'e' true 0 in int n
      | '0' .. '9' -> String (parse_str_len ())
      | _ -> raise_notrace Fail

    (* read integer until char [stop] is met, consume [stop], return int *)
    and read_int stop sign n : int =
      check_not_eof ();
      match String.unsafe_get s !i with
      | c when c == stop -> incr i; if sign then n else -n
      | '-' when stop == 'e' && sign && n=0 ->
        incr i; read_int stop false n
      | '0' .. '9' as c ->
        incr i; read_int stop sign (Char.code c - Char.code '0' + 10 * n)
      | _ -> raise_notrace Fail

    and parse_str_len () : string =
      let n = read_int ':' true 0 in
      if !i + n > String.length s then raise_notrace Fail;
      let s = String.sub s !i n in
      i := !i + n;
      s

    and read_list acc =
      check_not_eof();
      match String.unsafe_get s !i with
      | 'e' -> incr i; List (List.rev acc)
      | _ -> let x = top() in read_list (x::acc)

    and read_map acc =
      check_not_eof();
      match String.unsafe_get s !i with
      | 'e' -> incr i; Map acc
      | _ ->
        let k = parse_str_len () in
        let v = top() in
        read_map (Str_map.add k v acc)
    in

    try Some (top())
    with Fail -> None

  let of_string_exn s =
    match of_string s with
    | Some x -> x
    | None -> failwith "bencode.decode: invalid string"
end
