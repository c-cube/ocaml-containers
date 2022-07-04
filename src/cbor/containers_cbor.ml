module Fmt = CCFormat

type t =
  [ `Null
  | `Undefined
  | `Simple of int
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `Bytes of string
  | `Text of string
  | `Array of t list
  | `Map of (t * t) list
  | `Tag of int * t ]

let rec pp_diagnostic out (self : t) =
  match self with
  | `Null -> Fmt.string out "null"
  | `Undefined -> Fmt.string out "undefined"
  | `Simple i -> Fmt.fprintf out "simple(%d)" i
  | `Bool b -> Fmt.bool out b
  | `Int i -> Fmt.int out i
  | `Float f -> Fmt.float out f
  | `Bytes b -> Fmt.fprintf out "h'%s'" (CCString.to_hex b)
  | `Text s -> Fmt.fprintf out "%S" s
  | `Array l ->
    Fmt.fprintf out "[@[";
    List.iteri
      (fun i x ->
        if i > 0 then Fmt.fprintf out ",@ ";
        pp_diagnostic out x)
      l;
    Fmt.fprintf out "@]]"
  | `Map l ->
    Fmt.fprintf out "{@[";
    List.iteri
      (fun i (k, v) ->
        if i > 0 then Fmt.fprintf out ",@ ";
        Fmt.fprintf out "@[%a:@ %a@]" pp_diagnostic k pp_diagnostic v)
      l;
    Fmt.fprintf out "@]}"
  | `Tag (i, x) -> Fmt.fprintf out "%d(@[%a@])" i pp_diagnostic x

let to_string_diagnostic (self : t) : string =
  Format.asprintf "@[<h>%a@]" pp_diagnostic self

(* we use funtions from Bytes *)
[@@@ifge 4.08]

exception Indefinite

let decode_exn (s : string) : t =
  let b = Bytes.unsafe_of_string s in
  let i = ref 0 in

  (* currently at end delimiter? *)
  let[@inline] is_break_stop_code () = Char.code s.[!i] = 0b111_11111 in

  let[@inline] read_i8 () =
    let c = Char.code s.[!i] in
    incr i;
    c
  in

  let[@inline] read_i16 () =
    let c = Bytes.get_uint16_be b !i in
    i := !i + 2;
    c
  in

  let[@inline] read_i32 () =
    let c = Bytes.get_int32_be b !i in
    i := !i + 4;
    c
  in

  let[@inline] read_i64 () =
    let c = Bytes.get_int64_be b !i in
    i := !i + 8;
    c
  in

  let reserve_n n =
    let j = !i in
    if j + n > String.length s then failwith "cbor: cannot extract slice";
    i := !i + n;
    j
  in

  let[@inline] i64_to_int i =
    let j = Int64.to_int i in
    if Int64.(of_int j = i) then
      j
    else
      failwith "int64 does not fit in int"
  in

  (* read integer value from least significant bits *)
  let read_int ~allow_indefinite low =
    match low with
    | _ when low < 0 -> failwith "cbor: invalid length"
    | _ when low < 24 -> Int64.of_int low
    | 24 -> Int64.of_int (read_i8 ())
    | 25 -> Int64.of_int (read_i16 ())
    | 26 -> Int64.of_int32 (read_i32 ())
    | 27 -> read_i64 ()
    | 28 | 29 | 30 -> failwith "cbor: invalid length"
    | 31 ->
      if allow_indefinite then
        raise_notrace Indefinite
      else
        failwith "cbor: invalid integer 31 in this context"
    | _ -> assert false
  in

  (* appendix D

     double decode_half(unsigned char *halfp) {
       unsigned half = (halfp[0] << 8) + halfp[1];
       unsigned exp = (half >> 10) & 0x1f;
       unsigned mant = half & 0x3ff;
       double val;
       if (exp == 0) val = ldexp(mant, -24);
       else if (exp != 31) val = ldexp(mant + 1024, exp - 25);
       else val = mant == 0 ? INFINITY : NAN;
       return half & 0x8000 ? -val : val;
     }
  *)
  let decode_f16 (half : int) : float =
    (* exponent is bits 15:10 *)
    let exp = (half lsr 10) land 0x1f in
    (* mantissa is bits 9:0 *)
    let mant = half land 0x3ff in
    let value =
      if exp = 0 then
        ldexp (float mant) (-24)
      else if exp <> 31 then
        ldexp (float (mant + 1024)) (exp - 25)
      else if mant = 0 then
        infinity
      else
        nan
    in
    if half land 0x8000 <> 0 then
      -.value
    else
      value
  in

  (* roughly follow https://www.rfc-editor.org/rfc/rfc8949.html#pseudocode *)
  let rec read_value () =
    let c = read_i8 () in
    let high = (c land 0b111_00000) lsr 5 in
    let low = c land 0b000_11111 in
    match high with
    | 0 -> `Int (read_int ~allow_indefinite:false low |> i64_to_int)
    | 1 ->
      let i = read_int ~allow_indefinite:false low |> i64_to_int in
      `Int (-1 - i)
    | 2 ->
      let s = read_bytes ~ty:`Bytes low in
      `Bytes s
    | 3 ->
      let s = read_bytes ~ty:`String low in
      `Text s
    | 4 ->
      let l =
        match read_int ~allow_indefinite:true low |> i64_to_int with
        | len -> List.init len (fun _ -> read_value ())
        | exception Indefinite ->
          let l = ref [] in
          while not (is_break_stop_code ()) do
            l := read_value () :: !l
          done;
          incr i;
          (* consume stop code *)
          List.rev !l
      in
      `Array l
    | 5 ->
      let l =
        match read_int ~allow_indefinite:true low |> i64_to_int with
        | len -> List.init len (fun _ -> read_pair ())
        | exception Indefinite ->
          let l = ref [] in
          while not (is_break_stop_code ()) do
            l := read_pair () :: !l
          done;
          incr i;
          (* consume stop code *)
          List.rev !l
      in
      `Map l
    | 6 ->
      let tag = read_int ~allow_indefinite:false low |> i64_to_int in
      let v = read_value () in
      `Tag (tag, v)
    | 7 ->
      (* simple or float,
         https://www.rfc-editor.org/rfc/rfc8949.html#fpnocont *)
      let i = read_int ~allow_indefinite:false low in
      (match low with
      | 20 -> `Bool false
      | 21 -> `Bool true
      | 22 -> `Null
      | 23 -> `Undefined
      | _ when low <= 24 -> `Simple (i64_to_int i)
      | 25 ->
        (* float16 *)
        `Float (decode_f16 (Int64.to_int i))
      | 26 ->
        (* float 32 *)
        `Float (Int32.float_of_bits (Int64.to_int32 i))
      | 27 ->
        (* float 64 *)
        `Float (Int64.float_of_bits i)
      | 28 | 29 | 30 -> failwith "cbor: malformed"
      | 31 -> failwith "uncaught 'break' stop code"
      | _ -> assert false (* unreachable *))
    | _ -> assert false
  (* unreachable *)
  and read_bytes ~ty low =
    match read_int ~allow_indefinite:true low |> i64_to_int with
    | exception Indefinite ->
      let buf = Buffer.create 32 in
      while not (is_break_stop_code ()) do
        match read_value (), ty with
        | `Text s, `String | `Bytes s, `Bytes -> Buffer.add_string buf s
        | _ -> failwith "cbor: invalid chunk in indefinite length string/byte"
      done;
      incr i;
      (* consume stop code *)
      Buffer.contents buf
    | len ->
      let off = reserve_n len in
      String.sub s off len
  and read_pair () =
    let k = read_value () in
    let v = read_value () in
    k, v
  in
  read_value ()

let decode s = try Ok (decode_exn s) with Failure s -> Error s

let encode ?(buf = Buffer.create 32) (self : t) : string =
  Buffer.clear buf;

  let[@inline] add_byte (high : int) (low : int) =
    let i = (high lsl 5) lor low in
    assert (i land 0xff == i);
    Buffer.add_char buf (Char.unsafe_chr i)
  in

  let add_i64 (i : int64) = Buffer.add_int64_be buf i in

  (* add unsigned integer, including first tag byte *)
  let add_uint (high : int) (x : int) =
    assert (x >= 0);
    if x < 24 then
      add_byte high x
    else if x <= 0xff then (
      add_byte high 24;
      Buffer.add_char buf (Char.unsafe_chr x)
    ) else if x <= 0xff_ff then (
      add_byte high 25;
      Buffer.add_uint16_be buf x
    ) else if x <= 0xff_ff_ff_ff then (
      add_byte high 26;
      Buffer.add_int32_be buf (Int32.of_int x)
    ) else (
      add_byte high 27;
      Buffer.add_int64_be buf (Int64.of_int x)
    )
  in

  let rec encode_val (self : t) : unit =
    match self with
    | `Bool false -> add_byte 7 20
    | `Bool true -> add_byte 7 21
    | `Null -> add_byte 7 22
    | `Undefined -> add_byte 7 23
    | `Simple i ->
      if i < 24 then
        add_byte 7 i
      else if i <= 0xff then (
        add_byte 7 24;
        Buffer.add_char buf (Char.unsafe_chr i)
      ) else
        failwith "cbor: simple value too high (above 255)"
    | `Float f ->
      add_byte 7 27;
      (* float 64 *)
      add_i64 (Int64.bits_of_float f)
    | `Array l ->
      add_uint 4 (List.length l);
      List.iter encode_val l
    | `Map l ->
      add_uint 5 (List.length l);
      List.iter
        (fun (k, v) ->
          encode_val k;
          encode_val v)
        l
    | `Text s ->
      add_uint 3 (String.length s);
      Buffer.add_string buf s
    | `Bytes s ->
      add_uint 2 (String.length s);
      Buffer.add_string buf s
    | `Tag (t, v) ->
      add_uint 6 t;
      encode_val v
    | `Int i ->
      if i >= 0 then
        add_uint 0 i
      else if min_int + 2 > i then (
        (* large negative int, be careful. encode [(-i)-1] via int64. *)
        add_byte 1 27;
        Buffer.add_int64_be buf Int64.(neg (add 1L (of_int i)))
      ) else
        add_uint 1 (-i - 1)
  in
  encode_val self;
  Buffer.contents buf

[@@@endif]
