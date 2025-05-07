(* adapted from ocaml-protoc from code by c-cube *)

module Byte_slice = CCByte_slice
module Byte_buffer = CCByte_buffer

module Decode = struct
  let skip (sl : Byte_slice.t) off : int =
    let shift = ref 0 in
    let continue = ref true in

    let off = ref off in
    let n_consumed = ref 0 in

    while !continue do
      if sl.len <= 0 then invalid_arg "out of bound";
      incr n_consumed;
      let b = Char.code (Bytes.get sl.bs !off) in
      let cur = b land 0x7f in
      if cur <> b then (
        (* at least one byte follows this one *)
        incr off;
        shift := !shift + 7
      ) else if !shift < 63 || b land 0x7f <= 1 then
        continue := false
      else
        invalid_arg "leb128 varint is too long"
    done;

    !n_consumed

  let u64 (sl : Byte_slice.t) (off : int) : int64 * int =
    let shift = ref 0 in
    let res = ref 0L in
    let continue = ref true in

    let off = ref off in
    let n_consumed = ref 0 in

    while !continue do
      if sl.len <= 0 then invalid_arg "out of bound";
      incr n_consumed;
      let b = Char.code (Bytes.get sl.bs !off) in
      let cur = b land 0x7f in
      if cur <> b then (
        (* at least one byte follows this one *)
        (res := Int64.(logor !res (shift_left (of_int cur) !shift)));
        incr off;
        shift := !shift + 7
      ) else if !shift < 63 || b land 0x7f <= 1 then (
        (res := Int64.(logor !res (shift_left (of_int b) !shift)));
        continue := false
      ) else
        invalid_arg "leb128 varint is too long"
    done;

    !res, !n_consumed

  let[@inline] uint_truncate sl off =
    let v, n_consumed = u64 sl off in
    Int64.to_int v, n_consumed

  let[@inline] decode_zigzag (v : int64) : int64 =
    Int64.(logxor (shift_right v 1) (neg (logand v Int64.one)))

  let[@inline] i64 sl off : int64 * int =
    let v, n_consumed = u64 sl off in
    decode_zigzag v, n_consumed

  let[@inline] int_truncate sl off =
    let v, n_consumed = u64 sl off in
    Int64.to_int (decode_zigzag v), n_consumed
end

module Encode = struct
  let[@inline] encode_zigzag (i : int64) : int64 =
    Int64.(logxor (shift_left i 1) (shift_right i 63))

  external varint_size : (int64[@unboxed]) -> int
    = "caml_cc_leb128_varint_size_byte" "caml_cc_leb128_varint_size"
  [@@noalloc]
  (** Compute how many bytes this int would occupy as varint *)

  external varint_slice : bytes -> (int[@untagged]) -> (int64[@unboxed]) -> unit
    = "caml_cc_leb128_varint_byte" "caml_cc_leb128_varint"
  [@@noalloc]
  (** Write this int as varint into the given slice *)

  let[@inline] u64 (buf : Byte_buffer.t) (i : int64) =
    let n = varint_size i in
    Byte_buffer.ensure_free buf n;
    assert (buf.len + n <= Bytes.length buf.bs);
    varint_slice buf.bs buf.len i;
    buf.len <- buf.len + n

  let[@inline] i64 buf i : unit = u64 buf (encode_zigzag i)
  let[@inline] uint buf i : unit = u64 buf (Int64.of_int i)
  let[@inline] int buf i : unit = u64 buf (encode_zigzag (Int64.of_int i))
end
