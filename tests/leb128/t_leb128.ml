include (val Containers_testlib.make ~__FILE__ ())
module Leb128 = Containers_leb128
module Buf = CCByte_buffer
module Slice = CCByte_slice

let encode_decode_u64 (i : int64) : bool =
  let buf = Buf.create () in
  Leb128.Encode.u64 buf i;
  let slice = Buf.to_slice buf in
  let i', n = Leb128.Decode.u64 slice 0 in
  Int64.equal i i' && n = Slice.len slice

let encode_decode_i64 (i : int64) : bool =
  let buf = Buf.create () in
  Leb128.Encode.i64 buf i;
  let slice = Buf.to_slice buf in
  let i', n = Leb128.Decode.i64 slice 0 in
  Int64.equal i i' && n = Slice.len slice

let encode_decode_uint (i : int) : bool =
  i >= 0
  &&
  let buf = Buf.create () in
  Leb128.Encode.uint buf i;
  let slice = Buf.to_slice buf in
  let i', n = Leb128.Decode.uint_truncate slice 0 in
  Int.equal i i' && n = Slice.len slice

let encode_decode_int (i : int) : bool =
  let buf = Buf.create () in
  Leb128.Encode.int buf i;
  let slice = Buf.to_slice buf in
  let i', n = Leb128.Decode.int_truncate slice 0 in
  Int.equal i i' && n = Slice.len slice

let zigzag_roundtrip (i : int64) : bool =
  let encoded = Leb128.Encode.encode_zigzag i in
  let decoded = Leb128.Decode.decode_zigzag encoded in
  Int64.equal i decoded
;;

q ~count:10_000 ~long_factor:20 Q.int64 @@ fun i ->
if not (encode_decode_u64 (Int64.abs i)) then
  Q.Test.fail_reportf "u64 roundtrip failed for %Ld" i;
true
;;

q ~count:10_000 ~long_factor:20 Q.int64 @@ fun i ->
if not (encode_decode_i64 i) then
  Q.Test.fail_reportf "i64 roundtrip failed for %Ld" i;
true
;;

q ~count:10_000 ~long_factor:20 Q.int @@ fun i ->
(* make sure [i] is non negative *)
let i = max 0 (abs i) in
if not (encode_decode_uint i) then
  Q.Test.fail_reportf "uint roundtrip failed for %d" i;
true
;;

q ~count:10_000 ~long_factor:20 Q.int @@ fun i ->
if not (encode_decode_int i) then
  Q.Test.fail_reportf "int roundtrip failed for %d" i;
true
;;

q ~count:10_000 ~long_factor:20 Q.int64 @@ fun i ->
if not (zigzag_roundtrip i) then
  Q.Test.fail_reportf "zigzag roundtrip failed for %Ld" i;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.u64 buf 0L;
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 0L v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.u64 buf 127L;
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 127L v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.u64 buf 128L;
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 128L v;
assert_equal ~printer:string_of_int 2 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.u64 buf 16383L;
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 16383L v;
assert_equal ~printer:string_of_int 2 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.u64 buf 16384L;
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 16384L v;
assert_equal ~printer:string_of_int 3 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.i64 buf 0L;
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string 0L v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.u64 buf 127L;
Leb128.Encode.u64 buf 16384L;
Leb128.Encode.u64 buf 300L;
let slice = Buf.to_slice buf in
let n1 = Leb128.Decode.skip slice 0 in
let n2 = Leb128.Decode.skip slice n1 in
let n3 = Leb128.Decode.skip slice (n1 + n2) in
assert_equal ~printer:string_of_int 1 n1;
assert_equal ~printer:string_of_int 3 n2;
assert_equal ~printer:string_of_int 2 n3;
assert_equal ~printer:string_of_int 6 (n1 + n2 + n3);
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.i64 buf (-1L);
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string (-1L) v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.i64 buf 63L;
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string 63L v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.i64 buf (-64L);
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string (-64L) v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.i64 buf 64L;
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string 64L v;
assert_equal ~printer:string_of_int 2 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.i64 buf (-65L);
let slice = Buf.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string (-65L) v;
assert_equal ~printer:string_of_int 2 n;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.i64 buf Int64.min_int;
let slice = Buf.to_slice buf in
let v, _n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string Int64.min_int v;
true
;;

let buf = Buf.create () in
Leb128.Encode.i64 buf Int64.max_int;
let slice = Buf.to_slice buf in
let v, _n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string Int64.max_int v;
true
;;

t @@ fun () ->
let buf = Buf.create () in
Leb128.Encode.u64 buf 300L;
Leb128.Encode.u64 buf 500L;
let slice = Buf.to_slice buf in
let v1, n1 = Leb128.Decode.u64 slice 0 in
let v2, n2 = Leb128.Decode.u64 slice n1 in
assert_equal ~printer:Int64.to_string 300L v1;
assert_equal ~printer:Int64.to_string 500L v2;
assert_equal ~printer:string_of_int 2 n1;
assert_equal ~printer:string_of_int 2 n2;
true
;;

t @@ fun () ->
(* Test decoding from a slice with non-zero offset *)
let bytes = Bytes.of_string "\x00\x00\x54\x00" in
let slice = Slice.create ~off:2 ~len:1 bytes in
assert_equal
  ~printer:(fun c -> Printf.sprintf "0x%02x" (Char.code c))
  '\x54' (Slice.get slice 0);
let v, n = Leb128.Decode.int_truncate slice 0 in
assert_equal ~printer:string_of_int 42 v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
(* Test decoding u64 from a slice with non-zero offset *)
let bytes = Bytes.of_string "\xFF\xFF\x2A\x00" in
let slice = Slice.create ~off:2 ~len:1 bytes in
assert_equal
  ~printer:(fun c -> Printf.sprintf "0x%02x" (Char.code c))
  '\x2A' (Slice.get slice 0);
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 42L v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
(* Test decoding from a sub-slice *)
let buf = Buf.create () in
Buf.append_string buf "padding";
Leb128.Encode.int buf 42;
let slice = Buf.to_slice buf in
let sub_slice = Slice.sub slice 7 (Slice.len slice - 7) in
let v, n = Leb128.Decode.int_truncate sub_slice 0 in
assert_equal ~printer:string_of_int 42 v;
assert_equal ~printer:string_of_int 1 n;
true

let () = Containers_testlib.run_all ~descr:"test leb128" [ get () ]
