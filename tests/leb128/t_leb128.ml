include (val Containers_testlib.make ~__FILE__ ())
module Leb128 = Containers_leb128
module Bb = CCByte_buffer
module Bs = CCByte_slice

let encode_decode_u64 (i : int64) : bool =
  let buf = Bb.create () in
  Leb128.Encode.u64 buf i;
  let slice = Bb.to_slice buf in
  let i', n = Leb128.Decode.u64 slice 0 in
  Int64.equal i i' && n = Bs.len slice

let encode_decode_i64 (i : int64) : bool =
  let buf = Bb.create () in
  Leb128.Encode.i64 buf i;
  let slice = Bb.to_slice buf in
  let i', n = Leb128.Decode.i64 slice 0 in
  Int64.equal i i' && n = Bs.len slice

let encode_decode_uint (i : int) : bool =
  i >= 0
  &&
  let buf = Bb.create () in
  Leb128.Encode.uint buf i;
  let slice = Bb.to_slice buf in
  let i', n = Leb128.Decode.uint_truncate slice 0 in
  Int.equal i i' && n = Bs.len slice

let encode_decode_int (i : int) : bool =
  let buf = Bb.create () in
  Leb128.Encode.int buf i;
  let slice = Bb.to_slice buf in
  let i', n = Leb128.Decode.int_truncate slice 0 in
  Int.equal i i' && n = Bs.len slice

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
let i = abs i in
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
let buf = Bb.create () in
Leb128.Encode.u64 buf 0L;
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 0L v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.u64 buf 127L;
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 127L v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.u64 buf 128L;
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 128L v;
assert_equal ~printer:string_of_int 2 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.u64 buf 16383L;
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 16383L v;
assert_equal ~printer:string_of_int 2 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.u64 buf 16384L;
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.u64 slice 0 in
assert_equal ~printer:Int64.to_string 16384L v;
assert_equal ~printer:string_of_int 3 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.i64 buf 0L;
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string 0L v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.i64 buf (-1L);
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string (-1L) v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.i64 buf 63L;
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string 63L v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.i64 buf (-64L);
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string (-64L) v;
assert_equal ~printer:string_of_int 1 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.i64 buf 64L;
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string 64L v;
assert_equal ~printer:string_of_int 2 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.i64 buf (-65L);
let slice = Bb.to_slice buf in
let v, n = Leb128.Decode.i64 slice 0 in
assert_equal ~printer:Int64.to_string (-65L) v;
assert_equal ~printer:string_of_int 2 n;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.i64 buf 0L;
let slice = Bb.to_slice buf in
let skip = Leb128.Decode.skip slice 0 in
assert_equal ~printer:string_of_int 1 skip;
true
;;

t @@ fun () ->
let buf = Bb.create () in
Leb128.Encode.u64 buf 300L;
Leb128.Encode.u64 buf 500L;
let slice = Bb.to_slice buf in
let v1, n1 = Leb128.Decode.u64 slice 0 in
let v2, n2 = Leb128.Decode.u64 slice n1 in
assert_equal ~printer:Int64.to_string 300L v1;
assert_equal ~printer:Int64.to_string 500L v2;
assert_equal ~printer:string_of_int 2 n1;
assert_equal ~printer:string_of_int 2 n2;
true
