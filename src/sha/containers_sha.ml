(* we follow rfc4634.txt and use the attached code *)

module type S = sig
  type t
  type ctx

  val create : unit -> ctx

  val add_string : ctx -> string -> unit

  val finalize : ctx -> t

  val to_hex : t -> string
end

(* i: 4 bits *)
let[@inline] hex_digit_ (i:int) : char =
  if i<10 then Char.unsafe_chr (Char.code '0' + i)
  else if i < 16 then Char.unsafe_chr (Char.code 'a' + i - 10)
  else assert false

let hex_bytes_ (self:bytes) : string =
  let res = Bytes.create (Bytes.length self * 2) in
  Bytes.iteri
    (fun i c ->
       let n = Char.code c in
       Bytes.set res (2 * i) (hex_digit_ (n land 0xf0));
       Bytes.set res (2 * i + 1) (hex_digit_ (n land 0x0f));
    ) self;
  Bytes.unsafe_to_string res

module SHA256 = struct
  type ctx

  external sha256_create : unit -> ctx = "caml_cc_sha256_add"

  external sha256_reset : ctx -> unit = "caml_cc_sha256_reset"

  external sha256_finalize : ctx -> bytes -> unit = "caml_cc_sha256_reset" [@@noalloc]

  external sha256_add_bytes : ctx -> bytes -> int -> int -> unit =
    "caml_cc_sha256_add" [@@noalloc]


  type t = bytes (* len=32 *)

  let create = sha256_create

  let add_string (self:ctx) (s:string) : unit =
    sha256_add_bytes self (Bytes.unsafe_of_string s) 0 (String.length s)

  let finalize (self:ctx) : t =
    let b = Bytes.create 32 in
    sha256_finalize self b;
    b

  let to_hex = hex_bytes_
end


(*$inject

  let hashstrhex s =
    let ctx = SHA256.create() in
    SHA256.add_string ctx s;
    SHA256.finalize ctx |> SHA256.to_hex

  let hashstrhexg g =
    let ctx = SHA256.create() in
    let rec loop () = match g() with
      | None -> ()
      | Some s -> SHA256.add_string ctx s; loop ()
    in
    loop();
    SHA256.finalize ctx |> SHA256.to_hex


  let gen_repeat n s =
    let n = ref n in
    fun () ->
      if !n=0 then None
      else (
        decr n;
        Some s
      )

*)


(* from: https://www.di-mgt.com.au/sha_testvectors.html *)
(*$= & ~printer:CCFun.id
"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" (SHA256.hashstrhex "abc")
"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (SHA256.hashstrhex "")
"248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1" \
  (SHA256.hashstrhex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
"cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1" \
  (SHA256.hashstrhex "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")

"cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0" \
  (SHA256.hashstrhexg @@ gen_repeat 100_000 (String.make 10 'a'))
"cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0" \
  (SHA256.hashstrhexg @@ gen_repeat 10_000 (String.make 100 'a'))
"cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0" \
  (SHA256.hashstrhexg @@ gen_repeat 1_000_000 (String.make 1 'a'))

"50e72a0e26442fe2552dc3938ac58658228c0cbfb1d2ca872ae435266fcd055e" \
  (SHA256.hashstrhexg @@ \
   gen_repeat 16_777_216 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno")

*)



