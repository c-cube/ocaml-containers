type 'a iter = ('a -> unit) -> unit

type t = {
  mutable bs: bytes;
  mutable len: int;
}

let create ?(cap = 0) () : t =
  let bs =
    if cap = 0 then
      Bytes.unsafe_of_string ""
    else
      Bytes.create cap
  in
  { len = 0; bs }

let[@inline] capacity self : int = Bytes.length self.bs
let[@inline] bytes self = self.bs
let[@inline] length self = self.len
let[@inline] is_empty self = self.len = 0
let[@inline] clear self = self.len <- 0

let grow_cap_ self =
  min Sys.max_string_length
    (let n = capacity self in
     n + (n lsl 1) + 5)

let grow_to_ self newcap =
  if newcap = capacity self then invalid_arg "byte_buf: cannot grow further";
  let newbytes = Bytes.create newcap in
  Bytes.blit self.bs 0 newbytes 0 self.len;
  self.bs <- newbytes

let[@inline never] grow_ self =
  let newcap = grow_cap_ self in
  grow_to_ self newcap

let[@inline never] ensure_cap_grow_ self n =
  (* new capacity, make sure it's at least [grow_cap_] so
     that repeated calls to [ensure_cap] have the amortized complexity *)
  let newcap = max n (grow_cap_ self) in
  grow_to_ self newcap

let[@inline] ensure_cap self n =
  if n > capacity self then ensure_cap_grow_ self n

let[@inline] ensure_free self n =
  if n > capacity self - self.len then ensure_cap_grow_ self (self.len + n)

let[@inline] shrink_to self n = if self.len > n then self.len <- n

let append_buf (self : t) buf : unit =
  let n = Buffer.length buf in
  ensure_cap self (length self + n);
  Buffer.blit buf 0 self.bs self.len n;
  self.len <- self.len + n

let append_subbytes self b off len =
  ensure_cap self (length self + len);
  Bytes.blit b off self.bs self.len len;
  self.len <- self.len + len

let[@inline] append_bytes self b = append_subbytes self b 0 (Bytes.length b)
let[@inline] append_string self s = append_bytes self (Bytes.unsafe_of_string s)

let[@inline] append_substring self s off len =
  append_subbytes self (Bytes.unsafe_of_string s) off len

let[@inline] add_char_unsafe_ self c =
  Bytes.unsafe_set self.bs self.len c;
  self.len <- self.len + 1

let[@inline] add_char self c =
  if self.len = capacity self then grow_ self;
  add_char_unsafe_ self c

let[@inline] unsafe_get self i = Bytes.unsafe_get self.bs i
let[@inline] unsafe_set self i c = Bytes.unsafe_set self.bs i c

let[@inline] get self i =
  if i < 0 || i >= self.len then invalid_arg "Byte_buf.get";
  unsafe_get self i

let[@inline] set self i c =
  if i < 0 || i >= self.len then invalid_arg "Byte_buf.set";
  unsafe_set self i c

let[@inline] contents self = Bytes.sub_string self.bs 0 self.len
let[@inline] contents_bytes self = Bytes.sub self.bs 0 self.len
let[@inline] append_iter self i = i (add_char self)
let[@inline] append_seq self seq = Seq.iter (add_char self) seq
let[@inline] to_slice self = CCByte_slice.create ~len:self.len self.bs

let fold_left f acc self =
  let { bs; len } = self in

  (* capture current content *)
  let acc = ref acc in
  for i = 0 to len do
    acc := f !acc (Bytes.unsafe_get bs i)
  done;
  !acc

let[@inline] iter f self =
  (* capture current content *)
  let { bs; len } = self in
  for i = 0 to len do
    f (Bytes.unsafe_get bs i)
  done

let[@inline] iteri f self =
  let { bs; len } = self in
  for i = 0 to len do
    f i (Bytes.unsafe_get bs i)
  done

let of_seq seq =
  let self = create ~cap:32 () in
  append_seq self seq;
  self

let of_iter iter =
  let self = create ~cap:32 () in
  append_iter self iter;
  self

let to_iter self yield = iter yield self

let to_seq self =
  let { bs; len } = self in
  let rec s i () =
    if i = len then
      Seq.Nil
    else
      Seq.Cons (Bytes.unsafe_get bs i, s (i + 1))
  in
  s 0

(* TODO: unicode operators.*)
