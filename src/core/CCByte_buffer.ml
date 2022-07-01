type 'a iter = ('a -> unit) -> unit

type t = {
  mutable bytes: bytes;
  mutable sz: int;
}

let create ?(cap=0) () : t =
  let bytes =
    if cap=0 then Bytes.unsafe_of_string "" else Bytes.create cap in
  { sz=0; bytes }

let[@inline] capacity self : int = Bytes.length self.bytes
let[@inline] bytes self = self.bytes
let[@inline] length self = self.sz

let[@inline] is_empty self = self.sz = 0
let[@inline] clear self = self.sz <- 0

let grow_cap_ self =
  min Sys.max_string_length
    (let n = capacity self in n + n lsl 1 + 5)

let grow_to_ self newcap =
  if newcap = capacity self then (
    invalid_arg "byte_buf: cannot grow further";
  );
  let newbytes = Bytes.create newcap in
  Bytes.blit self.bytes 0 newbytes 0 self.sz;
  self.bytes <- newbytes

let[@inline never] grow_ self =
  let newcap = grow_cap_ self in
  grow_to_ self newcap

let ensure_cap self n =
  if n>capacity self then (
    let newcap = max n (grow_cap_ self) in
    grow_to_ self newcap
  )

let shrink_to self n =
  if self.sz > n then self.sz <- n

let append_buf (self:t) buf : unit =
  let n = Buffer.length buf in
  ensure_cap self (length self + n);
  Buffer.blit buf 0 self.bytes self.sz n;
  self.sz <- self.sz + n

let append_subbytes self b off len =
  ensure_cap self (length self + len);
  Bytes.blit b off self.bytes self.sz len;
  self.sz <- self.sz + len

let append_bytes self b = append_subbytes self b 0 (Bytes.length b)
let append_string self s = append_bytes self (Bytes.unsafe_of_string s)
let append_substring self s off len = append_subbytes self (Bytes.unsafe_of_string s) off len

let[@inline] add_char_unsafe_ self c =
  Bytes.unsafe_set self.bytes self.sz c;
  self.sz <- self.sz + 1

let[@inline] add_char self c =
  if self.sz = capacity self then grow_ self;
  add_char_unsafe_ self c

let[@inline] unsafe_get self i = Bytes.unsafe_get self.bytes i
let[@inline] unsafe_set self i c = Bytes.unsafe_set self.bytes i c

let[@inline] get self i =
  if i < 0 || i >= self.sz then invalid_arg "Byte_buf.get";
  unsafe_get self i

let[@inline] set self i c =
  if i < 0 || i >= self.sz then invalid_arg "Byte_buf.set";
  unsafe_set self i c

let[@inline] contents self = Bytes.sub_string self.bytes 0 self.sz
let[@inline] contents_bytes self = Bytes.sub self.bytes 0 self.sz

let[@inline] append_iter self i = i (add_char self)
let[@inline] append_seq self seq = Seq.iter (add_char self) seq

let fold_left f acc self =
  let {bytes; sz} = self in (* capture current content *)

  let acc = ref acc in
  for i=0 to sz do
    acc := f !acc (Bytes.unsafe_get bytes i)
  done;
  !acc

let iter f self =
  let {bytes; sz} = self in (* capture current content *)
  for i=0 to sz do
    f (Bytes.unsafe_get bytes i)
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
  let {bytes;sz} = self in
  let rec s i () =
    if i= sz then Seq.Nil
    else Seq.Cons (Bytes.unsafe_get bytes i, s (i+1))
  in
  s 0

(* TODO: unicode operators.*)
