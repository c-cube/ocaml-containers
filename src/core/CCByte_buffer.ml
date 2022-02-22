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

(*$T
    (let b = create() in is_empty b)
    (let b = create ~cap:32 () in is_empty b)
    (let b = create() in length b = 0)
    (let b = create ~cap:32 () in length b = 0)
*)

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

[@@@ifge 4.8]

let add_int16_le self (i:int) : unit =
  ensure_cap self (length self+2);
  Bytes.set_int16_le self.bytes self.sz i;
  self.sz <- self.sz + 2

let add_int16_be self (i:int) : unit =
  ensure_cap self (length self+2);
  Bytes.set_int16_be self.bytes self.sz i;
  self.sz <- self.sz + 2

let add_int16_ne self (i:int) : unit =
  ensure_cap self (length self+2);
  Bytes.set_int16_ne self.bytes self.sz i;
  self.sz <- self.sz + 2

let add_int32_le self (i:int32) : unit =
  ensure_cap self (length self+4);
  Bytes.set_int32_le self.bytes self.sz i;
  self.sz <- self.sz + 4

let add_int32_be self (i:int32) : unit =
  ensure_cap self (length self+4);
  Bytes.set_int32_be self.bytes self.sz i;
  self.sz <- self.sz + 4

let add_int32_ne self (i:int32) : unit =
  ensure_cap self (length self+4);
  Bytes.set_int32_ne self.bytes self.sz i;
  self.sz <- self.sz + 4

let add_int64_le self (i:int64) : unit =
  ensure_cap self (length self+8);
  Bytes.set_int64_le self.bytes self.sz i;
  self.sz <- self.sz + 8

let add_int64_be self (i:int64) : unit =
  ensure_cap self (length self+8);
  Bytes.set_int64_be self.bytes self.sz i;
  self.sz <- self.sz + 8

let add_int64_ne self (i:int64) : unit =
  ensure_cap self (length self+8);
  Bytes.set_int64_ne self.bytes self.sz i;
  self.sz <- self.sz + 8


[@@@endif]


(* TODO: unicode operators.*)

(*$inject
  let test_count = 2_500

  open QCheck

  type op =
    | Add_char of char
    | Add_string of string
    | Get_contents
    | Get of int
    | Clear
    | Shrink_to of int
    | Set of int * char
    | Add_int16_le of int
    | Add_int16_be of int
    | Add_int16_ge of int
    | Add_int32_le of int32
    | Add_int32_be of int32
    | Add_int32_ge of int32
    | Add_int64_le of int64
    | Add_int64_be of int64
    | Add_int64_ge of int64

  let spf = Printf.sprintf

  let str_op = function
    | Add_char c -> spf "add_char %C" c
    | Add_string s -> spf "add_string %S" s
    | Get_contents -> "contents"
    | Get i -> spf "get %d" i
    | Clear -> "clear"
    | Shrink_to n -> spf "shrink %d" n
    | Set (i,c) -> spf "set %d %C" i c
    | Add_int16_ne i -> spf "add_int16_ne %d" i
    | Add_int16_ge i -> spf "add_int16_ge %d" i
    | Add_int16_le i -> spf "add_int16_le %d" i
    | Add_int32_ne i -> spf "add_int32_ne %ld" i
    | Add_int32_ge i -> spf "add_int32_ge %ld" i
    | Add_int32_le i -> spf "add_int32_le %ld" i
    | Add_int64_ne i -> spf "add_int64_ne %Ld" i
    | Add_int64_ge i -> spf "add_int64_ge %Ld" i
    | Add_int64_le i -> spf "add_int64_le %Ld" i

  let gen_op size : (_*_) Gen.t =
    let open Gen in
    let int16 = (0 -- (1 lsl 16)-1) in
    let int32 = int32 and int64 = int64 in
    let base = if size>0 then
        [1, ((0--size) >|= fun x -> Get x, size);
         1, ((0--size) >>= fun x -> printable >|= fun c -> Set (x,c), size);
         1, ((0--size) >|= fun x -> Shrink_to x, x);
         1, (int16 >|= fun x -> Add_int16_ge x);
         1, (int16 >|= fun x -> Add_int16_le x);
         1, (int16 >|= fun x -> Add_int16_ne x);
         1, (int32 >|= fun x -> Add_int32_ge x);
         1, (int32 >|= fun x -> Add_int32_le x);
         1, (int32 >|= fun x -> Add_int32_ne x);
         1, (int64 >|= fun x -> Add_int64_ge x);
         1, (int64 >|= fun x -> Add_int64_le x);
         1, (int64 >|= fun x -> Add_int64_ne x);
        ]
      else []
    in
    frequency (base @ [
        1, return (Get_contents, size);
        1, return (Clear, 0);
        3, (printable >|= fun c -> Add_char c, size+1);
        1, (string_size (0 -- 100) ~gen:printable >|= fun s ->
            Add_string s, size+String.length s);
    ])

  let rec gen_l acc sz n =
    let open Gen in
    if n=0 then return (List.rev acc)
    else (
      gen_op sz >>= fun (op, sz) ->
      gen_l (op::acc) sz (n-1)
    )

  let gen : op list Gen.t = Gen.sized (gen_l [] 0)

  let is_valid ops =
    let rec loop sz = function
      | [] ->  true
      | Add_char _ :: tl -> loop (sz+1) tl
      | Clear :: tl -> loop 0 tl
      | Add_string s :: tl -> loop (sz+String.length s) tl
      | (Get n | Set (n,_)) :: tl -> n < sz && loop sz tl
      | Get_contents :: tl -> loop sz tl
      | Shrink_to x :: tl -> x <= sz && loop x tl
      | Add_int16_ne _
      | Add_int16_ge _
      | Add_int16_le _ -> loop (sz+2) tl
      | Add_int32_ne _
      | Add_int32_ge _
      | Add_int32_le _ -> loop (sz+4) tl
      | Add_int64_ne _
      | Add_int64_ge _
      | Add_int64_le _ -> loop (sz+8) tl
    in loop 0 ops

  let shrink_op = Iter.(function
    | Get_contents | Clear -> empty
    | Get n -> Shrink.int n >|= fun n->Get n
    | Add_char c -> Shrink.char c >|= fun c -> Add_char c
    | Add_string s -> Shrink.string s >|= fun s -> Add_string s
    | Shrink_to n -> Shrink.int n >|= fun n -> Shrink_to n
    | Set (n,c) ->
      (Shrink.int n >|= fun n-> Set(n,c)) <+>
      (Shrink.char c >|= fun c-> Set(n,c))
    | Add_int16_ne _
    | Add_int16_ge _
    | Add_int16_le _
    | Add_int32_ne _
    | Add_int32_ge _
    | Add_int32_le _
    | Add_int64_ne _
    | Add_int64_ge _
    | Add_int64_le _ -> empty
    )

  let arb = make gen ~print:(Print.list str_op)
      ~shrink:Shrink.(filter is_valid @@ list ~shrink:shrink_op)

  exception Nope of string

  let b2str n f x =
    let b = Bytes.create n in
    f b 0 x;
    Bytes.unsafe_to_string b

  let prop_consistent ops =
    let buf = ref "" in
    let b = create ~cap:32 () in

    let run_op op =
      match op with
      | Get i ->
        assert (String.length !buf = length b);
        let c1 = (!buf).[i] in
        let c2 = get b i in
        if c1<>c2 then raise (Nope (spf "c1=%C, c2=%C" c1 c2))

      | Get_contents ->
        let s1 = !buf in
        let s2 = contents b in
        if s1<>s2 then raise (Nope (spf "s1=%S, s2=%S" s1 s2))

      | Add_char c -> buf := !buf ^ String.make 1 c; add_char b c
      | Add_string s -> buf := !buf ^ s; append_string b s
      | Clear -> buf := ""; clear b
      | Shrink_to n -> buf := String.sub !buf 0 n; shrink_to b n
      | Set (n,c) ->
        (
          let b' = Bytes.of_string !buf in
          Bytes.set b' n c;
          buf := Bytes.unsafe_to_string b';
        );
        set b n c

      | Add_int16_ne i -> buf := !buf ^ b2str 2 Bytes.set_int16_le i; add_int16_ne b i
      | Add_int16_ge i -> buf := !buf ^ b2str 2 Bytes.set_int16_ge i; add_int16_ge b i
      | Add_int16_le i -> buf := !buf ^ b2str 2 Bytes.set_int16_le i; add_int16_le b i
      | Add_int32_ne i -> buf := !buf ^ b2str 4 Bytes.set_int32_le i; add_int32_ne b i
      | Add_int32_ge i -> buf := !buf ^ b2str 4 Bytes.set_int32_le i; add_int32_ge b i
      | Add_int32_le i -> buf := !buf ^ b2str 4 Bytes.set_int32_le i; add_int32_le b i
      | Add_int64_ne i -> buf := !buf ^ b2str 8 Bytes.set_int64_le i; add_int64_ne b i
      | Add_int64_ge i -> buf := !buf ^ b2str 8 Bytes.set_int64_le i; add_int64_ge b i
      | Add_int64_le i -> buf := !buf ^ b2str 8 Bytes.set_int64_le i; add_int64_le b i

    in

    assume (is_valid ops);
    try List.iter run_op ops; true
    with Nope str ->
      Test.fail_reportf "consistent ops failed:\n%s" str
*)

(*$Q
    arb (fun ops -> prop_consistent ops)
    *)
