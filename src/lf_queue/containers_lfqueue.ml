(* atomics *)

module A = CCAtomic

type 'a node = {
  value: 'a;
  next: 'a node option A.t;
}

type 'a t = {
  head: 'a node A.t;
  tail: 'a node A.t;
  dummy: 'a;
}

let create ~dummy () : _ t =
  let ptr0 = {value=dummy;next=A.make None} in
  { head=A.make ptr0;
    tail=A.make ptr0;
    dummy;
  }

let push (self:_ t) x : unit =
  (* new node to insert at the back *)
  let q = {value=x; next=A.make None} in

  let ok = ref false in
  while not !ok do
    let p = A.get self.tail in
    ok := A.compare_and_set p.next None (Some q);
    if not !ok then (
      (* try to ensure progress if another thread takes too long to update [tail] *)
      begin match A.get p.next with
        | None -> ()
        | Some p_next ->
          ignore (A.compare_and_set self.tail p p_next : bool)
      end;
    );
  done

(* try to pop an element already in the queue *)
let pop_nonblock self : _ option =
  let res = ref None in

  let continue = ref true in
  while !continue do
    let p = A.get self.head in
    match A.get p.next with
    | None ->
      continue := false; (* return None, queue is empty *)
    | Some p_next ->
      let ok = A.compare_and_set self.head p p_next in
      if ok then (
        res := Some p_next.value;
        continue := false;
      )
  done;
  !res

module Blocking = struct
  type nonrec 'a t = {
    q: 'a t;
    n_parked: int A.t; (* threads waiting *)
    park_lock: Mutex.t;
    park_cond: Condition.t;
  }

  let create ~dummy () : _ t =
    { q=create ~dummy ();
      n_parked=A.make 0;
      park_lock=Mutex.create();
      park_cond=Condition.create();
    }

  let push (self:_ t) x : unit =
    push self.q x;
    (* if any thread is parked, try to unpark one thread. It is possible
       that a thread was parked, and woke up from another signal, to pick the
       value already, but this should be safe. *)
    if A.get self.n_parked > 0 then (
      Mutex.lock self.park_lock;
      Condition.signal self.park_cond;
      Mutex.unlock self.park_lock;
    )

  let[@inline] pop_nonblock self : _ option =
    pop_nonblock self.q

  let pop_block (self:'a t) : 'a =

    (* be on the safe side: assume we're going to park,
       so that if another thread pushes after the "PARK" line it'll unpark us *)
    A.incr self.n_parked;

    let rec loop () =
      match pop_nonblock self with
        | Some x ->
          (* release the token in self.n_parked *)
          A.decr self.n_parked;
          x
        | None ->
          (* PARK *)
          Mutex.lock self.park_lock;
          Condition.wait self.park_cond self.park_lock;
          Mutex.unlock self.park_lock;
          (* try again *)
          (loop [@tailcall]) ()
    in
    loop()

end
