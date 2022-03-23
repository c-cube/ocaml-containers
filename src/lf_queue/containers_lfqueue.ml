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

(*$R
  let q = create ~dummy:0 () in
  push q 1;
  push q 2;
  assert_equal (Some 1) (pop_nonblock q);
  assert_equal (Some 2) (pop_nonblock q);
  assert_equal None (pop_nonblock q);
  push q 3;
  assert_equal (Some 3) (pop_nonblock q);
  assert_equal None (pop_nonblock q);
*)

(*$R
  let q = create ~dummy:0 () in

  let gen () =
    for i = 0 to 5 do
      Thread.delay 0.01;
      push q i
    done
  in

  let out = ref [] in
  let consume () =
    let missing = ref 6 in
    while !missing > 0 do
      match pop_nonblock q with
        | Some x -> out := x :: !out; decr missing
        | None -> Thread.yield();
    done
  in

  let th = [Thread.create gen (); Thread.create consume ()] in
  List.iter Thread.join th;
  assert_equal [5;4;3;2;1;0] !out
*)
