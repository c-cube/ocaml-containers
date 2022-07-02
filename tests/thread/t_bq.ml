
module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCBlockingQueue;;

t @@ fun () ->
  let q = create 1 in
  let t1 = CCThread.spawn (fun () -> push q 1; push q 2) in
  let t2 = CCThread.spawn (fun () -> push q 3; push q 4) in
  let l = CCLock.create [] in
  let t3 = CCThread.spawn (fun () -> for _i = 1 to 4 do
      let x = take q in
      CCLock.update l (fun l -> x :: l)
    done)
  in
  Thread.join t1; Thread.join t2; Thread.join t3;
  assert_equal [1;2;3;4] (List.sort Stdlib.compare (CCLock.get l));
  true;;

t @@ fun () ->
  let n = 1000 in
  let lists = [| CCList.(1 -- n) ; CCList.(n+1 -- 2*n); CCList.(2*n+1 -- 3*n) |] in
  let q = create 2 in
  let senders = CCThread.Arr.spawn 3
    (fun i ->
      if i=1
      then push_list q lists.(i)  (* test push_list *)
      else List.iter (push q) lists.(i)
    )
  in
  let res = CCLock.create [] in
  let receivers = CCThread.Arr.spawn 3
    (fun i ->
      if i=1 then
        let l = take_list q n in
        CCLock.update res (fun acc -> l @ acc)
      else
        for _j = 1 to n do
          let x = take q in
          CCLock.update res (fun acc -> x::acc)
        done
    )
  in
  CCThread.Arr.join senders; CCThread.Arr.join receivers;
  let l = CCLock.get res |> List.sort Stdlib.compare in
  assert_equal CCList.(1 -- 3*n) l;
  true
