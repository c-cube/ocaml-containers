
module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCCache;;

t @@ fun () ->
  let c = unbounded ~eq:Int64.equal 256 in
  let fib = with_cache_rec c
    (fun self n -> match n with
      | 1L | 2L -> 1L
      | _ -> CCInt64.(self (n-1L) + self (n-2L))
    )
  in
  assert_equal 55L (fib 10L);
  assert_equal 832040L (fib 30L);
  assert_equal 12586269025L (fib 50L);
  assert_equal 190392490709135L (fib 70L);
  true;;


t @@ fun () ->
  let eq (i1,_)(i2,_) = i1=i2 and hash (i,_) = CCInt.hash i in
  let c = lru ~eq ~hash 2 in
  ignore (with_cache c CCFun.id (1, true));
  ignore (with_cache c CCFun.id (1, false));
  with_cache c CCFun.id (1, false) = (1, true);;

t @@ fun () ->
  let f = (let r = ref 0 in fun _ -> incr r; !r) in
  let c = lru ~eq:CCInt.equal 2 in
  let res1 = with_cache c f 1 in
  let res2 = with_cache c f 2 in
  let res3 = with_cache c f 3 in
  let res1_bis = with_cache c f 1 in
  res1 <> res2 && res2 <> res3 && res3 <> res1_bis && res1_bis <> res1;;

t @@ fun () ->
  let f = (let r = ref 0 in fun _ -> incr r; !r) in
  let c = lru ~eq:CCEqual.unit 2 in
  let x = with_cache c f () in
  assert_equal 1 x;
  assert_equal 1 (size c);
  clear c ;
  assert_equal 0 (size c);
  let y = with_cache c f () in
  assert_equal 2 y;
  true;;
