module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCFun_vec

let spf = Printf.sprintf

let _listuniq =
  let g = Q.(list_small (pair nat_small nat_small)) in
  Q.map_same_type
    (fun l ->
      CCList.sort_uniq ~cmp:(fun a b -> Stdlib.compare (fst a) (fst b)) l)
    g
;;

t @@ fun () -> is_empty empty;;
t @@ fun () -> not (is_empty (return 2));;
t @@ fun () -> length (return 2) = 1;;

q _listuniq (fun l ->
    let m = of_list l in
    List.for_all (fun (i, y) -> get_exn i m = y) @@ List.mapi CCPair.make l)
;;

(* regression test for #298 *)
t @@ fun () ->
let rec consume x =
  match CCFun_vec.pop x with
  | None -> ()
  | Some (_, x) -> consume x
in
consume (of_list CCList.(1 -- 100));
true
;;

q
  Q.(pair int (list_small int))
  (fun (x, l) ->
    let q0 = of_list l in
    let q = push x q0 in
    assert_equal (length q) (length q0 + 1);
    let y, q = pop_exn q in
    assert_equal x y;
    assert_equal (to_list q) (to_list q0);
    true)
;;

q
  Q.(pair (fun1 Observable.int bool) (list_small int))
  (fun (f, l) ->
    let f = Q.Fn.apply f in
    List.map f l = (of_list l |> map f |> to_list))
;;

q
  Q.(pair (list_small int) (list_small int))
  (fun (l1, l2) -> l1 @ l2 = (append (of_list l1) (of_list l2) |> to_list))
;;

q Q.(list_small int) (fun l -> l = to_list (of_list l));;

q _listuniq (fun l ->
    List.sort Stdlib.compare l
    = (l |> Iter.of_list |> of_iter |> to_iter |> Iter.to_list
     |> List.sort Stdlib.compare))
;;

q _listuniq (fun l ->
    List.sort Stdlib.compare l
    = (l |> Gen.of_list |> of_gen |> to_gen |> Gen.to_list
     |> List.sort Stdlib.compare))
;;

t @@ fun () -> choose empty = None;;
t @@ fun () -> choose (of_list [ 1, 1; 2, 2 ]) <> None

module Ref_impl = struct
  type +'a t = 'a list

  let empty : _ t = []
  let length = List.length
  let push x l : _ t = l @ [ x ]
  let get i l = List.nth l i
  let to_list l = l
  let to_gen = Gen.of_list
  let add_list l l2 : _ t = List.append l l2

  let pop_exn l =
    match List.rev l with
    | x :: tl -> x, List.rev tl
    | [] -> invalid_arg "empty"

  let is_empty l = l = []

  let choose l =
    match l with
    | [] -> None
    | x :: _ -> Some x
end

module Op = struct
  type 'a t =
    | Push of 'a
    | Pop
    (* TODO: set *)
    | Add_list of 'a list
    | Check_get of int
    | Check_choose
    | Check_is_empty
    | Check_len
    | Check_to_list
    | Check_to_gen

  let well_formed ops : bool =
    let rec loop size = function
      | [] -> true
      | Push _ :: tl -> loop (size + 1) tl
      | Pop :: tl -> size >= 0 && loop (size - 1) tl
      | Add_list l :: tl -> loop (size + List.length l) tl
      | Check_get x :: tl -> x < size && loop size tl
      | Check_choose :: tl
      | Check_is_empty :: tl
      | Check_len :: tl
      | Check_to_list :: tl
      | Check_to_gen :: tl ->
        loop size tl
    in
    loop 0 ops

  let show show_x (self : _ t) : string =
    match self with
    | Push x -> spf "push %s" (show_x x)
    | Pop -> "pop"
    | Add_list l -> spf "add_list [%s]" (String.concat ";" @@ List.map show_x l)
    | Check_get i -> spf "check_get %d" i
    | Check_choose -> "check_choose"
    | Check_is_empty -> "check_is_empty"
    | Check_len -> "check_len"
    | Check_to_list -> "check_to_list"
    | Check_to_gen -> "check_to_gen"

  let shrink shrink_x (op : _ t) : _ Q.Iter.t =
    let open Q.Shrink in
    let open Q.Iter in
    match op with
    | Push x -> shrink_x x >|= fun x -> Push x
    | Pop -> empty
    | Add_list l -> list ~shrink:shrink_x l >|= fun x -> Add_list x
    | Check_get _ | Check_choose | Check_is_empty | Check_len | Check_to_list
    | Check_to_gen ->
      empty

  let shrink_l shrink_x : _ t list Q.Shrink.t =
    Q.Shrink.list ~shrink:(shrink shrink_x) |> Q.Shrink.filter well_formed

  type 'a op = 'a t

  (* generate list of length [n] *)
  let gen (gen_x : 'a Q.Gen.t) n : 'a t list Q.Gen.t =
    let open Q.Gen in
    let rec loop size n : 'a op list Q.Gen.t =
      if n = 0 then
        return []
      else (
        let op =
          oneof_weighted
          @@ List.flatten
               [
                 [
                   (3, gen_x >|= fun x -> Push x, size + 1);
                   1, return (Check_choose, size);
                   1, return (Check_is_empty, size);
                   1, return (Check_to_list, size);
                   1, return (Check_to_gen, size);
                 ];
                 (if size > 0 then
                    [
                      1, return (Pop, size - 1);
                      (1, 0 -- (size - 1) >|= fun x -> Check_get x, size);
                    ]
                  else
                    []);
                 [
                   ( 1,
                     list_small gen_x >|= fun l ->
                     Add_list l, size + List.length l );
                 ];
               ]
        in

        op >>= fun (op, size) ->
        loop size (n - 1) >>= fun tl -> return (op :: tl)
      )
    in
    loop 0 n
end

let arb_ops_int : int Op.t list Q.arbitrary =
  Q.make
    ~print:(fun o ->
      spf "[%s]" @@ String.concat ";" @@ List.map (Op.show @@ spf "%d") o)
    ~shrink:(Op.shrink_l Q.Shrink.int)
    Q.Gen.(0 -- 40 >>= fun len -> Op.gen nat_small len)

let check_ops ~show_x (ops : 'a Op.t list) : unit =
  let fail () =
    Q.Test.fail_reportf "on list [%s]"
      (String.concat ";" @@ List.map (Op.show show_x) ops)
  in
  let cur = ref empty in
  let cur_ref = ref Ref_impl.empty in
  List.iter
    (fun (op : _ Op.t) ->
      match op with
      | Op.Push x ->
        cur := push x !cur;
        cur_ref := Ref_impl.push x !cur_ref
      | Op.Pop ->
        let x1, cur' = pop_exn !cur in
        cur := cur';
        let x2, cur_ref' = Ref_impl.pop_exn !cur_ref in
        cur_ref := cur_ref';
        if x1 <> x2 then fail ()
      | Op.Add_list l ->
        cur := add_list !cur l;
        cur_ref := Ref_impl.add_list !cur_ref l
      | Op.Check_get i ->
        if get_exn i !cur <> Ref_impl.get i !cur_ref then fail ()
      | Op.Check_is_empty ->
        if is_empty !cur <> Ref_impl.is_empty !cur_ref then fail ()
      | Op.Check_len -> if length !cur <> Ref_impl.length !cur_ref then fail ()
      | Op.Check_to_list ->
        if to_list !cur <> Ref_impl.to_list !cur_ref then fail ()
      | Op.Check_choose ->
        if choose !cur <> Ref_impl.choose !cur_ref then fail ()
      | Op.Check_to_gen ->
        if
          to_gen !cur |> Gen.to_list <> (Ref_impl.to_gen !cur_ref |> Gen.to_list)
        then
          fail ())
    ops;
  ()

let () =
  q arb_ops_int (fun ops ->
      check_ops ~show_x:(spf "%d") ops;
      true)
