
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash Table with Heterogeneous Keys} *)

(*$inject
  open CCFun

*)

type 'b injection = {
  get : (unit -> unit) -> 'b option;
  set : 'b -> (unit -> unit);
}

(*$R
  let inj_int = create_inj () in
  let tbl = create 10 in
  OUnit.assert_equal None (get ~inj:inj_int tbl "a");
  set ~inj:inj_int tbl "a" 1;
  OUnit.assert_equal (Some 1) (get ~inj:inj_int tbl "a");
  let inj_string = create_inj () in
  set ~inj:inj_string tbl "b" "Hello";
  OUnit.assert_equal (Some "Hello") (get ~inj:inj_string tbl "b");
  OUnit.assert_equal None (get ~inj:inj_string tbl "a");
  OUnit.assert_equal (Some 1) (get ~inj:inj_int tbl "a");
  set ~inj:inj_string tbl "a" "Bye";
  OUnit.assert_equal None (get ~inj:inj_int tbl "a");
  OUnit.assert_equal (Some "Bye") (get ~inj:inj_string tbl "a");
*)

type 'a t = ('a, unit -> unit) Hashtbl.t

let create n = Hashtbl.create n

let create_inj () =
  let r = ref None in
  let get f =
    r := None;
    f ();
    !r
  and set v =
    (fun () -> r := Some v)
  in
  {get;set}

let get ~inj tbl x =
  try inj.get (Hashtbl.find tbl x)
  with Not_found -> None

let set ~inj tbl x y =
  Hashtbl.replace tbl x (inj.set y)

let length tbl = Hashtbl.length tbl

(*$R
  let inj_int = create_inj () in
  let tbl = create 5 in
  set ~inj:inj_int tbl "foo" 1;
  set ~inj:inj_int tbl "bar" 2;
  OUnit.assert_equal 2 (length tbl);
  OUnit.assert_equal 2 (find ~inj:inj_int tbl "bar");
  set ~inj:inj_int tbl "foo" 42;
  OUnit.assert_equal 2 (length tbl);
  remove tbl "bar";
  OUnit.assert_equal 1 (length tbl);
*)

let clear tbl = Hashtbl.clear tbl

(*$R
  let inj_int = create_inj () in
  let inj_str = create_inj () in
  let tbl = create 5 in
  set ~inj:inj_int tbl "foo" 1;
  set ~inj:inj_int tbl "bar" 2;
  set ~inj:inj_str tbl "baaz" "hello";
  OUnit.assert_equal 3 (length tbl);
  clear tbl;
  OUnit.assert_equal 0 (length tbl);
*)

let remove tbl x = Hashtbl.remove tbl x

let copy tbl = Hashtbl.copy tbl

let is_some = function
  | None -> false
  | Some _ -> true

let mem ~inj tbl x =
  try
    is_some (inj.get (Hashtbl.find tbl x))
  with Not_found -> false

(*$R
  let inj_int = create_inj () in
  let inj_str = create_inj () in
  let tbl = create 5 in
  set ~inj:inj_int tbl "foo" 1;
  set ~inj:inj_int tbl "bar" 2;
  set ~inj:inj_str tbl "baaz" "hello";
  OUnit.assert_bool "mem foo int" (mem ~inj:inj_int tbl "foo");
  OUnit.assert_bool "mem bar int" (mem ~inj:inj_int tbl "bar");
  OUnit.assert_bool "not mem baaz int" (not (mem ~inj:inj_int tbl "baaz"));
  OUnit.assert_bool "not mem foo str" (not (mem ~inj:inj_str tbl "foo"));
  OUnit.assert_bool "not mem bar str" (not (mem ~inj:inj_str tbl "bar"));
  OUnit.assert_bool "mem baaz str" (mem ~inj:inj_str tbl "baaz");
*)

let find ~inj tbl x =
  match inj.get (Hashtbl.find tbl x) with
    | None -> raise Not_found
    | Some v -> v

let iter_keys tbl f =
  Hashtbl.iter (fun x _ -> f x) tbl

let fold_keys tbl acc f =
  Hashtbl.fold (fun x _ acc -> f acc x) tbl acc

(** {2 Iterators} *)

type 'a sequence = ('a -> unit) -> unit

let keys_seq tbl yield =
  Hashtbl.iter
    (fun x _ -> yield x)
    tbl

(*$R
  let inj_int = create_inj () in
  let inj_str = create_inj () in
  let tbl = create 5 in
  set ~inj:inj_int tbl "foo" 1;
  set ~inj:inj_int tbl "bar" 2;
  set ~inj:inj_str tbl "baaz" "hello";
  let l = keys_seq tbl |> Iter.to_list in
  OUnit.assert_equal ["baaz"; "bar"; "foo"] (List.sort compare l);
*)

let bindings_of ~inj tbl yield =
  Hashtbl.iter
    (fun k value ->
       match inj.get value with
         | None -> ()
         | Some v -> yield (k, v)
    ) tbl

type value =
  | Value : ('b injection -> 'b option) -> value

let bindings tbl yield =
  Hashtbl.iter
    (fun x y -> yield (x, Value (fun inj -> inj.get y)))
    tbl

(*$R
  let inj_int = create_inj () in
  let inj_str = create_inj () in
  let tbl = create 5 in
  set ~inj:inj_int tbl "foo" 1;
  set ~inj:inj_int tbl "bar" 2;
  set ~inj:inj_str tbl "baaz" "hello";
  set ~inj:inj_str tbl "str" "rts";
  let l_int = bindings_of ~inj:inj_int tbl |> Iter.to_list in
  OUnit.assert_equal ["bar", 2; "foo", 1] (List.sort compare l_int);
  let l_str = bindings_of ~inj:inj_str tbl |> Iter.to_list in
  OUnit.assert_equal ["baaz", "hello"; "str", "rts"] (List.sort compare l_str);
*)
