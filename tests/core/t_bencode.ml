module T = (val Containers_testlib.make ~__FILE__ ())
include T
open Containers_bencode;;

eq ~printer:to_string_debug (map_of_list []) (Decode.of_string_exn "de");;

eq ~printer:to_string_debug
  (list [ int 1; int 2; string "foo" ])
  (Decode.of_string_exn "li1ei2e3:fooe")

module B = Containers_bencode

let rec size = function
  | Int _ | String _ -> 1
  | List l -> List.fold_left (fun n x -> n + size x) 0 l
  | Map m -> Str_map.fold (fun _ v n -> size v + n) m 0

let g_rand_b =
  Q.Gen.(
    sized_size (0 -- 7)
    @@ fix
    @@ fun self n ->
    let str n = string_size ~gen:char (0 -- n) in
    let base = [ int >|= B.int; str 100 >|= B.string ] in
    match n with
    | 0 -> oneof base
    | n ->
      frequency
      @@ List.map (fun x -> 2, x) base
      @ [
          1, list_size (0 -- 10) (self (n - 1)) >|= B.list;
          ( 1,
            list_size (0 -- 10) (pair (str 10) (self (n - 1))) >|= B.map_of_list
          );
        ])

let rec shrink_b self =
  Q.(
    Iter.(
      match self with
      | Int i -> Shrink.int64 i >|= B.int64
      | String s -> Shrink.string s >|= B.string
      | List l -> Shrink.list ~shrink:shrink_b l >|= B.list
      | Map l ->
        let l = Str_map.fold (fun k v l -> (k, v) :: l) l [] in
        Shrink.list
          ~shrink:(fun (k, v) ->
            Shrink.string k
            >|= (fun k -> k, v)
            <+> (shrink_b v >|= fun v -> k, v))
          l
        >|= B.map_of_list))

let rand_b =
  Q.make ~print:to_string_debug
    ~stats:[ "size", size ]
    ~shrink:shrink_b g_rand_b
;;

q rand_b (fun b ->
    let s = Encode.to_string b in
    equal (Decode.of_string_exn s) b)
