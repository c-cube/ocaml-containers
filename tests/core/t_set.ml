
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

module S = CCSet.Make(struct
  type t = int
  let compare x y = Stdlib.compare x y
end);;

eq ~printer:(fun s -> s)
  (S.to_string string_of_int (S.of_list [4; 3])) "3,4";;

q Q.(list int) (fun l ->
    let s = S.of_list l in
    (S.to_string string_of_int s)
      = (CCList.sort_uniq ~cmp:CCInt.compare l
         |> List.map string_of_int |> String.concat ","));;
q Q.(list int) (fun l ->
    let s = S.of_list l in
    (S.to_string ~sep:" " string_of_int s)
      = (CCList.sort_uniq ~cmp:CCInt.compare l
         |> List.map string_of_int |> String.concat " "));;
