
open QCheck

let check_create_cardinal =
  let gen = Arbitrary.small_int in
  let prop n = BV.cardinal (BV.create ~size:n true) = n in
  let name = "bv_create_cardinal" in
  mk_test ~name ~pp:string_of_int gen prop

let pp bv = PP.(list string) (List.map string_of_int (BV.to_list bv))

let check_iter_true =
  let gen = Arbitrary.(lift BV.of_list (list small_int)) in
  let prop bv =
    let l' = Sequence.to_rev_list (BV.iter_true bv) in
    let bv' = BV.of_list l' in
    BV.cardinal bv = BV.cardinal bv'
  in
  let name = "bv_iter_true" in
  mk_test ~pp ~size:BV.cardinal ~name gen prop

let props =
  [ check_create_cardinal
  ; check_iter_true
  ]
