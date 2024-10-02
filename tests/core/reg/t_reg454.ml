module Vec = CCVector

let () =
  let arr : Int32.t Vec.vector = Vec.create () in
  Vec.push arr (Int32.of_int 123456);
  Format.printf "%d\n" (Int32.to_int (Vec.get arr 0));
  let x = Vec.get arr 0 in
  Format.printf "%d\n" (Int32.to_int x)
