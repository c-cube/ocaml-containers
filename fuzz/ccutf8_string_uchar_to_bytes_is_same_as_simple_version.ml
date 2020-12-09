let simple_uchar_to_string (c : Uchar.t) : string =
  let c = Uchar.to_int c in
  let bits =
    Array.make 64 false
    |> Array.mapi (fun i _ ->
        ((Int.shift_right c (63 - i)) land 0x1) <> 0
      )
  in
  let char_of_bit_list bits =
    let bits = Array.of_list bits in
    assert (Array.length bits = 8);
    let res = ref 0 in
    for i=0 to 7 do
      if bits.(i) then
        res := !res lor (0x1 lsl (7-i))
    done;
    Char.chr !res
  in
  let get_start_from_right i =
    Array.get bits (63 - i)
  in
  let chars =
    if c <= 0x7F then (
      [
        [
          false;
          get_start_from_right 6;
          get_start_from_right 5;
          get_start_from_right 4;
          get_start_from_right 3;
          get_start_from_right 2;
          get_start_from_right 1;
          get_start_from_right 0;
        ]
      ]
    )
    else if c <= 0x7FF then (
      [
        [
          true;
          true;
          false;
          get_start_from_right 10;
          get_start_from_right 9;
          get_start_from_right 8;
          get_start_from_right 7;
          get_start_from_right 6;
        ];
        [
          true;
          false;
          get_start_from_right 5;
          get_start_from_right 4;
          get_start_from_right 3;
          get_start_from_right 2;
          get_start_from_right 1;
          get_start_from_right 0;
        ];
      ]
    )
    else if c <= 0xFFFF then (
      [
        [
          true;
          true;
          true;
          false;
          get_start_from_right 15;
          get_start_from_right 14;
          get_start_from_right 13;
          get_start_from_right 12;
        ];
        [
          true;
          false;
          get_start_from_right 11;
          get_start_from_right 10;
          get_start_from_right 9;
          get_start_from_right 8;
          get_start_from_right 7;
          get_start_from_right 6;
        ];
        [
          true;
          false;
          get_start_from_right 5;
          get_start_from_right 4;
          get_start_from_right 3;
          get_start_from_right 2;
          get_start_from_right 1;
          get_start_from_right 0;
        ];
      ]
    )
    else if c <= 0x10FFFF then (
      [
        [
          true;
          true;
          true;
          true;
          false;
          get_start_from_right 20;
          get_start_from_right 19;
          get_start_from_right 18;
        ];
        [
          true;
          false;
          get_start_from_right 17;
          get_start_from_right 16;
          get_start_from_right 15;
          get_start_from_right 14;
          get_start_from_right 13;
          get_start_from_right 12;
        ];
        [
          true;
          false;
          get_start_from_right 11;
          get_start_from_right 10;
          get_start_from_right 9;
          get_start_from_right 8;
          get_start_from_right 7;
          get_start_from_right 6;
        ];
        [
          true;
          false;
          get_start_from_right 5;
          get_start_from_right 4;
          get_start_from_right 3;
          get_start_from_right 2;
          get_start_from_right 1;
          get_start_from_right 0;
        ];
      ]
    )
    else (
      failwith "Unexpected case"
    )
  in
  chars
  |> List.map char_of_bit_list
  |> List.to_seq
  |> String.of_seq

let () =
  Crowbar.add_test ~name:"ccutf8_string_uchar_to_bytes_is_same_as_simple_version" [ Crowbar.range (succ 0x10FFFF) ]
    (fun c ->
       Crowbar.guard (Uchar.is_valid c);
       let c = Uchar.of_int c in
       let simple_answer =
         simple_uchar_to_string c
       in
       let answer =
         let buf = ref [] in
         CCUtf8_string.uchar_to_bytes c (fun c ->
             buf := c :: !buf;
           );
         !buf
         |> List.rev
         |> List.to_seq
         |> String.of_seq
       in
       Crowbar.check_eq
         simple_answer answer
    )
