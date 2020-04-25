
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 IO Utils} *)

type 'a or_error = ('a, string) result
type 'a gen = unit -> 'a option

let gen_empty () = None

let gen_singleton x =
  let done_ = ref false in
  fun () -> if !done_ then None else (done_ := true; Some x)

let gen_filter_map f gen =
  (* tailrec *)
  let rec next () =
    match gen() with
      | None -> None
      | Some x ->
        match f x with
          | None -> next()
          | (Some _) as res -> res
  in next

let gen_of_array arr =
  let r = ref 0 in
  fun () ->
    if !r = Array.length arr then None
    else (
      let x = arr.(!r) in
      incr r;
      Some x
    )

let gen_flat_map f next_elem =
  let state = ref `Init in
  let rec next() =
    match !state with
      | `Init -> get_next_gen()
      | `Run gen ->
        begin match gen () with
          | None -> get_next_gen ()
          | (Some _) as x -> x
        end
      | `Stop -> None
  and get_next_gen() = match next_elem() with
    | None -> state:=`Stop; None
    | Some x ->
      try state := `Run (f x); next()
      with e -> state := `Stop; raise e
  in
  next

let finally_ f x ~h =
  try
    let res = f x in
    h x;
    res
  with e ->
    h x;
    raise e

let with_in ?(mode=0o644) ?(flags=[Open_text]) filename f =
  let ic = open_in_gen (Open_rdonly::flags) mode filename in
  finally_ f ic ~h:close_in

let read_chunks_gen ?(size=1024) ic =
  let buf = Bytes.create size in
  let next() =
    let n = input ic buf 0 size in
    if n = 0
    then None
    else Some (Bytes.sub_string buf 0 n)
  in
  next

let read_line ic =
  try Some (input_line ic)
  with End_of_file -> None

let read_lines_gen ic =
  let stop = ref false in
  fun () ->
    if !stop then None
    else try Some (input_line ic)
      with End_of_file -> (stop:=true; None)

let read_lines_l ic =
  let l = ref [] in
  try
    while true do
      l := input_line ic :: !l
    done;
    assert false
  with End_of_file ->
    List.rev !l

(* thanks to nicoo for this trick *)
type _ ret_type =
  | Ret_string : string ret_type
  | Ret_bytes : Bytes.t ret_type

let read_all_
  : type a. op:a ret_type -> size:int -> in_channel -> a
  = fun ~op ~size ic ->
    let buf = ref (Bytes.create size) in
    let len = ref 0 in
    try
      while true do
        (* resize *)
        if !len = Bytes.length !buf then (
          buf := Bytes.extend !buf 0 !len;
        );
        assert (Bytes.length !buf > !len);
        let n = input ic !buf !len (Bytes.length !buf - !len) in
        len := !len + n;
        if n = 0 then raise Exit;  (* exhausted *)
      done;
      assert false (* never reached*)
    with Exit ->
    match op with
      | Ret_string -> Bytes.sub_string !buf 0 !len
      | Ret_bytes -> Bytes.sub !buf 0 !len

let read_all_bytes ?(size=1024) ic = read_all_ ~op:Ret_bytes ~size ic

let read_all ?(size=1024) ic = read_all_ ~op:Ret_string ~size ic

(*$R
  let s = String.make 200 'y' in
  let s = Printf.sprintf "a\nb\n %s\nlast line\n" s in
  OUnit.bracket_tmpfile ~prefix:"test_containers" ~mode:[Open_creat; Open_trunc]
    (fun (name, oc) ->
      output_string oc s;
      flush oc;
      let s' = with_in name read_all in
      OUnit.assert_equal ~printer:(fun s->s) s s'
    ) ()
*)


let with_out ?(mode=0o644) ?(flags=[Open_creat; Open_trunc; Open_text]) filename f =
  let oc = open_out_gen (Open_wronly::flags) mode filename in
  finally_ f oc ~h:close_out

let with_out_a ?mode ?(flags=[]) filename f =
  with_out ?mode ~flags:(Open_wronly::Open_creat::Open_append::flags) filename f

let write_line oc s =
  output_string oc s;
  output_char oc '\n'

let write_gen ?(sep="") oc g =
  let rec recurse () = match g() with
    | None -> ()
    | Some s ->
      output_string oc sep;
      output_string oc s;
      recurse ()
  in match g() with
    | None -> ()
    | Some s ->
      output_string oc s;
      recurse ()

let rec write_lines oc g = match g () with
  | None -> ()
  | Some l ->
    write_line oc l;
    write_lines oc g

let write_lines_l oc l =
  List.iter (write_line oc) l

(* test {read,write}_lines. Need to concatenate the lists because some
   strings in the random input might contain '\n' themselves *)

(*$QR
   Q.(list_of_size Gen.(0 -- 40) printable_string) (fun l ->
     let l' = ref [] in
     OUnit.bracket_tmpfile ~prefix:"test_containers" ~mode:[Open_creat; Open_trunc]
      (fun (name, oc) ->
        write_lines_l oc l;
        flush oc;
        l' := with_in name read_lines_l;
      ) ();
     String.concat "\n" l = String.concat "\n" !l'
    )
*)

(*$QR
   Q.(list_of_size Gen.(0 -- 40) printable_string) (fun l ->
     let l' = ref [] in
     OUnit.bracket_tmpfile ~prefix:"test_containers" ~mode:[Open_creat; Open_trunc]
      (fun (name, oc) ->
        write_lines oc (Gen.of_list l);
        flush oc;
        l' := with_in name (fun ic -> read_lines_gen ic |> Gen.to_list);
      ) ();
     String.concat "\n" l = String.concat "\n" !l'
    )
*)

let with_in_out ?(mode=0o644) ?(flags=[Open_creat]) filename f =
  let ic = open_in_gen (Open_rdonly::flags) mode filename in
  let oc = open_out_gen (Open_wronly::flags) mode filename in
  try
    let x = f ic oc in
    close_out oc; (* must be first?! *)
    close_in ic;
    x
  with e ->
    close_out_noerr oc;
    close_in_noerr ic;
    raise e

let copy_into ?(bufsize=4_096) ic oc : unit =
  let buf = Bytes.create bufsize in
  let cont = ref true in
  while !cont do
    let n = input ic buf 0 bufsize in
    if n > 0 then (
      output oc buf 0 n;
    ) else (
      cont := false
    )
  done

(*$QR
   Q.(list_of_size Gen.(0 -- 40) printable_string) (fun l ->
     let s = ref "" in
     OUnit.bracket_tmpfile ~prefix:"test_containers1" ~mode:[Open_creat; Open_trunc]
      (fun (name1, oc1) ->
        write_gen ~sep:"" oc1 (Gen.of_list l);
        flush oc1;
        OUnit.bracket_tmpfile ~prefix:"test_containers2" ~mode:[Open_creat; Open_trunc]
          (fun (name2, oc2) ->
             CCIO.with_in name1 (fun ic1 -> copy_into ic1 oc2);
             flush oc2;
             s := with_in name2 read_all;) ();
      ) ();
     String.concat "" l = !s
    )
*)

let tee funs g () = match g() with
  | None -> None
  | Some x as res ->
    List.iter (fun f -> f x) funs;
    res

(* TODO: lines/unlines:  string gen -> string gen *)

(* TODO:  words: string gen -> string gen,
   with a state machine that goes:
   - 0: read input chunk
   - switch to "search for ' '", and yield word
   - goto 0 if no ' ' found
   - yield leftover when g returns Stop
*)

module File = struct
  type t = string

  let to_string f = f

  let make f =
    if Filename.is_relative f
    then Filename.concat (Sys.getcwd()) f
    else f

  let exists f = Sys.file_exists f

  let is_directory f = Sys.is_directory f

  let remove_exn f = Sys.remove f

  let remove f =
    try Ok (Sys.remove f)
    with exn ->
      Error (Printexc.to_string exn)

  let read_exn f = with_in f (read_all_ ~op:Ret_string ~size:4096)

  let read f =
    try Ok (read_exn f) with e -> Error (Printexc.to_string e)

  let append_exn f x =
    with_out ~flags:[Open_append; Open_creat; Open_text] f
      (fun oc -> output_string oc x; flush oc)

  let append f x =
    try Ok (append_exn f x) with e -> Error (Printexc.to_string e)

  let write_exn f x =
    with_out f
      (fun oc -> output_string oc x; flush oc)

  let write f x =
    try Ok (write_exn f x) with e -> Error (Printexc.to_string e)

  let remove_noerr f = try Sys.remove f with _ -> ()

  let read_dir_base d =
    if Sys.is_directory d
    then
      let arr = Sys.readdir d in
      gen_of_array arr
    else fun () -> None

  let cons_ x tl =
    let first=ref true in
    fun () ->
      if !first then (
        first := false;
        Some x
      ) else tl ()

  let rec walk d =
    if not (Sys.file_exists d) then gen_empty
    else if Sys.is_directory d
    then (
      (* try to list the directory *)
      let arr = try Sys.readdir d with Sys_error _ -> [||] in
      let tail = gen_of_array arr in
      let tail = gen_flat_map
          (fun s -> walk (Filename.concat d s))
          tail
      in cons_ (`Dir,d) tail
    )
    else gen_singleton (`File, d)

  (*$R
    OUnit.assert_bool "walk categorizes files"
      (File.walk "."
        |> Gen.for_all
          (function
            | `File, f -> not (Sys.is_directory f)
            | `Dir, f -> Sys.is_directory f
          )
      )
  *)

  let walk_l d =
    let l = ref [] in
    let g = walk d in
    let rec aux () = match g() with
      | None -> !l
      | Some x -> l := x :: !l; aux ()
    in aux ()

  type walk_item = [`File | `Dir] * t

  let read_dir ?(recurse=false) d =
    if recurse
    then
      gen_filter_map
        (function
          | `File, f -> Some f
          | `Dir, _ -> None)
        (walk d)
    else read_dir_base d

  let show_walk_item ((i,f):walk_item) =
    (match i with
      | `File -> "file:"
      | `Dir -> "dir:"
    ) ^ f

  let with_temp ?temp_dir ~prefix ~suffix f =
    let name = Filename.temp_file ?temp_dir prefix suffix in
    finally_ f name ~h:remove_noerr
end
