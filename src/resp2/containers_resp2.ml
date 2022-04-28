(** Main data type *)
module Data = struct
  type t =
    | Simple_string of string
    | Bulk_string of string
    | Int of int
    | Error of string
    | Array of t list

  let rec pp out = function
    | Simple_string s -> Format.fprintf out "%S" s
    | Bulk_string s -> Format.fprintf out "bulk %d %S" (String.length s) s
    | Int i -> Format.fprintf out "%d" i
    | Error s -> Format.fprintf out "error %S" s
    | Array l ->
      Format.fprintf out "@[<v2>*%d@ " (List.length l);
      List.iter (fun s -> Format.fprintf out "@[%a@]" pp s) l;
      Format.fprintf out "@]"

  let show d = Format.asprintf "%a" pp d
end

module Parse = struct
  type 'a action =
    | Return of 'a
    | Fail of string
    | Read_char of (char -> 'a action)
    | Read_line of (string -> 'a action)
    | Read_exact of int * (string -> 'a action)
    | Bind : 'b action * ('b -> 'a action) -> 'a action

  let[@inline] return x = Return x
  let[@inline] fail str = Fail str
  let[@inline] read_char f = Read_char f
  let[@inline] read_exact i f = Read_exact (i, f)
  let[@inline] read_line f = Read_line f
  let[@inline] ( >>= ) x f = Bind (x, f)

  let rec cont_parser () : Data.t action =
    read_char @@ function
    | '+' -> read_line @@ fun line -> return (Data.Simple_string line)
    | '-' -> read_line @@ fun line -> return (Data.Error line)
    | ':' ->
      read_line @@ fun line ->
      (try return (Data.Int (int_of_string line))
       with _ -> fail "expected integer")
    | '$' ->
      read_line @@ fun line ->
      (match int_of_string line with
      | exception _ -> fail "expected integer (length of bulk string)"
      | i -> read_exact i @@ fun str -> return (Data.Bulk_string str))
    | '*' ->
      read_line @@ fun line ->
      (match int_of_string line with
      | exception _ -> fail "expected integer (length of array)"
      | i ->
        let rec loop acc i =
          if i = 0 then
            return (Data.Array (List.rev acc))
          else
            cont_parser () >>= fun x -> loop (x :: acc) (i - 1)
        in
        loop [] i)
    | c -> fail (Printf.sprintf "invalid command prefix %C" c)

  let parse_string (str : string) : Data.t =
    let rec run : type a b. int -> a action -> (int -> a -> b) -> b =
     fun i act k ->
      match act with
      | Return x -> k i x
      | Fail s -> failwith s
      | Bind (x, f) -> run i x @@ fun i x -> run i (f x) k
      | Read_char f ->
        if i >= String.length str then failwith "unexpected EOF";
        let c = str.[i] in
        run (i + 1) (f c) k
      | Read_line f ->
        (match String.index_from str i '\n' with
        | exception Not_found -> failwith "expected a line"
        | j ->
          if str.[j - 1] <> '\r' then
            failwith "expected '\\r' before the newline";
          let line = String.sub str i (j - i - 1) in
          run (j + 1) (f line) k)
      | Read_exact (n, f) ->
        if i + n + 2 > String.length str then failwith "not enough bytes";
        let sub = String.sub str i n in
        let i = i + n in
        if str.[i] <> '\r' || str.[i + 1] <> '\n' then
          failwith "bytes must be followed by '\\r\\n'";
        run (i + 2) (f sub) k
    in
    run 0 (cont_parser ()) (fun _ x -> x)

  let parse_chan (ic : in_channel) : Data.t =
    let rec run : type a. a action -> a =
     fun act ->
      match act with
      | Return x -> x
      | Fail s -> failwith s
      | Bind (x, f) ->
        let x = run x in
        run (f x)
      | Read_char f ->
        let c = input_char ic in
        run (f c)
      | Read_line f ->
        let line = input_line ic |> String.trim in
        run (f line)
      | Read_exact (n, f) ->
        let sub = really_input_string ic n in
        run (f sub)
    in
    run (cont_parser ())
end

(*$= & ~printer:(CCFormat.to_string Data.pp)
  Data.(Array [Simple_string "a"; Int 2]) (Parse.parse_string "*2\r\n+a\r\n:2\r\n")
*)

module Print = struct
  type action =
    | Return
    | Write_char of char * (unit -> action)
    | Write_line of string * (unit -> action)
    | Write_string of string * (unit -> action)
    | Bind of action * (unit -> action)

  let return = Return
  let[@inline] write_char c f = Write_char (c, f)
  let[@inline] write_string s f = Write_string (s, f)
  let[@inline] write_line s f = Write_line (s, f)
  let[@inline] ( >>= ) x f = Bind (x, f)

  let rec cont_printer (d : Data.t) : action =
    match d with
    | Data.Int i ->
      write_char ':' @@ fun () ->
      write_line (string_of_int i) @@ fun () -> return
    | Data.Simple_string s ->
      write_char '+' @@ fun () ->
      write_line s @@ fun () -> return
    | Data.Error s ->
      write_char '-' @@ fun () ->
      write_line s @@ fun () -> return
    | Data.Bulk_string s ->
      write_char '$' @@ fun () ->
      write_line (string_of_int (String.length s)) @@ fun () ->
      write_string s @@ fun () -> return
    | Data.Array l ->
      write_char '*' @@ fun () ->
      write_line (string_of_int (List.length l)) @@ fun () ->
      let rec loop = function
        | [] -> return
        | x :: tl -> cont_printer x >>= fun () -> loop tl
      in
      loop l

  let to_string (d : Data.t) : string =
    let buf = Buffer.create 32 in
    let rec run (a : action) : unit =
      match a with
      | Return -> ()
      | Write_char (c, f) ->
        Buffer.add_char buf c;
        run (f ())
      | Write_line (s, f) ->
        Buffer.add_string buf s;
        Buffer.add_string buf "\r\n";
        run (f ())
      | Write_string (s, f) ->
        Buffer.add_string buf s;
        Buffer.add_string buf "\r\n";
        run (f ())
      | Bind (a, f) ->
        run a;
        run (f ())
    in
    run (cont_printer d);
    Buffer.contents buf

  let to_chan (oc : out_channel) (d : Data.t) : unit =
    let rec run (a : action) : unit =
      match a with
      | Return -> ()
      | Write_char (c, f) ->
        output_char oc c;
        run (f ())
      | Write_line (s, f) ->
        output_string oc s;
        output_string oc "\r\n";
        run (f ())
      | Write_string (s, f) ->
        output_string oc s;
        output_string oc "\r\n";
        run (f ())
      | Bind (a, f) ->
        run a;
        run (f ())
    in
    run (cont_printer d)
end

(*$= & ~printer:(Printf.sprintf "%S")
  "*2\r\n+a\r\n:2\r\n" (Print.to_string Data.(Array [Simple_string "a"; Int 2]))
*)
