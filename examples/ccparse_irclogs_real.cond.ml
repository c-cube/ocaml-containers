
(* parse IRC logs *)

type datetime = {
  year: int;
  month: int;
  day: int;
  hour: int;
  min: int;
  sec: int;
}

let pp_datetime out d =
  let {year;month;day;hour;min;sec} = d in
  CCFormat.(fprintf out "{y=%d;M=%d;d=%d;h=%d;m=%d;s=%d}"
              year month day hour min sec)

type msg = {
  timestamp: datetime;
  user: string;
  msg: string;
}

let pp_msg out m =
  CCFormat.fprintf out "{@[time=%a;@ user=%S;@ msg=%S@]}"
    pp_datetime m.timestamp m.user m.msg

open CCParse

let p_datetime : datetime t =
  let int = U.int in
  let* date, time = split_2 ~on_char:' ' in
  let* y, m, d = recurse date (split_3 ~on_char:'-') in
  let* year = recurse y int in
  let* month = recurse m int in
  let* day = recurse d int in
  let* hour, min, sec =
    recurse time
      (let* hour = int in
        char ':' *>
        let* min = int in
        char ':' *>
        let+ sec = int in
        hour,min,sec)
  in
  let dt = {year;month;day;hour;min;sec} in
  return dt

let p_line =
  let* line = lookahead all in

  if Slice.is_empty line then return None
  else (
    let* fields = split_list ~on_char:'\t' in
    match fields with
      | [date; user; rest] ->
        let+ timestamp = recurse date p_datetime
        and+ user = recurse user (chars_if (function '>' -> false | _ -> true))
        and+ msg = recurse rest (all_str >|= String.trim) in
        Some {timestamp; user; msg}

      | _ ->
        failf "expected 3 fields, got [%s]"
          (String.concat ";" @@ List.map String.escaped @@ List.map Slice.to_string fields)
  )

let p_file =
  each_line (parsing "line" p_line) >|=
  CCList.keep_some

let () =
  let s = CCIO.File.read_exn Sys.argv.(1) in
  match parse_string p_file s with
    | Ok l ->
      Format.printf "parsed:@.";
      List.iter (Format.printf "%a@." pp_msg) l
    | Error e ->
      Format.printf "parse error: %s@." e; exit 1
