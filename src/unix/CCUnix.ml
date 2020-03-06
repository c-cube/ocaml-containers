
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 High-level Functions on top of Unix} *)

type 'a or_error = ('a, string) result
type 'a gen = unit -> 'a option

(** {2 Calling Commands} *)

let int_of_process_status = function
  | Unix.WEXITED i
  | Unix.WSIGNALED i
  | Unix.WSTOPPED i -> i

let str_exists s p =
  let rec f s p i =
    if i = String.length s then false
    else p s.[i] || f s p (i+1)
  in
  f s p 0

let rec iter_gen f g = match g() with
  | None -> ()
  | Some x -> f x; iter_gen f g

let finally_ f x ~h =
  try
    let y = f x in
    ignore (h());
    y
  with e ->
    ignore (h ());
    raise e

(* print a string, but escaped if required *)
let escape_str s =
  if
    str_exists s
      (function ' ' | '"' | '\'' | '\n' | '\t'-> true | _ -> false)
  then (
    let buf = Buffer.create (String.length s) in
    Buffer.add_char buf '\'';
    String.iter
      (function
        | '\'' -> Buffer.add_string buf "'\\''"
        | c -> Buffer.add_char buf c
      ) s;
    Buffer.add_char buf '\'';
    Buffer.contents buf
  ) else s

(*$T
  escape_str "foo" = "foo"
  escape_str "foo bar" = "'foo bar'"
  escape_str "fo'o b'ar" = "'fo'\\''o b'\\''ar'"
*)

let read_all ?(size=1024) ic =
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
    Bytes.sub_string !buf 0 !len

type call_result =
  < stdout:string;
    stderr:string;
    status:Unix.process_status;
    errcode:int; (** Extracted from status *)
  >

let kbprintf' buf fmt k = Printf.kbprintf k buf fmt

let call_full_inner ?(bufsize=2048) ?(stdin=`Str "") ?(env=Unix.environment()) ~f cmd =
  (* render the command *)
  let buf = Buffer.create 256 in
  kbprintf' buf cmd
    (fun buf ->
       let cmd = Buffer.contents buf in
       let oc, ic, errc = Unix.open_process_full cmd env in
       (* send stdin *)
       begin match stdin with
         | `Str s -> output_string ic s
         | `Gen g -> iter_gen (output_string ic) g
       end;
       close_out ic;
       (* read out and err *)
       let out = read_all ~size:bufsize oc in
       let err = read_all ~size:bufsize errc in
       let status = Unix.close_process_full (oc, ic, errc) in
       f (out,err,status)
    )

let call_full ?bufsize ?stdin ?env cmd =
  call_full_inner ?bufsize ?stdin ?env cmd
    ~f:(fun (out,err,status) ->
      object
        method stdout = out
        method stderr = err
        method status = status
        method errcode = int_of_process_status status
      end)

(*$T
  call_full ~stdin:(`Str "abc") "cat" |> stdout = "abc"
  call_full "echo %s" (escape_str "a'b'c") |> stdout = "a'b'c\n"
  call_full "echo %s" "a'b'c" |> stdout = "abc\n"
*)

let call ?bufsize ?stdin ?env cmd =
  call_full_inner ?bufsize ?stdin ?env cmd
    ~f:(fun (out,err,status) -> out, err, int_of_process_status status)

let call_stdout ?bufsize ?stdin ?env cmd =
  call_full_inner ?bufsize ?stdin ?env cmd
    ~f:(fun (out,_err,_status) -> out)

(*$T
  call_stdout ~stdin:(`Str "abc") "cat" = "abc"
  call_stdout "echo %s" (escape_str "a'b'c") = "a'b'c\n"
  call_stdout "echo %s" "a'b'c" = "abc\n"
*)

type line = string

type async_call_result =
  < stdout:line gen;
    stderr:line gen;
    stdin:line -> unit; (* send a line *)
    close_in:unit; (* close stdin *)
    close_err:unit;
    close_out:unit;
    close_all:unit;  (* close all 3 channels *)
    wait:Unix.process_status;  (* block until the process ends *)
    wait_errcode:int; (* block until the process ends, then extract errcode *)
  >

let async_call ?(env=Unix.environment()) cmd =
  (* render the command *)
  let buf = Buffer.create 256 in
  kbprintf' buf cmd
    (fun buf ->
       let cmd = Buffer.contents buf in
       let oc, ic, errc = Unix.open_process_full cmd env in
       object (self)
         method stdout () =
           try Some (input_line oc)
           with End_of_file -> None
         method stderr () =
           try Some (input_line errc)
           with End_of_file -> None
         method stdin l = output_string ic l; output_char ic '\n'
         method close_in = close_out ic
         method close_out = close_in oc
         method close_err = close_in errc
         method close_all = close_out ic; close_in oc; close_in errc; ()
         method wait = Unix.close_process_full (oc, ic, errc)
         method wait_errcode = int_of_process_status self#wait
       end
    )

let stdout x = x#stdout
let stderr x = x#stderr
let status x = x#status
let errcode x = x#errcode

let with_in ?(mode=0o644) ?(flags=[]) file ~f =
  let fd = Unix.openfile file (Unix.O_RDONLY::flags) mode in
  let ic = Unix.in_channel_of_descr fd in
  finally_ f ic
    ~h:(fun () -> Unix.close fd)

let with_out ?(mode=0o644) ?(flags=[Unix.O_CREAT; Unix.O_TRUNC]) file ~f =
  let fd = Unix.openfile file (Unix.O_WRONLY::flags) mode in
  let oc = Unix.out_channel_of_descr fd in
  finally_ f oc
    ~h:(fun () -> flush oc; Unix.close fd)

let with_process_in cmd ~f =
  let ic = Unix.open_process_in cmd in
  finally_ f ic
    ~h:(fun () -> ignore (Unix.close_process_in ic))

let with_process_out cmd ~f =
  let oc = Unix.open_process_out cmd in
  finally_ f oc
    ~h:(fun () -> ignore (Unix.close_process_out oc))

type process_full = <
  stdin: out_channel;
  stdout: in_channel;
  stderr: in_channel;
  close: Unix.process_status;
>

let with_process_full ?env cmd ~f =
  let env = match env with None -> Unix.environment () | Some e -> e in
  let oc, ic, err = Unix.open_process_full cmd env in
  let close = lazy (Unix.close_process_full (oc,ic,err)) in
  let p = object
    method stdin = ic
    method stdout = oc
    method stderr = err
    method close = Lazy.force close
  end in
  finally_ f p ~h:(fun () -> p#close)

let with_connection addr ~f =
  let ic, oc = Unix.open_connection addr in
  finally_ (fun () -> f ic oc) ()
    ~h:(fun () -> Unix.shutdown_connection ic)

(* make sure that we are a session leader; that is, our children die if we die *)
let ensure_session_leader =
  let thunk = lazy (
    if not Sys.win32 && not Sys.cygwin
    then ignore (Unix.setsid ())
  ) in
  fun () -> Lazy.force thunk

exception ExitServer

(* version of {!Unix.establish_server} that doesn't fork *)
let establish_server sockaddr ~f =
  let sock =
    Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 5;
  let continue = ref true in
  while !continue do
    try
      let s, _ = Unix.accept sock in
      let ic = Unix.in_channel_of_descr s in
      let oc = Unix.out_channel_of_descr s in
      ignore (f ic oc)
    with ExitServer ->
      continue := false
  done


(** {2 Locking} *)

let with_file_lock ~kind filename f =
  let lock_file = Unix.openfile filename [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
  let lock_action = match kind with
    | `Read -> Unix.F_RLOCK
    | `Write -> Unix.F_LOCK
  in
  Unix.lockf lock_file lock_action 0;
  try
    let x = f () in
    Unix.lockf lock_file Unix.F_ULOCK 0;
    Unix.close lock_file;
    x
  with e ->
    Unix.lockf lock_file Unix.F_ULOCK 0;
    Unix.close lock_file;
    raise e

(*$R
  let m = 200 in
  let n = 50 in
  let write_atom filename s =
    with_file_lock ~kind:`Write filename
      (fun () ->
        CCIO.with_out ~flags:[Open_append; Open_creat]
          filename (fun oc -> output_string oc s; flush oc))
  in
  let f filename =
    for j=1 to m do
      write_atom filename "foo\n"
    done
  in
  CCIO.File.with_temp ~prefix:"containers_" ~suffix:".txt"
    (fun filename ->
        let a = Array.init n (fun _ -> Thread.create f filename) in
        Array.iter Thread.join a;
        let lines = CCIO.with_in filename CCIO.read_lines_l in
        assert_equal ~printer:string_of_int (n * m) (List.length lines);
        assert_bool "all valid" (List.for_all ((=) "foo") lines))
*)

module Infix = struct
  let (?|) fmt = call_full fmt

  let (?|&) fmt = async_call fmt
end

include Infix

(** {2 Temporary directory} *)

let rand_digits_ =
  let st = lazy (Random.State.make_self_init()) in
  fun () ->
    let rand = Random.State.bits (Lazy.force st) land 0xFFFFFF in
    Printf.sprintf "%06x" rand

let rmdir_ dir =
  try ignore (Sys.command ("rm -r " ^ dir) : int)
  with _ -> ()

let with_temp_dir ?(mode=0o700) ?dir pat (f: string -> 'a) : 'a =
  let dir = match dir with
    | Some d -> d
    | None   -> Filename.get_temp_dir_name ()
  in
  let raise_err msg = raise (Sys_error msg) in
  let rec loop count =
    if count < 0 then (
      raise_err "mk_temp_dir: too many failing attempts"
    ) else (
      let dir = Filename.concat dir (pat ^ rand_digits_ ()) in
      match Unix.mkdir dir mode with
      | () ->
        finally_ f dir ~h:(fun () -> rmdir_ dir)
      | exception Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
      | exception Unix.Unix_error (Unix.EINTR, _, _)  -> loop count
      | exception Unix.Unix_error (e, _, _)           ->
        raise_err ("mk_temp_dir: " ^ (Unix.error_message e))
    )
  in
  loop 1000

(*$R
  let filename = with_temp_dir "test_containers"
      (fun dir ->
         let name = Filename.concat dir "test" in
         CCIO.with_out name (fun oc -> output_string oc "content"; flush oc);
         assert_bool ("file exists:"^name) (Sys.file_exists name);
         name)
  in
  assert_bool ("file does not exist"^filename) (not (Sys.file_exists filename));
  ()
*)
