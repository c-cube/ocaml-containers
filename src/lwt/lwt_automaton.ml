
(*
copyright (c) 2013, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption) however
caused and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
*)

(** {1 interface lwt-automaton} *)

open Containers_misc

module I = struct
  let send f i =
    Lwt.on_success f (Automaton.I.send i)

  let iter_stream str i =
    Lwt_stream.iter (Automaton.I.send i) str
end

module O = struct
  let next o =
    let fut, send = Lwt.wait () in
    Automaton.O.once o (Lwt.wakeup send);
    fut
end

let next_transition a = O.next (Automaton.Instance.transitions a)

let (>>=) = Lwt.bind

module Unix = struct
  let read_write fd =
    let err_fut, err_send = Lwt.wait () in
    let transition st i = match st, i with
      | `Error _, _
      | `Stopped, _ -> st, []
      | `Active, `Failwith e ->
          Lwt.ignore_result (Lwt_unix.close fd);
          `Error e, [ `Error e ]
      | `Active, `Stop ->
          Lwt.ignore_result (Lwt_unix.close fd);
          `Stopped, [`Closed]
      | `Active, `Write s ->
          let fut = Lwt_unix.write fd s 0 (Bytes.length s) in
          (* propagate error *)
          Lwt.on_failure fut (fun e -> Lwt.wakeup err_send e);
          st, []
      | `Active, `JustRead s ->
          st, [`Read s]
    in
    let a = Automaton.Instance.create ~f:transition `Active in
    let buf = Bytes.make 128 ' ' in
    (* read a string from buffer *)
    let rec _read () =
      if Automaton.Instance.state a = `Active
        then Lwt_unix.read fd buf 0 (Bytes.length buf) >>= fun n ->
        begin if n = 0
          then Automaton.Instance.send a `Stop
          else
            let s = Bytes.sub_string buf 0 n in
            Automaton.Instance.send a (`JustRead s)
        end;
        _read ()
      else Lwt.return_unit
    in
    Lwt.ignore_result (_read ());
    Lwt.on_success err_fut
      (fun e -> Automaton.Instance.send a (`Failwith e));
    a

  let timeout f =
    let o = Automaton.O.create () in
    let fut = Lwt_unix.sleep f in
    Lwt.on_success fut
      (fun () -> Automaton.O.send o `Timeout);
    o
end
