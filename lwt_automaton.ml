
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

module I = struct
  let send f i =
    Lwt.on_success f (fun x -> Automaton.I.send x i)

  let iter_stream str i =
    Lwt_stream.iter (fun x -> Automaton.I.send x i) str
end

module O = struct
  let next o =
    let fut, send = Lwt.wait () in
    Automaton.O.once o (Lwt.wakeup send);
    fut
end

let next_transition a = O.next (Automaton.Instance.transitions a)
