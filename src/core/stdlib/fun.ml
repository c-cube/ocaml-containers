(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external id : 'a -> 'a = "%identity"
let const c _ = c
let flip f x y = f y x
let negate p v = not (p v)

exception Finally_raised of exn

let protect ~(finally : unit -> unit) work =
  let finally_no_exn () =
    try finally () with e ->
      raise (Finally_raised e)
  in
  match work () with
  | result -> finally_no_exn () ; result
  | exception work_exn ->
      finally_no_exn () ;
      raise work_exn
