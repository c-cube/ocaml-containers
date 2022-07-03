(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id$ i*)

(*i*)
open Obj
(*i*)

(*s Pointers already visited are stored in a hash-table, where
    comparisons are done using physical equality. *)

module H = Hashtbl.Make (struct
  type t = Obj.t

  let equal = ( == )
  let hash o = Hashtbl.hash (magic o : int)
end)

let node_table = (H.create 257 : unit H.t)

let in_table o =
  try
    H.find node_table o;
    true
  with Not_found -> false

let add_in_table o = H.add node_table o ()
let reset_table () = H.clear node_table

(*s Objects are traversed recursively, as soon as their tags are less than
    [no_scan_tag]. [count] records the numbers of words already visited. *)

let size_of_double = size (repr 1.0)
let count = ref 0

let rec traverse t =
  if not (in_table t) then (
    add_in_table t;
    if is_block t then (
      let n = size t in
      let tag = tag t in
      if tag < no_scan_tag then (
        count := !count + 1 + n;
        for i = 0 to n - 1 do
          let f = field t i in
          if is_block f then traverse f
        done
      ) else if tag = string_tag then
        count := !count + 1 + n
      else if tag = double_tag then
        count := !count + size_of_double
      else if tag = double_array_tag then
        count := !count + 1 + (size_of_double * n)
      else
        incr count
    )
  )

(*s Sizes of objects in words and in bytes. The size in bytes is computed
    system-independently according to [Sys.word_size]. *)

let size_w o =
  reset_table ();
  count := 0;
  traverse (repr o);
  !count

let size_b o = size_w o * (Sys.word_size / 8)
let size_kb o = size_w o / (8192 / Sys.word_size)
