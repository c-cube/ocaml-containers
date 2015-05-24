
(*
copyright (c) 2013-2015, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Set of Heterogeneous Values}

    {[
      let k1 : int key = newkey () in
      let k2 : int key = newkey () in
      let k3 : string key = newkey () in
      let set =
        empty
        |> set ~key:k1 1
        |> set ~key:k2 2
        |> set ~key:k3 "3"
      in
      assert (get ~key:k1 set = Some 1);
      assert (get ~key:k2 set = Some 2);
      assert (get ~key:k3 set = Some "3");
      ()
    ]}

    @since 0.11 *)

type t
(** A set of values of heterogeneous types *)

type 'a key
(** A unique "key" to access a value of type ['a] in a [set] *)

val newkey : unit -> 'a key
(** [newkey ()] creates a new unique key that can be used to access
    a ['a] value in a set. Each key created with [newkey] is distinct
    from any other key, even if they have the same type.

    Not thread-safe. *)

val empty : t
(** Empty set *)

val set : key:'a key -> 'a -> t -> t
(** [set ~key v set] maps [key] to [v] in [set]. It means that
    for every [set], [get ~key (set ~key v set) = Some v]. *)

val get : key:'a key -> t -> 'a option
(** [get ~key set] obtains the value for [key] in [set], if any. *)

val get_exn : key:'a key -> t -> 'a
(** Same as {!get}, but can fail
    @raise Not_found if the key is not present *)

val cardinal : t -> int
(** Number of mappings *)


