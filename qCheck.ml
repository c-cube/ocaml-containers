
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

(** {6 Quickcheck inspired property-based testing} *)

module Arbitrary = struct
  type 'a t = Random.State.t -> 'a

  let return x st = x

  let int n st = Random.State.int st n

  let int_range ~start ~stop st =
    let n = stop - start in
    if n <= 0
      then 0
      else start + Random.State.int st n

  let (--) start stop = int_range ~start ~stop

  let small_int = int 100

  let bool = Random.State.bool

  let float f st = Random.State.float st f

  let char st = Char.chr (Random.State.int st 128)

  let alpha st =
    Char.chr (Char.code 'a' + Random.State.int st (Char.code 'z' - Char.code 'a'))

  let string_len len st =
    let n = len st in
    assert (n>=0);
    let s = String.create n in
    for i = 0 to n-1 do
      s.[i] <- alpha st
    done;
    s

  let string st = string_len (int 10) st

  let map ar f st = f (ar st)

  let rec _make_list ar st acc n =
    if n = 0 then acc else
      let x = ar st in
      _make_list ar st (x::acc) (n-1)

  let list ?(len=int 10) ar st =
    let n = len st in
    _make_list ar st [] n

  let opt ar st =
    if Random.State.bool st
      then Some (ar st)
      else None

  let list_repeat len ar st =
    _make_list ar st [] len

  let array ?(len=int 10) ar st =
    let n = len st in
    Array.init n (fun _ -> ar st)

  let array_repeat n ar st =
    Array.init n (fun _ -> ar st)

  let among_array a st =
    let i = Random.State.int st (Array.length a) in
    a.(i)

  let among l = among_array (Array.of_list l)

  let choose l =
    assert (l <> []);
    let a = Array.of_list l in
    fun st ->
      let i = Random.State.int st (Array.length a) in
      a.(i) st

  let _fix ~max ~depth recursive f =
    let rec ar = lazy (fun st -> (Lazy.force ar_rec) st)
    and ar_rec = lazy (f ar) in
    Lazy.force ar

  let fix ?(max=max_int) ~base f =
    let rec ar = lazy
      (fun depth st ->
        if depth >= max || Random.State.int st max < depth
          then base st (* base case. THe deeper, the more likely. *)
        else  (* recurse *)
          let ar' = Lazy.force ar (depth+1) in
          f ar' st)
    in
    Lazy.force ar 0

  let fix_depth ~depth ~base f st =
    let max = depth st in
    fix ~max ~base f st

  let lift f a st = f (a st)

  let lift2 f a b st = f (a st) (b st)

  let lift3 f a b c st = f (a st) (b st) (c st)

  let lift4 f a b c d st = f (a st) (b st) (c st) (d st)

  let pair a b = lift2 (fun x y -> x,y) a b

  let triple a b c = lift3 (fun x y z -> x,y,z) a b c

  let quad a b c d = lift4 (fun x y z w -> x,y,z,w) a b c d

  let (>>=) a f st =
    let x = a st in
    f x st

  let generate ?(n=100) ?(rand=Random.State.make_self_init()) gen =
    let l = ref [] in
    for i = 0 to n-1 do
      l := (gen rand) :: !l
    done;
    !l
end

(** {2 Pretty printing} *)

module PP = struct
  type 'a t = 'a -> string

  let int = string_of_int
  let bool = string_of_bool
  let float = string_of_float
  let string s = s
  let char c =
    let s = "_" in
    s.[0] <- c;
    s

  let pair a b (x,y) = Printf.sprintf "(%s, %s)" (a x) (b y)
  let triple a b c (x,y,z) = Printf.sprintf "(%s, %s, %s)" (a x) (b y) (c z)
  let quad a b c d (x,y,z,w) =
    Printf.sprintf "(%s, %s, %s, %s)" (a x) (b y) (c z) (d w)

  let list pp l =
    let b = Buffer.create 25 in
    Buffer.add_char b '(';
    List.iteri (fun i x ->
      if i > 0 then Buffer.add_string b ", ";
      Buffer.add_string b (pp x))
      l;
    Buffer.add_char b ')';
    Buffer.contents b

  let array pp a = 
    let b = Buffer.create 25 in
    Buffer.add_char b '[';
    Array.iteri (fun i x ->
      if i > 0 then Buffer.add_string b ", ";
      Buffer.add_string b (pp x))
      a;
    Buffer.add_char b ']';
    Buffer.contents b
end

(** {2 Testing} *)

module Prop = struct
  type 'a t = 'a -> bool

  exception PrecondFail
  
  let assume p =
    if not p then raise PrecondFail

  let assume_lazy (lazy p) =
    if not p then raise PrecondFail

  let (==>) a b =
    fun x ->
      assume (a x);
      b x

  let (&&&) a b x = a x && b x

  let (|||) a b x = a x || b x

  let (!!!) a x = not (a x)
end

type 'a result =
  | Ok of int * int  (* total number / precond failed *)
  | Failed of 'a list
  | Error of exn

(* random seed, for repeatability of tests *)
let __seed = [| 89809344; 994326685; 290180182 |]

let check ?(rand=Random.State.make __seed) ?(n=100) gen prop =
  let precond_failed = ref 0 in
  let failures = ref [] in
  try
    for i = 0 to n - 1 do
      let x = gen rand in
      try
        if not (prop x)
          then failures := x :: !failures
      with Prop.PrecondFail ->
        incr precond_failed
    done;
    match !failures with
    | [] -> Ok (n, !precond_failed)
    | _ -> Failed (!failures)
  with e ->
    Error e

(** {2 Main} *)

type 'a test_cell = {
  n : int;
  pp : 'a PP.t option;
  prop : 'a Prop.t;
  gen : 'a Arbitrary.t;
  name : string;
}
type test =
  | Test : 'a test_cell -> test
  (** GADT needed for the existential type *)

let mk_test ?(n=100) ?pp ?(name="<anon prop>") gen prop =
  Test { prop; gen; name; n; pp; }

let run ?(out=stdout) ?(rand=Random.State.make __seed) (Test test) =
  Printf.fprintf out "testing property %s...\n" test.name;
  match check ~rand ~n:test.n test.gen test.prop with
  | Ok (n, prefail) ->
    Printf.fprintf out "  [✔] passed %d tests (%d preconditions failed)\n" n prefail;
    true
  | Failed l ->
    begin match test.pp with
    | None -> Printf.fprintf out "  [×] %d failures\n" (List.length l)
    | Some pp ->
      Printf.fprintf out "  [×] %d failures:\n" (List.length l);
      List.iter
        (fun x -> Printf.fprintf out "  %s\n" (pp x))
        l
    end;
    false
  | Error e ->
    Printf.fprintf out "  [×] error: %s\n" (Printexc.to_string e);
    false

type suite = test list

let flatten = List.flatten

let run_tests ?(out=stdout) ?(rand=Random.State.make __seed) l =
  let start = Unix.gettimeofday () in
  let failed = ref 0 in
  Printf.fprintf out "check %d properties...\n" (List.length l);
  List.iter (fun test -> if not (run ~out ~rand test) then incr failed) l;
  Printf.fprintf out "tests run in %.2fs\n" (Unix.gettimeofday() -. start);
  if !failed = 0
    then Printf.fprintf out "[✔] Success!\n"
    else Printf.fprintf out "[×] Failure (%d tests failed).\n" !failed;
  !failed = 0
