
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

(** {6 Action Language for command line} *)

module Action = struct
  type trigger = string

  type _ t =
    | Return : 'a -> 'a t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Ignore : ('a t * 'b t) -> 'b t
    | Any : string t
    | ReadInt : (int -> 'a t) -> 'a t
    | ReadString : (string -> 'a t) -> 'a t
    | ReadBool : (bool -> 'a t) -> 'a t
    | Choice : 'a t list -> 'a t
    | Fail : string -> 'a t

  let return x = Return x
  
  let (>>=) x f = Bind (x, f)

  let (>>) x f = Bind (x, (fun _ -> f ()))

  let ( *>) a b = Ignore (a, b)

  let ignore x = x *> return ()

  let any = Any

  let accept trigger =
    Any >>= fun x ->
      if x = trigger
        then return ()
        else Fail ("expected trigger \"" ^ trigger ^ "\"")

  let with_string ?trigger f =
    match trigger with
    | None -> ReadString f
    | Some t -> accept t *> ReadString f

  let with_int ?trigger f =
    match trigger with
    | None -> ReadInt f
    | Some t -> accept t *> ReadInt f

  let with_bool ?trigger f =
    match trigger with
    | None -> ReadBool f
    | Some t -> accept t *> ReadBool f

  let choice l = Choice l

  let repeat act =
    let rec try_next acc =
      choice
        [ act >>= (fun x -> try_next (x::acc))
        ; return acc
        ]
    in
    (try_next []) >>= (fun l -> return (List.rev l))

  let opt act =
    choice [ act >>= (fun x -> return (Some x)); return None ]
    
  let fail msg = Fail msg
end

type 'a result =
  | Ok of 'a
  | Error of string

type 'a partial_result =
  | POk of 'a * int   (* value and position in args *)
  | PError of string  (* error message *)

let parse_args args (act : 'a Action.t) : 'a result =
  let module A = Action in
  (* interpret recursively, with backtracking. Returns partial result *)
  let rec interpret : type a. string array -> int -> a Action.t -> a partial_result
  = fun args i act ->
    let n = Array.length args in
    match act with
    | A.Return x -> POk (x, i)
    | A.Bind (x, f) ->
      begin match interpret args i x with
      | POk (x, i') -> interpret args i' (f x)
      | PError msg -> PError msg
      end
    | A.Ignore (a, b) ->
      begin match interpret args i a with
      | POk (_, i') -> interpret args i' b
      | PError msg -> PError msg
      end
    | A.Any when i >= n -> mk_error i "expected [any], reached end"
    | A.Any -> POk (args.(i), i+1)
    | A.ReadInt f when i >= n -> mk_error i "expected [int], reached end"
    | A.ReadInt f ->
      begin try
        let j = int_of_string args.(i) in
        interpret args (i+1) (f j)
      with Failure _ -> mk_error i "expected [int]"
      end
    | A.ReadString _ when i >= n -> mk_error i "expected [string], reached end"
    | A.ReadString f -> interpret args (i+1) (f args.(i))
    | A.ReadBool _ -> failwith "not implemented: read bool" (* TODO *)
    | A.Fail msg -> mk_error i msg
    | A.Choice l -> try_choices args i [] l
  (* try the actions remaining in [l], whenre [errors] is the list
      of errors in already tried branches *)
  and try_choices : type a. string array -> int -> string list -> a Action.t list -> a partial_result 
  = fun args i errors l ->
    match l with
    | [] ->
      let msg = Printf.sprintf "choice failed: [%s]" (String.concat " | " errors) in
      mk_error i msg
    | act::l' ->
      begin match interpret args i act with
      | POk _ as res -> res   (* success! *)
      | PError msg ->
        try_choices args i (msg :: errors) l'
      end
  (* report error *)
  and mk_error : type a. int -> string -> a partial_result
  = fun i msg ->
    PError (Printf.sprintf "at arg %d: %s" i msg)
  in
  match interpret args 1 act with
  | POk (x,_) -> Ok x
  | PError msg -> Error msg

let parse act = parse_args Sys.argv act

let print_doc oc act =
  failwith "print_doc: not implemented"
