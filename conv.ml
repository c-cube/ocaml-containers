
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

(** {1 Bidirectional Conversion} *)

exception ConversionFailure of string

module Sink = struct
  type 'a t =
    | Int : (int -> 'a) -> 'a t
    | String : (string -> 'a) -> 'a t
    | List : (('b t -> 'b list) -> 'a) -> 'a t
    | RecordField : string * 'b t * ('b -> 'a t) -> 'a t
    | Return : 'a -> 'a t

  let return x = Return x

  let int_ = Int (fun i -> i)
  let string_ = String (fun s -> s)

  let list_ e =
    List (fun k -> let l = k e in l)
end

module Source = struct
  type 'a t = {
    convert : 'b. 'b Sink.t -> 'a -> 'b;
  }

  let int_ =
    let convert sink x = match sink with
    | Sink.Int f -> f x
    | _ -> raise (ConversionFailure "expected int")
    in { convert; }

  let string_ =
    let convert sink x = match sink with
    | Sink.String f -> f x
    | _ -> raise (ConversionFailure "expected string")
    in { convert; }

  let list_ e =
    let convert sink l = match sink with
      | Sink.List f -> f (fun sink' -> List.map (e.convert sink') l)
      | _ -> raise (ConversionFailure "expected list")
    in
    { convert; }
end

let conv ~src ~sink x = src.Source.convert sink x

(* test for records
 * TODO: continue, and provide a good API for records,
 * recursive types and tuples. *)

type point = {
  x:int;
  color:string;
}

let point_sink : point Sink.t =
  Sink.RecordField ("x", Sink.int_,
    (fun x -> Sink.RecordField ("color", Sink.string_,
      (fun color -> Sink.return {x;color;}))))

type 'r record_source =
  | RSStop : 'r record_source
  | RSField : string * ('r -> 'a) * 'a Source.t * 'r record_source -> 'r record_source

let mk_record_src rs =
  let rec convert : 'b. 'b Sink.t -> 'a -> 'b
  = fun sink x -> match sink with
  | Sink.Return y -> y
  | Sink.RecordField (name, sink', kont) ->
      search rs name sink' x kont
  | _ -> raise (ConversionFailure "expected record")
  and search
  : 'b 'c. 'a record_source -> string -> 'b Sink.t -> 'a -> ('b -> 'c Sink.t) -> 'c 
  = fun rs name sink' x kont -> match rs with
  | RSStop -> raise (ConversionFailure ("could not find field "^name))
  | RSField (name', get, src, _) when name = name' ->
      (* use source to encode x *)
      let new_sink = kont (conv ~src ~sink:sink' (get x)) in
      convert new_sink x
  | RSField (_, _, _, rs') ->
      (* search further *)
      search rs' name sink' x kont
  in Source.({ convert; })

let point_source : point Source.t =
  mk_record_src (
    RSField ("x", (fun p -> p.x), Source.int_,
    RSField ("color", (fun p -> p.color), Source.string_,
    RSStop ))
  )

let p = {x=1; color="yellow"; }

