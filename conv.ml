
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

let (@@@) a b = a b

(* error-raising function *)
let __error msg =
  let b = Buffer.create 15 in
  Printf.bprintf b "conversion error: ";
  Printf.kbprintf
    (fun b -> raise (ConversionFailure (Buffer.contents b)))
    b msg

module Sink = struct
  (** A specific sink that requires a given shape to produce
   * a value of type 'a *)
  type 'a t =
    | Unit : 'a -> 'a t
    | Bool : (bool -> 'a) -> 'a t
    | Float : (float -> 'a) -> 'a t
    | Int : (int -> 'a) -> 'a t
    | String : (string -> 'a) -> 'a t
    | List : (('b t -> 'b list) -> 'a) -> 'a t
    | Record : 'a record_sink -> 'a t
    | Tuple : 'a tuple_sink -> 'a t
    | Sum : (string -> ('b t -> 'b) -> 'a) -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Fix : ('a t -> 'a t) -> 'a t

  and 'r record_sink =
    | RecordField : string * 'a t * ('a -> 'r record_sink) -> 'r record_sink
    | RecordStop : 'r -> 'r record_sink

  and 't tuple_sink =
    | TupleField : 'a t * ('a -> 't tuple_sink) -> 't tuple_sink
    | TupleStop : 't -> 't tuple_sink

  and 's sum_sink =
    | SumSink : (string -> ('b t -> 'b) -> 's) -> 's sum_sink

  let rec __expected : type a. a t -> string = function
    | Unit _ -> "unit"
    | Bool _ -> "bool"
    | Float _ -> "float"
    | Int _ -> "int"
    | String _ -> "string"
    | List _ -> "list"
    | Record _ -> "record"
    | Tuple _ -> "tuple"
    | Sum _ -> "sum"
    | Map (sink', _) -> __expected sink'
    | (Fix f) as sink -> __expected (f sink)

  let __id x = x

  let unit_ = Unit ()
  let bool_ = Bool __id
  let float_ = Float __id
  let int_ = Int __id
  let string_ = String __id
  let list_ e =
    List (fun k -> let l = k e in l)

  let map f sink = Map (sink, f)
  let array_ sink =
    map Array.of_list (list_ sink)

  let (-->) a b = a, b
  let (|:|) (name,sink) cont = RecordField (name,sink,cont)
  let yield_record r = RecordStop r
  let record r = Record r
  let record_fix f =
    Fix (fun r -> Record (f r))

  let (|+|) sink cont = TupleField (sink, cont)
  let yield_tuple t = TupleStop t
  let tuple t = Tuple t

  let pair a b =
    tuple (
      a |+| fun x ->
      b |+| fun y ->
      yield_tuple (x,y)
    )

  let triple a b c =
    tuple (
      a |+| fun x ->
      b |+| fun y ->
      c |+| fun z ->
      yield_tuple (x,y,z)
    )

  let quad a b c d =
    tuple (
      a |+| fun x ->
      b |+| fun y ->
      c |+| fun z ->
      d |+| fun w ->
      yield_tuple (x,y,z,w)
    )

  let sum f = Sum f
  let sum_fix f =
    Fix (fun s -> Sum (f s))

  let opt sink = sum (fun name cont ->
      match name with
      | "some" -> Some (cont sink)
      | "none" -> None
      | _ -> __error "unexpected variant %s" name)

  (** Universal sink, such as a serialization format *)
  class type ['a] universal = object
    method unit_ : 'a
    method bool_ : bool -> 'a
    method float_ : float -> 'a
    method int_ : int -> 'a
    method string_ : string -> 'a
    method list_ : 'a list -> 'a
    method record : (string*'a) list -> 'a
    method tuple : 'a list -> 'a
    method sum : string -> 'a list -> 'a
  end
end

module Source = struct
  (** A specific source that follows the shape of the type 'a *)
  type 'a t =
    | Unit : unit t
    | Bool : bool t
    | Float : float t
    | Int : int t
    | String : string t
    | List : 'a t -> 'a list t
    | Record : 'a record_src -> 'a t
    | Tuple : 'a tuple_src -> 'a t
    | Sum : ('a -> string * sum_src) -> 'a t
    | Map : 'a t * ('b -> 'a) -> 'b t
    | Fix : ('a t -> 'a t) -> 'a t

  and 'r record_src =
    | RecordField : string * ('r -> 'a) * 'a t * 'r record_src -> 'r record_src
    | RecordStop : 'r record_src

  and 't tuple_src =
    | TupleField : 'a t * ('t -> 'a) * 't tuple_src -> 't tuple_src
    | TupleStop : 't tuple_src

  and sum_src =
    | SumCons : 'a t * 'a * sum_src -> sum_src
    | SumNil : sum_src

  let unit_ = Unit
  let bool_ = Bool
  let float_ = Float
  let int_ = Int
  let string_ = String
  let list_ e = List e

  let map f src = Map (src, f)
  let array_ src = map Array.to_list (list_ src)

  let record_field name get src' cont =
    RecordField (name,get,src',cont)
  let record_stop = RecordStop
  let record r = Record r
  let record_fix f =
    Fix (fun r -> Record (f r))

  let tuple_field src get cont = TupleField (src,get,cont)
  let tuple_stop = TupleStop
  let tuple t = Tuple t

  let pair a b =
    tuple (tuple_field a fst (tuple_field b snd tuple_stop))

  let triple a b c =
    tuple
    (tuple_field a (fun (a,b,c) -> a)
      (tuple_field b (fun (a,b,c) -> b)
        (tuple_field c (fun (a,b,c) -> c)
          tuple_stop)))

  let quad a b c d =
    tuple
    (tuple_field a (fun (a,b,c,d) -> a)
      (tuple_field b (fun (a,b,c,d) -> b)
        (tuple_field c (fun (a,b,c,d) -> c)
          (tuple_field d (fun (a,b,c,d) -> d)
            tuple_stop))))

  let sum_nil = SumNil
  let sum_cons src' x tl = SumCons (src', x, tl)
  let sum f = Sum f
  let sum_fix f =
    Fix (fun s -> Sum (f s))

  let opt src = sum (function
      | Some x -> "some", sum_cons src x sum_nil
      | None -> "none", sum_nil)

  (* function to look up the given name in an association list *)
  let _get_field l name =
    try List.assoc name l
    with Not_found ->
      __error "record field %s not found in source" name

  class virtual ['a] universal = object(self)
    method private unit_ : 'b. 'b Sink.t -> 'b
      = fun sink -> match sink with
      | Sink.Unit u -> u
      | Sink.Int f -> f 0
      | Sink.Map (sink', f) -> f (self#unit_ sink')
      | Sink.Fix f -> self#unit_ (f sink)
      | _ -> __error "get Unit, but expected %s" (Sink.__expected sink)

    method private bool_ : 'b. 'b Sink.t -> bool -> 'b
      = fun sink b -> match sink with
      | Sink.Bool f -> f b
      | Sink.Int f -> f (if b then 1 else 0)
      | Sink.String f -> f (string_of_bool b)
      | Sink.Map (sink', f) -> f (self#bool_ sink' b)
      | Sink.Fix f -> self#bool_ (f sink) b
      | _ -> __error "get Bool, but expected %s" (Sink.__expected sink)

    method private float_ : 'b. 'b Sink.t -> float -> 'b
      = fun sink x -> match sink with
      | Sink.Float f -> f x
      | Sink.String f -> f (string_of_float x)
      | Sink.Map (sink', f) -> f (self#float_ sink' x)
      | Sink.Fix f -> self#float_ (f sink) x
      | _ -> __error "get Float, but expected %s" (Sink.__expected sink)

    method private int_ : 'b. 'b Sink.t -> int -> 'b
      = fun sink i -> match sink with
      | Sink.Int f -> f i
      | Sink.String f -> f (string_of_int i)
      | Sink.Map (sink', f) -> f (self#int_ sink' i)
      | Sink.Fix f -> self#int_ (f sink) i
      | _ -> __error "get Int, but expected %s" (Sink.__expected sink)

    method private string_ : 'b. 'b Sink.t -> string -> 'b
      = fun sink s -> match sink with
      | Sink.String f -> f s
      | Sink.Int f ->
        begin try f (int_of_string s)
        with Invalid_argument _ -> __error "get String, but expected Int"
        end
      | Sink.Map (sink', f) -> f (self#string_ sink' s)
      | Sink.Fix f -> self#string_ (f sink) s
      | _ -> __error "get String, but expected %s" (Sink.__expected sink)

    method private list_ : 'b. 'b Sink.t -> 'a list -> 'b
      = fun sink l -> match sink with
      | Sink.List f ->
        f (fun sink' -> List.map (self#visit sink') l)
      | Sink.Map (sink', f) -> f (self#list_ sink' l)
      | Sink.Fix f -> self#list_ (f sink) l
      | _ -> __error "get List, but expected %s" (Sink.__expected sink)

    method private record : 'b. 'b Sink.t -> (string*'a) list -> 'b
      = fun sink l -> match sink with
      | Sink.Record r ->
        (* fold over the expected record fields *)
        let rec build_record : 'r. 'r Sink.record_sink -> 'r
        = function
          | Sink.RecordStop x -> x
          | Sink.RecordField (name, sink', cont) ->
              let src_field = _get_field l name in
              let sink_field = self#visit sink' src_field in
              build_record (cont sink_field)
        in build_record r
      | Sink.Map (sink', f) -> f (self#record sink' l)
      | Sink.Fix f -> self#record (f sink) l
      | _ -> __error "get Record, but expected %s" (Sink.__expected sink)

    method private tuple : 'b. 'b Sink.t -> 'a list -> 'b
      = fun sink l -> match sink with
      | Sink.Tuple t_sink ->
        (* fold over the expected tuple component *)
        let rec build_tuple : 't. 'a list -> 't Sink.tuple_sink -> 't
        = fun l t_sink -> match l, t_sink with
          | [], Sink.TupleStop t -> t
          | [], _ ->
              __error "not enough tuple components"
          | _::_, Sink.TupleStop _ ->
              __error "too many tuple components (%d too many)" (List.length l)
          | x::l', Sink.TupleField (sink', cont) ->
              let y = self#visit sink' x in
              build_tuple l' (cont y)
        in build_tuple l t_sink
      | Sink.Map (sink', f) -> f (self#tuple sink' l)
      | Sink.Fix f -> self#tuple (f sink) l
      | _ -> __error "get Tuple, but expected %s" (Sink.__expected sink)

    method private sum : 'b. 'b Sink.t -> string -> 'a -> 'b
      = fun sink name s -> match sink with
      | Sink.Sum f ->
          f name (fun sink' -> self#visit sink' s)
      | Sink.Map (sink', f) -> f (self#sum sink' name s)
      | Sink.Fix f -> self#sum (f sink) name s
      | _ -> __error "get Sum(%s), but expected %s" name (Sink.__expected sink)

    method virtual visit : 'b. 'b Sink.t -> 'a -> 'b
  end
end

let rec into : type a b. a Source.t -> b Sink.universal -> a -> b =
  fun src sink x -> match src with
  | Source.Unit -> sink#unit_
  | Source.Bool -> sink#bool_ x
  | Source.Float -> sink#float_ x
  | Source.Int -> sink#int_ x
  | Source.String -> sink#string_ x
  | Source.List src' ->
      let l = List.map (into src' sink) x in
      sink#list_ l
  | Source.Record r ->
      let rec conv_fields : (string*b)list -> a Source.record_src -> b
      = fun acc r -> match r with
      | Source.RecordStop -> sink#record acc
      | Source.RecordField (name,get,src',r') ->
          let acc = (name, into src' sink (get x)) :: acc in
          conv_fields acc r'
      in conv_fields [] r
  | Source.Tuple t ->
      let rec conv_tuple : b list -> a Source.tuple_src -> b
      = fun acc t -> match t with
      | Source.TupleStop -> sink#tuple (List.rev acc)
      | Source.TupleField (src',get,t') ->
          let acc = into src' sink (get x) :: acc in
          conv_tuple acc t'
      in conv_tuple [] t
  | Source.Sum f ->
      let rec conv_sum : string -> b list -> Source.sum_src -> b
      = fun name acc sum -> match sum with
      | Source.SumNil -> sink#sum name (List.rev acc)
      | Source.SumCons (src',x,sum') ->
          let acc = into src' sink x :: acc in
          conv_sum name acc sum'
      in
      let name, sum = f x in
      conv_sum name [] sum
  | Source.Map (src', f) -> into src' sink (f x)
  | Source.Fix f ->
      let src' = f src in
      into src' sink x

let from (src:'a Source.universal) (sink:'b Sink.t) (x:'a) : 'b = src#visit sink x

(** {6 Exemples} *)

module Json = struct
  type t = [
    | `Int of int
    | `Float of float
    | `Bool of bool
    | `Null
    | `String of string
    | `List of t list
    | `Assoc of (string * t) list
  ]

  let source = object(self)
    inherit [t] Source.universal
    method visit sink (x:t) = match x with
    | `Int i -> self#int_ sink i
    | `Float f -> self#float_ sink f
    | `Bool b -> self#bool_ sink b
    | `Null -> self#unit_ sink
    | `String s -> self#string_ sink s
    | `List l -> self#list_ sink l
    | `Assoc l -> self#record sink l
  end

  let sink : t Sink.universal = object
    method unit_ = `Null
    method bool_ b = `Bool b
    method float_ f = `Float f
    method int_ i = `Int i
    method string_ s = `String s
    method list_ l = `List l
    method record l = `Assoc l
    method tuple l = `List l
    method sum name l = match l with
      | [] -> `String name
      | _::_ -> `List (`String name :: l)
  end
end

(* test for records *)

type point = {
  x:int;
  y:int;
  color:string;
  prev : point option; (* previous position, say *)
}

let rec point_sink =
  Sink.(record_fix
    (fun self ->
      "x" --> int_ |:| fun x ->
      "y" --> int_ |:| fun y ->
      "color" --> string_ |:| fun color ->
      "prev" --> (opt self) |:| fun prev ->
      yield_record {x;y;color;prev}
    ))


let point_source : point Source.t =
  Source.(record_fix
    (fun self ->
      record_field "x" (fun p -> p.x) int_ @@@
      record_field "y" (fun p -> p.y) int_ @@@
      record_field "color" (fun p -> p.color) string_ @@@
      record_field "prev" (fun p -> p.prev) (opt self) @@@
      record_stop
    ))

let p = {x=1; y=42; color="yellow";
         prev = Some {x=1; y=41; color="red"; prev=None};}

let p2 = into point_source Json.sink p
(* TODO tests *)
