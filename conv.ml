
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

(* error-raising function *)
let __error msg =
  let b = Buffer.create 15 in
  Printf.bprintf b "conversion error: ";
  Printf.kbprintf
    (fun b -> raise (ConversionFailure (Buffer.contents b)))
    b msg

module Sink = struct
  (** A specific sink that requires a given shape to produce
      a value of type 'a *)
  type 'a t =
    | Unit : unit t
    | Bool : bool t
    | Float : float t
    | Int : int t
    | String : string t
    | List : (('b t -> 'b list) -> 'a) -> 'a t
    | Record : 'a record_sink -> 'a t
    | Tuple : 'a hlist -> 'a t
    | Sum : (string -> 'a hlist) -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Fix : ('a t -> 'a t) -> 'a t

  and 'r record_sink =
    | RecordField : string * 'a t * ('a -> 'r record_sink) -> 'r record_sink
    | RecordStop : 'r -> 'r record_sink

  and 't hlist =
    | HCons : 'a t * ('a -> 't hlist) -> 't hlist
    | HNil : 't -> 't hlist

  let rec __expected : type a. a t -> string = function
    | Unit -> "unit"
    | Bool -> "bool"
    | Float -> "float"
    | Int -> "int"
    | String -> "string"
    | List _ -> "list"
    | Record _ -> "record"
    | Tuple _ -> "tuple"
    | Sum _ -> "sum"
    | Map (sink', _) -> __expected sink'
    | (Fix f) as sink -> __expected (f sink)

  let unit_ = Unit
  let bool_ = Bool
  let float_ = Float
  let int_ = Int
  let string_ = String
  let list_ e =
    List (fun k -> let l = k e in l)

  let map f sink = Map (sink, f)
  let array_ sink =
    map Array.of_list (list_ sink)

  let field name sink cont = RecordField (name, sink, cont)
  let yield_record r = RecordStop r
  let record r = Record r
  let record_fix f =
    let rec r = lazy (Fix (fun _ -> Record (f (Lazy.force r)))) in
    Lazy.force r

  let (|+|) sink cont = HCons (sink, cont)
  let yield t = HNil t

  let tuple t = Tuple t

  let pair a b =
    tuple (
      a |+| fun x ->
      b |+| fun y ->
      yield (x,y)
    )

  let triple a b c =
    tuple (
      a |+| fun x ->
      b |+| fun y ->
      c |+| fun z ->
      yield (x,y,z)
    )

  let quad a b c d =
    tuple (
      a |+| fun x ->
      b |+| fun y ->
      c |+| fun z ->
      d |+| fun w ->
      yield (x,y,z,w)
    )

  let sum f = Sum f
  let sum_fix f =
    Fix (fun s -> Sum (f s))

  let opt sink = sum (fun name ->
      match name with
      | "some" -> sink |+| fun x -> yield (Some x)
      | "none" -> yield None
      | _ -> __error "unexpected variant %s" name)

  (** What is expected by the sink? *)
  type expected =
    | ExpectInt
    | ExpectBool
    | ExpectUnit
    | ExpectFloat
    | ExpectString
    | ExpectRecord
    | ExpectTuple
    | ExpectList
    | ExpectSum

  let rec expected : type a. a t -> expected = function
    | Unit -> ExpectUnit
    | Bool -> ExpectBool
    | Int -> ExpectInt
    | Float -> ExpectFloat
    | String -> ExpectString
    | Record _ -> ExpectRecord
    | Tuple _ -> ExpectTuple
    | Sum _ -> ExpectSum
    | List _ -> ExpectList
    | (Fix f) as sink -> expected (f sink)
    | Map (sink', _) -> expected sink'

  (** Universal sink, such as a serialization format *)
  module Universal = struct
    type 'a t = {
      unit_ : 'a;
      bool_ : bool -> 'a;
      float_ : float -> 'a;
      int_ : int -> 'a;
      string_ : string -> 'a;
      list_ : 'a list -> 'a;
      record : (string*'a) list -> 'a;
      tuple : 'a list -> 'a;
      sum : string -> 'a list -> 'a;
    }
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

  let field name get src' cont =
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

  module Universal = struct
    type 'a t = {
      visit : 'b. 'b Sink.t -> 'a -> 'b;
    }

    let rec unit_ : type b. b Sink.t -> b
    = fun sink -> match sink with
      | Sink.Unit -> ()
      | Sink.Int -> 0
      | Sink.Map (sink', f) -> f (unit_ sink')
      | Sink.Fix f -> unit_ (f sink)
      | _ -> __error "get Unit, but expected %s" (Sink.__expected sink)

    let rec bool_ : type b. b Sink.t -> bool -> b
    = fun sink b -> match sink with
      | Sink.Bool -> b
      | Sink.Int -> if b then 1 else 0
      | Sink.String -> string_of_bool b
      | Sink.Map (sink', f) -> f (bool_ sink' b)
      | Sink.Fix f -> bool_ (f sink) b
      | _ -> __error "get Bool, but expected %s" (Sink.__expected sink)

    let rec float_ : type b. b Sink.t -> float -> b
    = fun sink x -> match sink with
      | Sink.Float -> x
      | Sink.String -> string_of_float x
      | Sink.Map (sink', f) -> f (float_ sink' x)
      | Sink.Fix f -> float_ (f sink) x
      | _ -> __error "get Float, but expected %s" (Sink.__expected sink)

    let rec int_ : type b. b Sink.t -> int -> b
    = fun sink i -> match sink with
      | Sink.Int -> i
      | Sink.Bool -> i <> 0
      | Sink.String -> string_of_int i
      | Sink.Map (sink', f) -> f (int_ sink' i)
      | Sink.Fix f -> int_ (f sink) i
      | _ -> __error "get Int, but expected %s" (Sink.__expected sink)

    let rec string_ : type b. b Sink.t -> string -> b
    = fun sink s -> match sink with
      | Sink.String -> s
      | Sink.Int ->
        begin try int_of_string s
        with Invalid_argument _ -> __error "get String, but expected Int"
        end
      | Sink.Bool ->
        begin try bool_of_string s
        with Invalid_argument _ -> __error "get String, but expected Bool"
        end
      | Sink.Float ->
        begin try float_of_string s
        with Invalid_argument _ -> __error "get String, but expected Float"
        end
      | Sink.Map (sink', f) -> f (string_ sink' s)
      | Sink.Fix f -> string_ (f sink) s
      | _ -> __error "get String, but expected %s" (Sink.__expected sink)

    let rec list_ : type b. src:'a t -> b Sink.t -> 'a list -> b
    = fun ~src sink l -> match sink with
      | Sink.List f ->
        f (fun sink' -> List.map (src.visit sink') l)
      | Sink.Tuple _ -> tuple ~src sink l
      | Sink.Map (sink', f) -> f (list_ ~src sink' l)
      | Sink.Fix f -> list_ ~src (f sink) l
      | _ -> __error "get List, but expected %s" (Sink.__expected sink)

    and record : type b. src:'a t -> b Sink.t -> (string*'a) list -> b
    = fun ~src sink l -> match sink with
      | Sink.Record r ->
        (* fold over the expected record fields *)
        let rec build_record 
        = function
          | Sink.RecordStop x -> x
          | Sink.RecordField (name, sink', cont) ->
              let src_field = _get_field l name in
              let sink_field = src.visit sink' src_field in
              build_record (cont sink_field)
        in build_record r
      | Sink.Map (sink', f) -> f (record ~src sink' l)
      | Sink.Fix f -> record ~src (f sink) l
      | _ -> __error "get Record, but expected %s" (Sink.__expected sink)

    and build_hlist : 't. src:'a t -> 'a list -> 't Sink.hlist -> 't
        = fun ~src l t_sink -> match l, t_sink with
          | [], Sink.HNil t -> t
          | [], _ ->
              __error "not enough tuple components"
          | _::_, Sink.HNil _ ->
              __error "too many tuple components (%d too many)" (List.length l)
          | x::l', Sink.HCons (sink', cont) ->
              let y = src.visit sink' x in
              build_hlist ~src l' (cont y)

    and tuple : type b. src:'a t -> b Sink.t -> 'a list -> b
    = fun ~src sink l -> match sink with
      | Sink.Tuple t_sink ->
        (* fold over the expected tuple component *)
        build_hlist ~src l t_sink
      | Sink.List _ -> list_ ~src sink l  (* adapt *)
      | Sink.Map (sink', f) -> f (tuple ~src sink' l)
      | Sink.Fix f -> tuple ~src (f sink) l
      | _ -> __error "get Tuple, but expected %s" (Sink.__expected sink)

    and sum : type b. src:'a t -> b Sink.t -> string -> 'a list -> b
    = fun ~src sink name s -> match sink with
      | Sink.Sum f ->
          let l_sink = f name in
          build_hlist ~src s l_sink
      | Sink.Map (sink', f) -> f (sum ~src sink' name s)
      | Sink.Fix f -> sum ~src (f sink) name s
      | _ -> __error "get Sum(%s), but expected %s" name (Sink.__expected sink)
  end
end

let rec into : type a b. a Source.t -> b Sink.Universal.t -> a -> b =
  let open Sink.Universal in
  fun src sink x -> match src with
  | Source.Unit -> sink.unit_
  | Source.Bool -> sink.bool_ x
  | Source.Float -> sink.float_ x
  | Source.Int -> sink.int_ x
  | Source.String -> sink.string_ x
  | Source.List src' ->
      let l = List.map (into src' sink) x in
      sink.list_ l
  | Source.Record r ->
      let rec conv_fields : (string*b)list -> a Source.record_src -> b
      = fun acc r -> match r with
      | Source.RecordStop -> sink.record (List.rev acc)
      | Source.RecordField (name,get,src',r') ->
          let acc = (name, into src' sink (get x)) :: acc in
          conv_fields acc r'
      in conv_fields [] r
  | Source.Tuple t ->
      let rec conv_tuple : b list -> a Source.tuple_src -> b
      = fun acc t -> match t with
      | Source.TupleStop -> sink.tuple (List.rev acc)
      | Source.TupleField (src',get,t') ->
          let acc = into src' sink (get x) :: acc in
          conv_tuple acc t'
      in conv_tuple [] t
  | Source.Sum f ->
      let rec conv_sum : string -> b list -> Source.sum_src -> b
      = fun name acc sum -> match sum with
      | Source.SumNil -> sink.sum name (List.rev acc)
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

let from (src:'a Source.Universal.t) (sink:'b Sink.t) (x:'a) : 'b =
  src.Source.Universal.visit sink x

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

  let source =
    let module U = Source.Universal in
    let rec visit : type b. b Sink.t -> t -> b =
    fun sink x -> match x with
      | `Int i -> U.int_ sink i
      | `Float f -> U.float_ sink f
      | `Bool b -> U.bool_ sink b
      | `Null -> U.unit_ sink
      | `String s ->
          begin match Sink.expected sink with
          | Sink.ExpectSum -> U.sum ~src sink s []
          | _ -> U.string_ sink s
          end
      | `List ((`String name :: l) as l') ->
          begin match Sink.expected sink with
          | Sink.ExpectSum -> U.sum ~src sink name l
          | _ -> U.list_ ~src sink l'
          end
      | `List l -> U.list_ ~src sink l
      | `Assoc l -> U.record ~src sink l
    and src = { U.visit=visit; } in
    src

  let sink : t Sink.Universal.t =
    let open Sink.Universal in
    { unit_ = `Null;
      bool_ = (fun b -> `Bool b);
      float_ = (fun f -> `Float f);
      int_ = (fun i -> `Int i);
      string_ = (fun s -> `String s);
      list_ = (fun l -> `List l);
      record = (fun l -> `Assoc l);
      tuple = (fun l -> `List l);
      sum = (fun name l -> match l with
        | [] -> `String name
        | _::_ -> `List (`String name :: l));
    }
end

module Sexp = struct
  type t =
    | Atom of string
    | List of t list

  let source =
    let module U = Source.Universal in
    let rec visit : type b. b Sink.t -> t -> b =
    fun sink x -> match x, Sink.expected sink with
      | Atom s, Sink.ExpectSum -> U.sum ~src sink s []
      | List (Atom name :: l), Sink.ExpectSum -> U.sum ~src sink name l
      | List l, Sink.ExpectRecord ->
          let l' = List.map (function
            | List [Atom name; x] -> name, x
            | _ -> __error "get List, but expected Record") l
          in U.record ~src sink l'
      | Atom s, _ -> U.string_ sink s
      | List [], Sink.ExpectUnit -> U.unit_ sink
      | List l, _ -> U.list_ ~src sink l
    and src = { U.visit=visit; } in
    src

  let sink =
    let open Sink.Universal in
    { unit_ = List [];
      bool_ = (fun b -> Atom (string_of_bool b));
      float_ = (fun f -> Atom (string_of_float f));
      int_ = (fun i -> Atom (string_of_int i));
      string_ = (fun s -> Atom (String.escaped s));
      list_ = (fun l -> List l);
      record = (fun l -> List (List.map (fun (a,b) -> List [Atom a; b]) l));
      tuple = (fun l -> List l);
      sum = (fun name l -> match l with
        | [] -> Atom name
        | _::_ -> List (Atom name :: l));
    }

  let rec fmt out = function
    | Atom s -> Format.pp_print_string out s
    | List l ->
        Format.pp_print_char out '(';
        List.iteri (fun i s ->
          if i > 0 then Format.pp_print_char out ' ';
          fmt out s) l;
        Format.pp_print_char out ')'
end

module Bencode = struct
  type t =
    | Int of int
    | String of string
    | List of t list
    | Assoc of (string * t) list

  let source =
    let module U = Source.Universal in
    let rec visit : type b. b Sink.t -> t -> b =
    fun sink x -> match x, Sink.expected sink with
      | String s, Sink.ExpectSum -> U.sum ~src sink s []
      | List (String name :: l), Sink.ExpectSum -> U.sum ~src sink name l
      | Assoc l, _ -> U.record ~src sink l
      | String s, _ -> U.string_ sink s
      | List [], Sink.ExpectUnit -> U.unit_ sink
      | List l, _ -> U.list_ ~src sink l
      | Int 0, Sink.ExpectUnit -> U.unit_ sink
      | Int i, _ -> U.int_ sink i
    and src = { U.visit=visit; } in
    src

  let sink =
    let open Sink.Universal in
    { unit_ = Int 0;
      bool_ = (fun b -> Int (if b then 1 else 0));
      float_ = (fun f -> String (string_of_float f));
      int_ = (fun i -> Int i);
      string_ = (fun s -> String s);
      list_ = (fun l -> List l);
      record = (fun l -> Assoc l);
      tuple = (fun l -> List l);
      sum = (fun name l -> match l with
        | [] -> String name
        | _::_ -> List (String name :: l));
    }
end

(* tests *)

module Point = struct
  type t = {
    x : int;
    y : int;
    color : string;
    prev : t option; (* previous position, say *)
  }

  let sink =
    Sink.(record_fix
      (fun self ->
        field "x" int_ @@ fun x ->
        field "y" int_ @@ fun y ->
        field "color" string_ @@ fun color ->
        field "prev" (opt self) @@ fun prev ->
        yield_record {x;y;color;prev}
      ))

  let source =
    Source.(record_fix
      (fun self ->
        field "x" (fun p -> p.x) int_ @@
        field "y" (fun p -> p.y) int_ @@
        field "color" (fun p -> p.color) string_ @@
        field "prev" (fun p -> p.prev) (opt self) @@
        record_stop
      ))

  let p = {x=1; y=42; color="yellow";
           prev = Some {x=1; y=41; color="red"; prev=None};}

  let p2 = into source Json.sink p

  let p3 = from Json.source sink p2

  let p4 = into source Json.sink p3

  let p2_sexp = into source Sexp.sink p

  let p3_sexp = from Sexp.source sink p2_sexp

  let p4_sexp = into source Sexp.sink p3_sexp
end

module Lambda = struct
  type t =
    | Var of string
    | App of t * t
    | Lambda of string * t

  let source = Source.(sum_fix
    (fun self t -> match t with
        | Var s -> "var", sum_cons string_ s @@ sum_nil
        | App (t1, t2) -> "app", sum_cons self t1 @@ sum_cons self t2 @@ sum_nil
        | Lambda (s, t) -> "lam", sum_cons string_ s @@ sum_cons self t @@ sum_nil
      ))

  let sink = Sink.(sum_fix
    (fun self str -> match str with
      | "var" -> string_ |+| fun s -> yield (Var s)
      | "app" -> self |+| fun t1 -> self |+| fun t2 -> yield (App (t1, t2))
      | "lam" -> string_ |+| fun s -> self |+| fun t -> yield (Lambda (s, t))
      | _ -> __error "expected lambda term"
    ))

  let t1 = Lambda ("x", App (Lambda ("y", App (Var "y", Var "x")), Var "x"))

  let t1_json = into source Json.sink t1
  let t1_bencode = into source Bencode.sink t1
  let t1_sexp = into source Sexp.sink t1
end
