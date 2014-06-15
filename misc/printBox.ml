
(*
copyright (c) 2013-2014, simon cruanes
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

(** {1 Pretty-Printing of Boxes} *)

type position = { x:int ; y: int }

let origin = {x=0; y=0;}

let _move pos x y = {x=pos.x + x; y=pos.y + y}
let _add pos1 pos2 = _move pos1 pos2.x pos2.y
let _move_x pos x = _move pos x 0
let _move_y pos y = _move pos 0 y

(** {2 Output: where to print to} *)

module Output = struct
  type t = {
    put_char : position -> char -> unit;
    put_string : position -> string -> unit;
    put_sub_string : position -> string -> int -> int -> unit;
    flush : unit -> unit;
  }

  let put_char out pos c = out.put_char pos c
  let put_string out pos s = out.put_string pos s
  let put_sub_string out pos s s_i s_len = out.put_sub_string pos s s_i s_len

  (** An internal buffer, suitable for writing efficiently, then
      convertable into a list of lines *)
  type buffer = {
    mutable buf_lines : buf_line array;
    mutable buf_len : int;
  }
  and buf_line = {
    mutable bl_str : string;
    mutable bl_len : int;
  }

  let _make_line _ = {bl_str=""; bl_len=0}

  let _ensure_lines buf i =
    if i >= Array.length buf.buf_lines
    then (
      let lines' = Array.init (2 * i + 5) _make_line in
      Array.blit buf.buf_lines 0 lines' 0 buf.buf_len;
      buf.buf_lines <- lines';
    )

  let _ensure_line line i =
    if i >= String.length line.bl_str
    then (
      let str' = String.make (2 * i + 5) ' ' in
      String.blit line.bl_str 0 str' 0 line.bl_len;
      line.bl_str <- str';
    )

  let _buf_put_char buf pos c =
    _ensure_lines buf pos.y;
    _ensure_line buf.buf_lines.(pos.y) pos.x;
    buf.buf_len <- max buf.buf_len (pos.y+1);
    let line = buf.buf_lines.(pos.y) in
    line.bl_str.[pos.x] <- c;
    line.bl_len <- max line.bl_len (pos.x+1)

  let _buf_put_sub_string buf pos s s_i s_len =
    _ensure_lines buf pos.y;
    _ensure_line buf.buf_lines.(pos.y) (pos.x + s_len);
    buf.buf_len <- max buf.buf_len (pos.y+1);
    let line = buf.buf_lines.(pos.y) in
    String.blit s s_i line.bl_str pos.x s_len;
    line.bl_len <- max line.bl_len (pos.x+s_len)

  let _buf_put_string buf pos s =
    _buf_put_sub_string buf pos s 0 (String.length s)

  (* create a new buffer *)
  let make_buffer () =
    let buf = {
      buf_lines = Array.init 16 _make_line;
      buf_len = 0;
    } in
    let buf_out = {
      put_char = _buf_put_char buf;
      put_sub_string = _buf_put_sub_string buf;
      put_string = _buf_put_string buf;
      flush = (fun () -> ());
    } in
    buf, buf_out

  let buf_to_lines ?(indent=0) buf =
    let buffer = Buffer.create (5 + buf.buf_len * 32) in
    for i = 0 to buf.buf_len - 1 do
      for k = 1 to indent do Buffer.add_char buffer ' ' done;
      let line = buf.buf_lines.(i) in
      Buffer.add_substring buffer line.bl_str 0 line.bl_len;
      Buffer.add_char buffer '\n';
    done;
    Buffer.contents buffer

  let buf_output ?(indent=0) oc buf =
    for i = 0 to buf.buf_len - 1 do
      for k = 1 to indent do output_char oc ' '; done;
      let line = buf.buf_lines.(i) in
      output oc line.bl_str 0 line.bl_len;
      output_char oc '\n';
    done
end

(* find [c] in [s], starting at offset [i] *)
let rec _find s c i =
  if i >= String.length s then None
  else if s.[i] = c then Some i
  else _find s c (i+1)

let rec _lines s i k = match _find s '\n' i with
  | None -> ()
  | Some j ->
      let s' = String.sub s i (j-i) in
      k s';
      _lines s (j+1) k

module Box = struct
  type grid_shape =
    | GridBase
    | GridFramed

  type 'a shape =
    | Line of string
    | Text of string list  (* in a box *)
    | Frame of 'a
    | Grid of grid_shape * 'a array array

  type t = {
    shape : t shape;
    size : position lazy_t;
  }

  let size box = Lazy.force box.size

  let shape b = b.shape

  let _array_foldi f acc a =
    let acc = ref acc in
    Array.iteri (fun i x -> acc := f !acc i x) a;
    !acc

  let _dim_matrix m =
    if Array.length m = 0 then {x=0;y=0}
    else {y=Array.length m; x=Array.length m.(0); }

  (* height of a line composed of boxes *)
  let _height_line a =
    _array_foldi
      (fun h i box ->
        let s = size box in
        max h s.y
      ) 0 a

  (* how large is the [i]-th column of [m]? *)
  let _width_column m i =
    let acc = ref 0 in
    for j = 0 to Array.length m - 1 do
      acc := max !acc (size m.(j).(i)).x
    done;
    !acc

  (* from a matrix [m] (line,column), return two arrays [lines] and [columns],
    with [col.(i)] being the start offset of column [i] and
    [lines.(j)] being the start offset of line [j].
    Those arrays have one more slot to indicate the end position. *)
  let _size_matrix m =
    let dim = _dim_matrix m in
    (* columns *)
    let columns = Array.make (dim.x + 1) 0 in
    for i = 0 to dim.x - 1 do
      (* +1 is for keeping room for the vertical/horizontal line/column *)
      columns.(i+1) <- columns.(i) + 1 + (_width_column m i)
    done;
    (* lines *)
    let lines = Array.make (dim.y + 1) 0 in
    for j = 1 to dim.y do
      lines.(j) <- lines.(j-1) + 1 + (_height_line m.(j-1))
    done;
    (* no trailing bars, adjust *)
    columns.(dim.x) <- columns.(dim.x) - 1;
    lines.(dim.y) <- lines.(dim.y) - 1;
    lines, columns

  let _size = function
    | Line s -> { x=String.length s; y=1 }
    | Text l ->
        let width = List.fold_left
          (fun acc line -> max acc (String.length line)) 0 l
        in
        { x=width; y=List.length l; }
    | Frame t ->
        let {x;y} = size t in
        { x=x+2; y=y+2; }
    | Grid (_,m) ->
        let dim = _dim_matrix m in
        let lines, columns = _size_matrix m in
        { y=lines.(dim.y); x=columns.(dim.x)}

  let _make shape =
    { shape; size=(lazy (_size shape)); }

  let line s =
    assert (_find s '\n' 0 = None);
    _make (Line s)

  let text s =
    let acc = ref [] in
    _lines s 0 (fun x -> acc := x :: !acc);
    _make (Text (List.rev !acc))

  let lines l =
    assert (List.for_all (fun s -> _find s '\n' 0 = None) l);
    _make (Text l)

  let frame b = _make (Frame b)

  let grid ?(framed=true) m =
    _make (Grid ((if framed then GridFramed else GridBase), m))

  let init_grid ?framed ~line ~col f =
    let m = Array.init line (fun j-> Array.init col (fun i -> f ~line:j ~col:i)) in
    grid ?framed m

  let vlist ?framed l =
    let a = Array.of_list l in
    grid ?framed (Array.map (fun line -> [| line |]) a)

  let hlist ?framed l =
    grid ?framed [| Array.of_list l |]
end

let _write_vline ~out pos n =
  for j=0 to n-1 do
    Output.put_char out (_move_y pos j) '|'
  done

let _write_hline ~out pos n =
  for i=0 to n-1 do
    Output.put_char out (_move_x pos i) '-'
  done

(* render given box on the output, starting with upper left corner
    at the given position. *)
let rec _render ~out b pos =
  match Box.shape b with
    | Box.Line s -> Output.put_string out pos s
    | Box.Text l ->
        List.iteri
          (fun i line ->
            Output.put_string out (_move_y pos i) line
          ) l
    | Box.Frame b' ->
        let {x;y} = Box.size b' in
        Output.put_char out pos '+';
        Output.put_char out (_move pos (x+1) (y+1)) '+';
        Output.put_char out (_move pos 0 (y+1)) '+';
        Output.put_char out (_move pos (x+1) 0) '+';
        _write_hline out (_move_x pos 1) x;
        _write_hline out (_move pos 1 (y+1)) x;
        _write_vline out (_move_y pos 1) y;
        _write_vline out (_move pos (x+1) 1) y;
        _render ~out b' (_move pos 1 1)
    | Box.Grid (grid_shape,m) ->
        let dim = Box._dim_matrix m in
        let lines, columns = Box._size_matrix m in

        (* write boxes *)
        for j = 0 to dim.y - 1 do
          for i = 0 to dim.x - 1 do
            let pos' = _move pos (columns.(i)) (lines.(j)) in
            _render ~out m.(j).(i) pos'
          done;
        done;

        (* write frame if needed *)
        begin match grid_shape with
        | Box.GridBase -> ()
        | Box.GridFramed ->
          let size = Box.size b in
          for j=1 to dim.y - 1 do
            _write_hline ~out {pos with y=lines.(j)} size.x
          done;
          for i=1 to dim.x - 1 do
            _write_vline ~out {pos with x=columns.(i)} size.y
          done;
        end

let render out b =
  _render ~out b origin

let to_string b =
  let buf, out = Output.make_buffer () in
  render out b;
  Output.buf_to_lines buf

let output ?(indent=0) oc b =
  let buf, out = Output.make_buffer () in
  render out b;
  Output.buf_output ~indent oc buf;
  flush oc
