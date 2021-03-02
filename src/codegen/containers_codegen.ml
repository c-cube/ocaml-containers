
(** {1 Code generators} *)

module Fmt = CCFormat
let spf = Printf.sprintf
let fpf = Fmt.fprintf

type code =
  | Base of { pp: unit Fmt.printer }
  | Struct of string * code list
  | Sig of string * code list

module Code = struct
  type t = code

  let in_struct m (cs:t list) : t = Struct (m, cs)
  let in_sig m (cs:t list) : t = Sig (m, cs)

  let rec pp_rec out c =
    let ppl = Fmt.(list ~sep:(return "@ ") pp_rec) in
    match c with
      | Base {pp} -> pp out ()
      | Struct (m,cs) ->
        fpf out "@[<hv2>module %s = struct@ %a@;<1 -2>end@]" m ppl cs
      | Sig (m,cs) ->
        fpf out "@[<hv2>module %s : sig@ %a@;<1 -2>end@]" m ppl cs

  let pp out c = fpf out "@[<v>%a@]" pp_rec c
  let to_string c = Fmt.to_string pp c

  let mk_pp pp = Base {pp}
  let mk_str s = Base {pp=Fmt.const Fmt.string s}
end

module Bitfield = struct
  type field = {
    f_name: string;
    f_offset: int;
    f_def: field_def;
  }
  and field_def =
    | F_bit
    | F_int of {width: int}

  type t = {
    name: string;
    mutable fields: field list;
    mutable width: int;
    emit_failure_if_too_wide: bool;
  }

  let make ?(emit_failure_if_too_wide=true) ~name () : t =
    { name; fields=[]; width=0; emit_failure_if_too_wide; }

  let total_width self = self.width

  let field_bit self f_name =
    let f_offset = total_width self in
    let f = {f_name; f_offset; f_def=F_bit} in
    self.fields <- f :: self.fields;
    self.width <- 1 + self.width

  let field_int self ~width f_name : unit =
    let f_offset = total_width self in
    let f = {f_name; f_offset; f_def=F_int {width}} in
    self.fields <- f :: self.fields;
    self.width <- self.width + width

  let empty_name self =
    if self.name = "t" then "empty" else spf "empty_%s" self.name

  let gen_ml self : code =
    Code.mk_pp @@ fun out () ->
    fpf out "@[<v>type %s = int@," self.name;
    fpf out "@[let %s : %s = 0@]@," (empty_name self) self.name;
    List.iter
      (fun f ->
         let inline = "[@inline]" in (* TODO: option to enable/disable that *)
         let off = f.f_offset in
         match f.f_def with
         | F_bit ->
           let x_lsr = if off = 0 then "x" else spf "(x lsr %d)" off in
           fpf out "@[let%s get_%s (x:%s) : bool = (%s land 1) <> 0@]@,"
             inline f.f_name self.name x_lsr;
           let mask_shifted = 1 lsl off in
           fpf out "@[<2>let%s set_%s (v:bool) (x:%s) : %s =@ \
                    if v then x lor %d else x land (lnot %d)@]@,"
             inline f.f_name self.name self.name mask_shifted mask_shifted;
         | F_int {width} ->
           let mask0 = (1 lsl width) - 1 in
           fpf out "@[let%s get_%s (x:%s) : int = ((x lsr %d) land %d)@]@,"
             inline f.f_name self.name off mask0;
           fpf out "@[<2>let%s set_%s (i:int) (x:%s) : %s =@ \
                    assert ((i land %d) == i);@ \
                    ((x land (lnot %d)) lor (i lsl %d))@]@,"
             inline f.f_name self.name self.name
             mask0 (mask0 lsl off) off;
      )
      (List.rev self.fields);
    (* check width *)
    if self.emit_failure_if_too_wide then (
      fpf out "(* check that int size is big enough *)@,\
               @[let () = assert (Sys.int_size >= %d);;@]" (total_width self);
    );
    fpf out "@]"

  let gen_mli self : code =
    Code.mk_pp @@ fun out () ->
    fpf out "@[<v>type %s = private int@," self.name;
    fpf out "@[<v>val %s : %s@," (empty_name self) self.name;
    List.iter
      (fun f ->
         match f.f_def with
         | F_bit ->
           fpf out "@[val get_%s : %s -> bool@]@," f.f_name self.name;
           fpf out "@[val set_%s : bool -> %s -> %s@]@," f.f_name self.name self.name;
         | F_int {width} ->
           fpf out "@[val get_%s : %s -> int@]@,"
             f.f_name self.name;
           fpf out "@,@[(** %d bits integer *)@]@,\
                    @[val set_%s : int -> %s -> %s@]@,"
             width f.f_name self.name self.name;
      )
      (List.rev self.fields);
    fpf out "@]"
end

let emit_chan oc cs =
  let fmt = Fmt.formatter_of_out_channel oc in
  List.iter (fun c -> Fmt.fprintf fmt "@[%a@]@." Code.pp c) cs;
  Fmt.fprintf fmt "@?"

let emit_file file cs =
  CCIO.with_out file (fun oc -> emit_chan oc cs)

let emit_string cs : string =
  Fmt.asprintf "@[<v>%a@]" (Fmt.list ~sep:(Fmt.return "@ ") Code.pp) cs

