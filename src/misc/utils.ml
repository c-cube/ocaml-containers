
(** {1 Some very basic utils} *)

(* val sprintf : ('a, Format.formatter, unit, string) format4 -> 'a *)

let sprintf format =
  let buffer = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.kfprintf
    (begin fun fmt ->
    Format.pp_print_flush fmt ();
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    s
    end)
  fmt
  format
