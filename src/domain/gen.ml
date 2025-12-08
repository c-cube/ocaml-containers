let domain_4 =
  {|
let is_main_domain () = true
let cpu_relax = ignore
let relax_loop : int -> unit = ignore
  |}

let domain_5 =
  {|
let is_main_domain = Domain.is_main_domain
let cpu_relax = Domain.cpu_relax
let relax_loop i =
  for _j = 1 to i do cpu_relax () done
|}

let write_file file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

let () =
  let version = Scanf.sscanf Sys.ocaml_version "%d.%d.%s" (fun x y _ -> x, y) in
  write_file "containers_domain.ml"
    (if version >= (5, 0) then
       domain_5
     else
       domain_4);
  ()
