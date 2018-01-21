let () =
  let major, minor =
    Scanf.sscanf Sys.ocaml_version "%u.%u"
      (fun major minor -> major, minor)
  in
  let after_4_3 = (major, minor) >= (4, 3) in
  let flags_file = open_out "flambda.flags" in
  if after_4_3 then (
    output_string flags_file "(-O3 -unbox-closures -unbox-closures-factor 20 -color always)\n";
  ) else (
    output_string flags_file "()\n";
  );
  close_out flags_file
