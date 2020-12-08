let () =
  Crowbar.add_test ~name:"ccsexp_parse_string_does_not_crash" [ Crowbar.bytes ]
    (fun s -> CCSexp.parse_string s |> ignore)
