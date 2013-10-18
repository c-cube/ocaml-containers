
(** Export the list of files in a directory *)

let dir = "/tmp/"

(* list of files in a dir *)
let lsdir dir =
  let d = Unix.opendir dir in
  let l = ref [] in
  begin try while true do
    l := Unix.readdir d :: !l
  done with End_of_file -> Unix.closedir d
  end;
  !l

let export dir =
  let l = lsdir dir in
  ToWeb.HTML.(concat
    [ h1 (str ("files in "^ dir))
    ; list (List.map str l)
    ])

let state = ToWeb.State.create dir ~export

let _ =
  ToWeb.serve_state ~sockfile:"/tmp/foo.sock" state
