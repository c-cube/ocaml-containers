
(** Crawl the web to find shortest path between two urls *)

open Batteries

let pool = Future.Pool.create ~timeout:15. ~size:15

let split_lines s = String.nsplit s ~by:"\n"

let get_and_parse url =
  let cmd = Format.sprintf "wget -q '%s' -O - | grep -o 'http\\(s\\)\\?://[^ \"]\\+'" url in
  let content = Future.spawn_process ?stdin:None ~pool ~cmd in
  content
    |> Future.map (fun (_, stdout, _) -> stdout)
    |> Future.map split_lines
    |> Batteries.tap (fun lines ->
      Future.on_success lines (fun lines -> Format.printf "downloaded %s (%d urls)@." url (List.length lines)))

type page = string * (string list Future.t)

(** The web graph; its vertices are annotated by futures of the content *)
let g : (page, unit, unit) LazyGraph.t =
  let force (url, future) =
    Format.printf "force %s@." url;
    let urls =
      try Future.get future |> List.map (fun url -> (), (url, get_and_parse url))
      with e -> [] in
    let edges = Gen.of_list urls in
    (* need to parse the page to get the urls *)
    LazyGraph.Node ((url, future), (), edges)
  in LazyGraph.make
    ~eq:(fun (url1,_) (url2,_) -> url1 = url2)
    ~hash:(fun (url,_) -> Hashtbl.hash url)
    force 

let pp_path fmt path =
  List.print ~sep:"\n"
    (fun fmt ((u1,_), (), (u2,_)) ->
      String.print fmt u1; String.print fmt " -> "; String.print fmt u2)
    fmt path

(* seek a path from the first url to the second *)
let main from into =
  Format.printf "seek path from %s to %s@." from into;
  let on_explore (url,_) = Format.printf "  explore %s...@." url in
  try
    let cost, path = LazyGraph.dijkstra ~on_explore g
      (from, get_and_parse from) (into, get_and_parse into) in
    Printf.printf "found path (cost %f):\n%a\n" cost pp_path path
  with Not_found ->
    Format.printf "no path could be found@."

let _ =
  if Array.length Sys.argv < 3
    then Format.printf "usage: crawl url1 url2"
    else main Sys.argv.(1) Sys.argv.(2)
