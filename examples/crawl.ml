
(** Crawl the web to find shortest path between two urls *)

open Batteries

let pool = Future.Pool.create ~timeout:15. ~size:50

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
let g : (page, string, unit) LazyGraph.t =
  let force (url, future) =
    Format.printf "force %s@." url;
    let urls =
      try Future.get future |> List.map (fun url -> (), (url, get_and_parse url))
      with e -> [] in
    let edges = Gen.of_list urls in
    (* need to parse the page to get the urls *)
    LazyGraph.Node ((url, future), url, edges)
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
let path_between from into =
  Format.printf "seek path from %s to %s@." from into;
  let on_explore (url,_) = Format.printf "  explore %s...@." url in
  try
    let cost, path = LazyGraph.dijkstra ~on_explore g
      (from, get_and_parse from) (into, get_and_parse into) in
    Printf.printf "found path (cost %f):\n%a\n" cost pp_path path
  with Not_found ->
    Format.printf "no path could be found@."

let print_limit file start depth =
  Format.printf "print into %s webgraph starting from %s, up to depth %d@."
    file start depth;
  let start = start, get_and_parse start in
  let g' = LazyGraph.limit_depth g depth (Gen.singleton start) in
  let g'' = LazyGraph.map ~vertices:(fun v -> [`Label v]) ~edges:(fun _ -> []) g' in
  let out = Format.formatter_of_out_channel (open_out file) in
  LazyGraph.Dot.pp ~name:"web" g'' out (Gen.singleton start);
  Format.pp_print_flush out ();
  ()

let _ =
  let timer = Future.Timer.create () in
  let rec ping () =
    Format.printf "*** ping! (size of pool: %d)@." (Future.Pool.size pool);
    Future.Timer.schedule_in timer 10. ping
  in ping ()

let print_usage () =
  Format.printf "usage: crawl path url1 url2@.";
  Format.printf "usage: crawl print file url depth@.";
  ()

let _ =
  match Sys.argv with
  | [|_; "print"; file; url; depth|] ->
    print_limit file url (int_of_string depth)
  | [|_; "path"; from; into|] ->
    path_between from into
  | _ ->
    print_usage ()
