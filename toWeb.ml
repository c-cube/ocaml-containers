
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

(** {1 Expose the State of a Program to the Web}

    We export some values (and associated functions for converting them to
    html, and update them) as a FastCGI interface.

    This module depends on CamlGI.
*)

(** {2 Some combinators to build HTML documents} *)

module HTML = struct
  type t = 
    | Str of string  (* content *)
    | List of t list
    | Url of url
    | Img of image
    | Concat of t list
    | H of int * t
    | Link of link
    | Tag of string * t
  and url = {
    url_alt : string option;
    url_url : string;
    url_descr : string;
  }
  and image = {
    img_alt : string option;
    img_url : string;
  }
  and link = {
    link_rel : string;
    link_url : string;
  }

  let str s = Str s

  let bprintf format =
    let buffer = Buffer.create 64 in
    Printf.kbprintf
      (fun fmt -> str (Buffer.contents buffer))
      buffer
      format

  let bprintf format =
    let buffer = Buffer.create 64 in
    Printf.ksprintf
      str
      format

  let list l = List l

  let url ?alt ~url ~descr = Url {
    url_alt = alt;
    url_url = url;
    url_descr = descr;
  }

  let img ?alt url = Img {
    img_alt = alt;
    img_url = url;
  }

  let append a b = Concat [a; b]

  let concat l = Concat l

  let h1 x = H (1, x)

  let h2 x = H (2, x)

  let h3 x = H (3, x)

  let h n x = H (n, x)

  let link ~rel ~url = Link {
    link_rel = rel;
    link_url = url;
  }

  let head x = Tag ("head", x)

  let body x = Tag ("body", x)

  let html x = Tag ("html", x)

  (* XXX UGLY: I copied code from CamlGI because it doesn't provide a way
     to encode strings with HTML conventions. Bad bad bad. *)
  module Encode = struct
    (* Use a table lookup for speed. *)
    let char_of_hex =
      let hex = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
                   'A'; 'B'; 'C'; 'D'; 'E'; 'F' |] in
      fun i -> Array.unsafe_get hex i


    let encode_wrt is_special s0 =
      let len = String.length s0 in
      let encoded_length = ref len in
      for i = 0 to len - 1 do
        if is_special(String.unsafe_get s0 i) then
          encoded_length := !encoded_length + 2
      done;
      let s = String.create !encoded_length in
      let rec do_enc i0 i = (* copy the encoded string in s *)
        if i0 < len then begin
          let s0i0 = String.unsafe_get s0 i0 in
          if is_special s0i0 then begin
            let c = Char.code s0i0 in
            let i1 = succ i in
            let i2 = succ i1 in
            String.unsafe_set s i '%';
            String.unsafe_set s i1 (char_of_hex (c lsr 4));
            String.unsafe_set s i2 (char_of_hex (c land 0x0F));
            do_enc (succ i0) (succ i2)
          end
          else if s0i0 = ' ' then begin
            String.unsafe_set s i '+';
            do_enc (succ i0) (succ i)
          end
          else begin
            String.unsafe_set s i s0i0;
            do_enc (succ i0) (succ i)
          end
        end in
      do_enc 0 0;
      s


    (* Unreserved characters consist of all alphanumeric chars and the
       following limited set of punctuation marks and symbols: '-' | '_' |
       '.' | '!' | '~' | '*' | '\'' | '(' | ')'.  According to RFC 2396,
       they should not be escaped unless the context requires it. *)
    let special_rfc2396 = function
      | ';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' (* Reserved *)
      | '\000' .. '\031' | '\127' .. '\255' (* Control chars and non-ASCII *)
      | '<' | '>' | '#' | '%' | '"'         (* delimiters *)
      | '{' | '}' | '|' | '\\' | '^' | '[' | ']' | '`' (* unwise *)
          -> true
      | _ -> false
    (* ' ' must also be encoded but its encoding '+' takes a single char. *)

    let encode = encode_wrt special_rfc2396
  end

  (* real rendering is always into a buffer (for now) *)
  let rec to_buf buf x =
    match x with
    | Str s -> Buffer.add_string buf (Encode.encode s)
    | List l ->
      Buffer.add_string buf "<ul>";
      List.iter
        (fun y -> Printf.bprintf buf "<li>%a</li>" to_buf y)
        l;
      Buffer.add_string buf "</ul>"
    | Url url ->
      begin match url.url_alt with
      | None ->
        Printf.bprintf buf "<a href=\"%s\">%s</a>" url.url_url
          (Encode.encode url.url_descr)
      | Some alt ->
        Printf.bprintf buf "<a href=\"%s\" alt=\"%s\">%s</a>"
          url.url_url (Encode.encode alt) (Encode.encode url.url_descr)
      end
    | Img i -> failwith "img: not implemented"
    | Concat l -> List.iter (to_buf buf) l
    | H (n, y) ->
      Printf.bprintf buf "<h%i> %a </h%i>" n to_buf y n
    | Link _ -> failwith "link: not implemented"
    | Tag (str, y) -> Printf.bprintf buf "<%s> %a </%s>" str to_buf y str

  let render x = 
    let buf = Buffer.create 256 in
    to_buf buf x;
    Buffer.contents buf

  let to_chan oc x =
    let buf = Buffer.create 256 in
    to_buf buf x;
    Buffer.output_buffer oc buf
end

(** {2 Stateful Object on the Web} *)

module State = struct
  type 'a t = {
    mutable content : 'a;
    mutable callbacks : ('a -> unit) list;
    id : ('a -> string) option;
    export : 'a -> HTML.t;
    update : (string * string) list -> 'a -> 'a;
  } (** A value that can be exposed to the Web.
      The [export] function is used to print the current state of
      the object into HTML (when requested).
      The [update] optional function can be used to update
      the value, given a query with parameters. *)

  type wrap = Wrap : 'a t -> wrap
    (** Hides the type parameter in a GADT. *)

  let create ?(update=fun _ x -> x) ?id ~export content = {
    content;
    export;
    id;
    callbacks = [];
    update;
  }

  let on_change st f =
    st.callbacks <- f :: st.callbacks

  let handle_request st req =
    let cgi = new CamlGI.Cgi.cgi req in
    (* update value? *)
    try
      let x = st.content in
      let params = cgi#params in
      (* update [x] using the parameters? *)
      let y = st.update params x in
      let changed = match st.id with
        | None -> x != y
        | Some id -> id x <> id y
      in
      (* notify callbacks that we have a new object *)
      if changed then
        List.iter (fun f -> f y) st.callbacks;
      (* now print [y] *)
      (* TODO: add a head, declaration, etc. *)
      let html = st.export y in
      let final_output = HTML.render html in
      (* render output *)
      let template = object
        method output f = f final_output
      end in
      cgi#template template
    with e ->
      let msg = Printf.sprintf "error: %s" (Printexc.to_string e) in
      cgi#log msg
end

(** {2 Routing} *)

module Router = struct
  type t = {
    mutable default : State.wrap;
    log : out_channel option;
    tbl : (string, State.wrap) Hashtbl.t;
  }

  let __default =
    State.Wrap (State.create ~export:HTML.str "<no default handler>")

  let _log router fmt = match router.log with
    | None ->
      Printf.ifprintf stdout fmt
    | Some oc ->
      Printf.kfprintf
        (fun oc ->
          output_char oc '\n';
          flush oc)
        oc
        fmt

  let create ?(default=__default) ?log () =
    let router = {
      default;
      log;
      tbl = Hashtbl.create 15;
    } in
    _log router "new router created";
    router

  let default router default =
    router.default <- default

  let unregister router name =
    Hashtbl.remove router.tbl name

  let register ?(weak=false) router name state =
    if Hashtbl.mem router.tbl name
      then failwith "Router: name already registered"
      else begin
        Hashtbl.add router.tbl name state;
        if weak then match state with
          | State.Wrap st ->
            Gc.finalise (fun _ -> unregister router name) st.State.content
      end

  let add_list router l = 
    List.iter
      (fun (name, state) -> register router name state)
      l

  let to_list router =
    Hashtbl.fold
      (fun name state acc -> (name,state) :: acc)
      router.tbl []

  let random_id () =
    CamlGI.Cgi.random_sessionid ()

  let handle_request router req =
    let cgi = new CamlGI.Cgi.cgi req in
    let url = cgi#url () in
    let st =
      try
        let last_part_i = String.rindex url '/' in
        let last_part = String.sub url (last_part_i+1) (String.length url -last_part_i-1) in
        _log router "received request for url /%s" last_part;
        Hashtbl.find router.tbl last_part
      with Not_found ->
        router.default
    in
    match st with
    | State.Wrap st -> State.handle_request st req
end
