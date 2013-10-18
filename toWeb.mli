
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

module HTML : sig
  type t
    (** A html document. Encoding is assumed to be UTF8 for now. *)

  val str : string -> t
    (** Simple string *)

  val bprintf : ('a, Buffer.t, unit, unit) format4 -> t
    (** Use a buffer printer to render a string. Shortcut for {!str} *)

  val sprintf : ('a, unit, string, unit) format4 -> t
    (** Use a string printer to render into a string. Shortcut for {!str} *)

  val list : t list -> t
    (** Build a list of items *)

  val url : ?alt:string -> url:string -> descr:string -> t
    (** build an URL tag. *)

  val img : ?alt:string -> string -> t
    (** Link to an image *)

  val append : t -> t -> t
    (** Concatenation of two documents *)

  val concat : t list -> t
    (** Concatenation of html documents *)

  val h1 : t -> t

  val h2 : t -> t

  val h3 : t -> t

  val h : int -> t -> t
    (** Title of level parametrized by the integer *)

  val p : t -> t
    (** Paragraph *)

  val div : ?id:string -> ?class_:string -> t -> t
    (** Div tag, to specify a block *)

  val span : ?id:string -> ?class_:string -> t -> t
    (** Non semantic tag, mostly useful for CSS *)

  val link : rel:string -> url:string -> t
    (** Link (for head) *)

  val head : t -> t
    (** Head part of a document *)

  val body : t -> t
    (** Body part of a document *)

  val html : t -> t
    (** The whole document *)

  val render : t -> string
    (** Print into a string *)

  val to_buf : Buffer.t -> t -> unit
    (** Print in the buffer *)

  val to_chan : out_channel -> t -> unit
    (** Print on the channel *)
end

(** {2 Stateful Object on the Web} *)

(** This module defines how to bundle an OCaml value (possibly
    stateful) with functions that export it to HTML,
    and possibly update it from a CGI request.
*)

module State : sig
  type 'a t
    (** A value that can be exposed to the web. *)

  type wrap = Wrap : 'a t -> wrap
    (** Hides the type parameter in a GADT. Useful for {!Router}. *)

  val create : ?update:((string*string) list -> 'a -> 'a) ->
               ?id:('a -> string) ->
               export:('a -> HTML.t) ->
               'a ->
               'a t
  (** Create a value that can be exposed to the Web.
      @param export function used to print the current state of
        the object into HTML (when requested).
      @param update optional function that can be used to update
        the value, given a query with parameters.
      @param id optional function that maps a value to a (unique)
        string. Can be used to obtain a unique URL for this value. *)

  val on_change : 'a t -> ('a -> unit) -> unit
    (** Register a callback that will be called everytime the value
        is updated. Physical equality is used to determine whether
        the value changed if no [id] function was provided;
        otherwise, [id] is used to check whether the old and the new
        strings are equal. *)

  val handle_request : 'a t -> CamlGI.Cgi.Request.t -> unit
    (** Handle the incoming request. It replies to the request by
        possibly updating the local state, and 
        object. *)
end


(** {2 Routing} *)

module Router : sig
  type t
    (** An URL router. It dispatches incoming requests to registered
        {!State.t} values depending on the request's URL. *)

  val create : ?default:State.wrap -> ?log:out_channel -> unit -> t
    (** New router.
        @param log a channel on which to log events (incoming requests)
        @param default a default object to expose, for incorrect routes
        *)

  val default : t -> State.wrap -> unit
    (** Set the default handler, for incorrect routes (for which no
        object is registered) or for routing the root url *)

  val register : ?weak:bool -> t -> string -> State.wrap -> unit
    (** Register a state object (see {!State}) under a given path.
        Right now routing only dispatches at one level, there is no
        tree-like structure, only a flat "directory" of objects.

        @param weak (default false) if true, the object will unregister itself
        when it's garbage collected. Only works if the type of the wrapped
        object is heap allocated.
        
        @raise Failure if the name is already taken
        @raise Invalid_argument if [weak] is true and no finalizer can be
        registered. *)

  val unregister : t -> string -> unit
    (** Remove a stateful value *)

  val add_list : t -> (string * State.wrap) list -> unit
    (** Register several handlers.
        @raise Failure if it meets an already registered handler *)

  val to_list : t -> (string * State.wrap) list
    (** Currently registered objects *)

  val random_id : unit -> string
    (** Fresh, random ID that can be used for registering temporary objects *)

  val handle_request : t -> CamlGI.Cgi.Request.t -> unit
    (** Handle the incoming request, by routing to an appropriate
        object. *)
end

(** {2 Main interface} *)

val serve_state : ?sockfile:string -> ?sockaddr:Unix.sockaddr ->
                  'a State.t -> unit
  (** Serve incoming requests using a single object.
      @param sockfile the unix file to use as a socket *)

val serve_router : ?sockfile:string -> ?sockaddr:Unix.sockaddr ->
                   Router.t -> unit
  (** Shortcut. It calls {!CamlGI.Cgi.register_script} with a callback
      that forwards requests to the given Router.
      @param sockfile the unix file to use as a socket *)
