#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;
#load "containers.cma";;
#require "lwt.unix";;
#load "containers_misc.cma";;
#load "containers_lwt.cma";;

let (>>=) = Lwt.(>>=)

module A = Containers_lwt.Lwt_actor

let a = A.spawn
  (fun _ (`Ping sender) ->
    Lwt_io.printl "ping!" >>= fun () ->
    Lwt_unix.sleep 1. >>= fun () ->
    A.send sender `Pong
  )

let b = A.spawn
  (fun self -> function
    | `Pong
    | `Start ->
        Lwt_io.printl "pong!" >>= fun () ->
        Lwt_unix.sleep 1. >>= fun () ->
        A.send a (`Ping self)
  )

let () = Lwt_main.run (
  Lwt_io.printl "start" >>= fun () ->
  A.send b `Start >>= fun () ->
  A.wait_all ()
)

