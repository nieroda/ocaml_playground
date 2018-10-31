open Core
open Lwt
open Cohttp
open Cohttp_lwt_unix

(*
corebuild -pkg cohttp-lwt-unix,core client_example.native

*)

(* val get_query_param' : t ‑> string ‑> string list option *)

let no_route_exists =
  Server.respond_string ~status:`Not_implemented ~body:"No Route Exists" ()

let method_not_allowed =
  Server.respond_string ~status:`Method_not_allowed ~body:"Method not allowed" ()

let server_respond str =
  Server.respond_string ~status:`OK ~body:str ()

let get_user_feed id =
  server_respond ("Feed for user id:" ^ id ^ "{}")

let ping_route meth =
  match meth with
  | `GET -> server_respond "PONG"
  | _ -> method_not_allowed

let user_route meth id =
  match meth with
  | `GET -> get_user_feed id
  | _ -> method_not_allowed

let router _conn req body =
  let meth = req |> Request.meth in
  let uri = req |> Request.uri |> Uri.path in
  match uri with
  | "/" -> ping_route (*(req |> Request.uri)*) meth
  | _ as line ->
      match String.rsplit2 line ~on:'/' with
      | Some ("/getUserFeed", id) -> user_route meth id
      | _ -> no_route_exists
;;

let server =
  print_endline "Server running on port 8000";
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback:router ())


let () = ignore (Lwt_main.run server)
