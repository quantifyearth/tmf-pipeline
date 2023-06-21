open Lwt.Infix
open Evaluations
module Server = Cohttp_lwt_unix.Server

external tty_output_buffer_size : Unix.file_descr -> int
  = "caml_tmf_output_buffer_count"

let ( >>!= ) v f =
  v >>= function
  | Ok v -> f v
  | Error (`Msg m) -> Lwt.fail (Failure m)
  | Error `Cancelled -> Lwt.fail (Failure "Cancelled")

let crunch ?content_type ?(max_age = 86400) _ =
  object
    inherit Current_web.Resource.t
    val! can_get = `Viewer

    method! private get (ctx : Current_web.Context.t) =
      let path =
        Cohttp.Request.uri (Current_web.Context.request ctx) |> Uri.path
      in
      match Static.read path with
      | None -> Server.respond_not_found () >|= fun r -> `Response r
      | Some body ->
          let content_type =
            Option.value ~default:(Magic_mime.lookup path) content_type
          in
          let headers =
            Cohttp.Header.of_list
              [
                ("Content-Type", content_type);
                ("Cache-Control", Printf.sprintf "public, max-age=%d;" max_age);
              ]
          in
          Server.respond_string ~status:`OK ~headers ~body () >|= fun r ->
          `Response r
  end

let terminal _id =
  object
    inherit Current_web.Resource.t
    val! can_get = `Viewer

    method! private get (_ctx : Current_web.Context.t) =
      let body =
        {|
      <!DOCTYPE html>
      <html lang="en">
      <head>
          <meta charset="UTF-8">
          <meta http-equiv="X-UA-Compatible" content="IE=edge">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <title>Document</title>
          <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/xterm@4.19.0/css/xterm.css" />
          <script src="https://cdn.jsdelivr.net/npm/xterm@4.19.0/lib/xterm.js"></script>
          <style>
            html,body {
              margin: 0px;
            }
            #terminal {
              height: 100vh;
              background: black;
              padding-top: 10px;
              padding-left: 20px;
            }
          </style>
      </head>
      <body>
          <div id="terminal"></div>
          <script src="/js/index.js"></script>
      </body>
      </html>
      |}
      in
      let headers =
        Cohttp.Header.of_list
          [
            ("Content-Type", "text/html");
            ("Content-Length", string_of_int (String.length body));
          ]
      in
      Server.respond_string ~status:`OK ~headers ~body () >|= fun r ->
      `Response r
  end

let read_all fd =
  let bytes = Bytes.create 1024 in
  Lwt_unix.read fd bytes 0 1024 >|= fun i -> Bytes.sub bytes 0 i

let connect (Builder ((module B), b) : Current_obuilder.builder) id =
  object
    inherit Current_web.Resource.t
    val! can_get = `Viewer

    method! private get (ctx : Current_web.Context.t) =
      let open Lwt.Infix in
      let open Websocket in
      Logs.info (fun f -> f "Listening for RUNC%!");
      let req = Current_web.Context.request ctx in
      let fd_passer =
        Lwt_unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0
      in
      Lwt_unix.setsockopt fd_passer Unix.SO_REUSEADDR true;
      let unix_sock =
        Filename.concat "/tmp/"
          ("runc-" ^ (string_of_int @@ Random.int 999999) ^ ".sock")
      in
      Lwt_unix.bind fd_passer (Unix.ADDR_UNIX unix_sock) >>= fun () ->
      Lwt_unix.listen fd_passer 5;
      let established, _exited = B.shell ~unix_sock b id in
      established >>= fun () ->
      (* Do IOVecs need to be non-empty? *)
      let destination_buffer = Bytes.create 4096 in
      let io_vectors = Lwt_unix.IO_vectors.create () in
      Lwt_unix.IO_vectors.append_bytes io_vectors destination_buffer 0 4096;
      Lwt_unix.accept fd_passer >>= fun (socket, _) ->
      Lwt_unix.recv_msg ~socket ~io_vectors >>= fun (_, fds) ->
      let console_fd_unix = List.hd fds in
      let console_fd = Lwt_unix.of_unix_file_descr console_fd_unix in
      Websocket_cohttp_lwt.upgrade_connection req (fun { opcode; content; _ } ->
          match opcode with
          | Frame.Opcode.Close -> Logs.app (fun m -> m "[RECV] CLOSE")
          | _ ->
              let i =
                Unix.write console_fd_unix (Bytes.of_string content) 0
                  (String.length content)
              in
              Logs.info (fun f -> f "Wrote %i bytes to pty" i))
      >>= fun (resp, frames_out_fn) ->
      (* send a message to the client every second *)
      let rec go () =
        read_all console_fd >>= fun msg ->
        let msg = Bytes.to_string msg in
        Lwt.wrap1 frames_out_fn @@ Some (Frame.create ~content:msg ()) >>= go
      in
      Lwt.async go;
      Lwt.return resp
  end

module HMap = Hashtbl.Make (String)

let static_routes ~engine ~store builder custom_css =
  [
    Routes.(
      (s "css" / s custom_css /? nil)
      @--> crunch ~content_type:"text/css" custom_css);
    Routes.(
      (s "js" / s "index.js" /? nil)
      @--> crunch ~content_type:"text/javascript" "js/index.js");
    Routes.((s "img" / str /? nil) @--> crunch);
    Routes.((s "terminal" / str /? nil) @--> terminal);
    Routes.((s "terminal" / str / s "connect" /? nil) @--> connect builder);
  ]
  @ Web_job.routes ~store ~engine
