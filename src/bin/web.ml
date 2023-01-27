module Server = Cohttp_lwt_unix.Server

let crunch ?content_type ?(max_age = 86400) _ =
  object
    inherit Current_web.Resource.t
    val! can_get = `Viewer

    method! private get (ctx : Current_web.Context.t) =
      let path =
        Cohttp.Request.uri (Current_web.Context.request ctx) |> Uri.path
      in
      match Static.read path with
      | None -> Server.respond_not_found ()
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
          Server.respond_string ~status:`OK ~headers ~body ()
  end

let static_routes custom_css =
  [
    Routes.(
      (s "css" / s custom_css /? nil)
      @--> crunch ~content_type:"text/css" custom_css);
    Routes.((s "img" / str /? nil) @--> crunch);
  ]
