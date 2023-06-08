open Tyxml.Html
open Astring
open Lwt.Infix
open Current_web
module Server = Cohttp_lwt_unix.Server
let sep = "@@LOG@@"

let max_log_chunk_size = 102400L  (* 100K at a time *)

let read ~start path =
  let ch = open_in_bin (Fpath.to_string path) in
  Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
  let len = LargeFile.in_channel_length ch in
  let (+) = Int64.add in
  let (-) = Int64.sub in
  let start = if start < 0L then len + start else start in
  let start = if start < 0L then 0L else if start > len then len else start in
  LargeFile.seek_in ch start;
  let len = min max_log_chunk_size (len - start) in
  really_input_string ch (Int64.to_int len), start + len


module Set = Set.Make(String)

let input_jobs ~id (x : _ Current.t) =
  let rec loop ?(add=false) acc (ty : Current.Node.metadata_ty) =
  match ty with
    | Constant _ -> acc
    | Map_input { source = Term ty; _ } -> loop ~add acc ty.ty
    | Opt_input { source = Term ty } -> loop ~add acc ty.ty
    | State { source = Term ty; _ } -> loop ~add acc ty.ty
    | Catch { source = Term ty; _ } -> loop ~add acc ty.ty
    | Map (Term ty) -> loop ~add acc ty.ty
    | Bind_in (Term ty, _) -> loop ~add acc ty.ty
    | Bind_out incr -> 
      let Term ty = Current_incr.observe incr in
      loop ~add acc ty.ty
    | Primitive { x = Term ty; meta; _ } ->
      let meta = Current_incr.observe meta in
      let job = Option.bind meta (fun (j : Current.Metadata.t) -> j.job_id) in
      let found = match job with Some j -> String.equal id j | _ -> false in
      if found then begin 
        loop ~add:true acc ty.ty
      end
      else begin 
        if add then 
          let s = Option.map (fun s -> Set.add s acc) job in
          let s = Option.value ~default:acc s in
          loop s ty.ty else loop acc ty.ty 
      end
    | Pair (Term t1, Term t2) ->
      (* Hmmm... *)
      let s2 = loop ~add acc t2.ty in
      loop ~add (Set.union s2 acc) t1.ty
    | Gate_on { value = Term ty; _ } ->
      loop ~add acc ty.ty
    | List_map { items = Term ty; _ } ->
      loop ~add acc ty.ty
    | Option_map { item = Term ty; _ } ->
      loop ~add acc ty.ty
    | Collapse { input = Term ty; _ } ->
      loop ~add acc ty.ty
  in
  loop Set.empty x.ty |> Set.to_seq |> List.of_seq

let link_obuilder_id s =
  match Astring.String.cuts ~sep:"\"" s with
  | "" :: id :: rest ->
    Fmt.str "\"<a href='/terminal/%s'>%s</a>\"%s" id id (String.concat ~sep:"\"" rest)
  | _ -> s

let render ctx ~actions ~job_id ~log:path jobs =
  let _ansi = Ansi.create () in
  let action op = a_action (Fmt.str "/job/%s/%s" job_id op) in
  let csrf = Current_web.Context.csrf ctx in
  let rebuild_button =
    if actions#rebuild = None then []
    else
      [form ~a:[action "rebuild"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Rebuild"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf] () ]
      ]
  in
  let cancel_button =
    match Current.Job.lookup_running job_id with
    | Some job when Current.Job.cancelled_state job = Ok () ->
      [form ~a:[action "cancel"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Cancel"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf] () ]
      ]
    | _ -> []
  in
  let start_button =
    match Current.Job.lookup_running job_id with
    | Some job when Current.Job.is_waiting_for_confirmation job ->
      [form ~a:[action "start"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Start now"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf] () ]
      ]
    | _ -> []
  in
  let job_item ~label id =
    let label = txt label in
    if id = job_id then b [label]
    else a ~a:[a_href (Fmt.str "/job/%s" id)] [label]
  in
  let history =
    match Current_cache.Db.history ~limit:10 ~job_id with
    | None, [] -> []
    | current, past ->
      let items = past |> List.map (fun entry ->
          let label = Int64.to_string entry.Current_cache.Db.build in
          let item = job_item ~label entry.job_id in
          li [item]
        ) in
      let items =
        match current with
        | None -> items
        | Some id -> li [job_item id ~label:"(building)"] :: items
      in
      [div ~a:[a_class ["build-history"]]
         [txt "Build: ";
          ol items]
      ]
  in
  let inputs = [
    p [ txt "Transitive Input Jobs:" ];
    ul (List.map (fun s -> li [ a ~a:[ a_href ("/alt/job/" ^ s) ] [ txt s ] ]) jobs)
  ] in
  let line_numbers_js = [script ~a:[a_src (Xml.uri_of_string "/js/line-numbers.js")] (txt "");]
  in
  let tmpl =
    Current_web.Context.template ctx (
      line_numbers_js @
      history @
      rebuild_button @
      cancel_button @
      start_button @
      inputs @
      [pre [txt sep]]
    )
  in
  match String.cut ~sep tmpl with
  | None -> assert false
  | Some (pre, post) ->
    let i = ref `Pre in
    let stream =
      Lwt_stream.from (fun () ->
          match !i with
          | `Pre -> i := `Log 0L; Lwt.return_some pre
          | `Log start ->
            let rec aux () =
              begin match read ~start path with
                | "", _ ->
                  begin match Current.Job.lookup_running job_id with
                    | None -> i := `Done; Lwt.return_some post
                    | Some job -> Current.Job.wait_for_log_data job >>= aux
                  end
                | (data, next) ->
                  i := `Log next;
                  let data = Astring.String.cuts ~sep:"---> saved as " data in
                  let rec loop acc = function
                    | pre :: cache :: rest ->
                      let c = pre ^ "---> saved as " ^ (link_obuilder_id cache) in
                      loop (c :: acc) rest
                    | [] -> List.rev acc |> String.concat ~sep:""
                    | [ cache ] -> 
                      let c = "" ^ "---> saved as " ^ (link_obuilder_id cache) in
                      loop (c :: acc) []
                  in
                  let data = loop [] data in
                  (* let data = Ansi.process ansi data in *)
                  Lwt.return_some data
              end
            in aux ()
          | `Done -> Lwt.return_none
        )
    in
    Cohttp_lwt.Body.of_stream stream

type actions = <
  rebuild : (unit -> string) option;
>

let lookup_actions ~engine job_id =
  let state = Current.Engine.state engine in
  let jobs = state.Current.Engine.jobs in
  match Current.Job.Map.find_opt job_id jobs with
  | Some a -> (a :> actions)
  | None ->
    object
      method rebuild = None
    end

let job ~engine ~job_id = object
  inherit Current_web.Resource.t

  val! can_get = `Viewer

  method! private get ctx =
    let actions = lookup_actions ~engine job_id in
    match Current.Job.log_path job_id with
    | Error (`Msg msg) -> Context.respond_error ctx `Bad_request msg
    | Ok path ->
      let lst = input_jobs ~id:job_id (Current.Engine.pipeline engine) in
      let body = render ctx ~actions ~job_id ~log:path lst in
      let headers =
        (* Otherwise, an nginx reverse proxy will wait for the whole log before sending anything. *)
        Cohttp.Header.init_with "X-Accel-Buffering" "no"
      in
      Server.respond ~status:`OK ~headers ~body () >|= fun r ->
      `Response r
end

let rebuild ~engine ~job_id = object
  inherit Resource.t

  val! can_post = `Builder

  method! private post ctx  _body =
    let actions = lookup_actions ~engine job_id in
    match actions#rebuild with
    | None -> Context.respond_error ctx `Bad_request "Job does not support rebuild"
    | Some rebuild ->
      let new_id = rebuild () in
      Server.respond_redirect ~uri:(Uri.of_string ("/job/" ^ new_id)) () >|= fun r ->
      `Response r
end

let cancel ~job_id = object
  inherit Resource.t

  val! can_post = `Builder

  method! private post ctx _body =
    match Current.Job.lookup_running job_id with
    | None -> Context.respond_error ctx `Bad_request "Job does not support cancel (already finished?)"
    | Some job ->
      Current.Job.cancel job "Cancelled by user";
      Context.respond_redirect ctx (Uri.of_string "/")
end

let start ~job_id = object
  inherit Resource.t

  val! can_post = `Admin

  method! private post ctx _body =
    match Current.Job.lookup_running job_id with
    | None -> Context.respond_error ctx `Bad_request "Job is not awaiting confirmation"
    | Some j ->
      Current.Job.approve_early_start j;
      let id = Current.Job.id j in
      Context.respond_redirect ctx (Uri.of_string ("/job/" ^ id))
end

let id ~date ~log = Fmt.str "%s/%s" date log

let routes ~engine = Routes.[
    s "alt" / s "job" / str / str /? nil @--> (fun date log -> job ~engine ~job_id:(id ~date ~log));
    s "alt" / s "job" / str / str / s "rebuild" /? nil @--> (fun date log -> rebuild ~engine ~job_id:(id ~date ~log));
    s "alt" / s "job" / str / str / s "cancel" /? nil @--> (fun date log -> cancel ~job_id:(id ~date ~log));
    s "alt" / s "job" / str / str / s "start" /? nil @--> (fun date log -> start ~job_id:(id ~date ~log));
  ]
